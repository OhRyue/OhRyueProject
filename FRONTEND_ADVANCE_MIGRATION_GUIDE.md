# 프론트엔드 전달사항: 세션 단계 전이(Advance) API 변경

## 📋 개요

세션 단계 전이 로직을 중앙화하여 **모든 단계 전이는 `advance` API를 통해서만** 수행되도록 변경되었습니다. 이로 인해 프론트엔드에서 단계 상태를 직접 변경하는 로직을 제거하고, `advance` API 호출로 대체해야 합니다.

---

## 🔄 주요 변경사항

### 1. 단계 전이 방식 변경

**이전 방식 (더 이상 사용 불가):**
```typescript
// ❌ 이제 작동하지 않음
// submitMini() 호출 시 자동으로 MINI → MCQ 전이
await submitMini(learningSessionId, answers);
// 내부적으로 자동으로 상태 변경됨
```

**새로운 방식 (필수):**
```typescript
// ✅ 올바른 방식
// 1. 문제 제출
await submitMini(learningSessionId, answers);

// 2. 모든 문제를 풀었을 때만 advance 호출
if (allQuestionsAnswered) {
  const result = await advance({
    sessionId: learningSessionId,
    step: "MINI",
    score: calculatedScore,
    detailsJson: JSON.stringify(metadata)
  });
  // result.movedTo에 다음 단계가 반환됨
}
```

### 2. 단계 순서 수정

**필기(WRITTEN) 모드 단계 순서:**
```
CONCEPT → MINI → MCQ → REVIEW_WRONG → SUMMARY
```

**실기(PRACTICAL) 모드 단계 순서:**
```
CONCEPT → MINI → PRACTICAL → REVIEW_WRONG → SUMMARY
```

### 3. 오답이 없을 때 자동 건너뛰기

**이전:** 프론트엔드에서 오답 여부를 확인하고 조건부로 REVIEW_WRONG을 건너뛰어야 했음

**현재:** 백엔드에서 자동으로 처리
- 오답이 없으면 `advance` 호출 시 REVIEW_WRONG을 건너뛰고 SUMMARY로 이동
- 프론트엔드는 `advance` 응답의 `movedTo` 필드를 따라가면 됨

---

## 💾 세션 ID 관리

### learningSessionId 저장 방법

**중요:** `POST /api/study/session/start`를 호출하면 반환되는 `sessionId`(learningSessionId)를 **반드시 저장**해야 합니다.

#### 1. 세션 시작

```typescript
// 세션 시작
const startResponse = await fetch('/api/study/session/start', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    topicId: 1,
    mode: "WRITTEN",  // 또는 "PRACTICAL"
    resume: false
  })
});

const { sessionId } = await startResponse.json();
// sessionId === learningSessionId

// ⚠️ 반드시 저장해야 함!
localStorage.setItem('learningSessionId', sessionId.toString());
// 또는 상태 관리 라이브러리 사용
```

#### 2. 저장 방법 선택

**옵션 1: localStorage (권장)**
```typescript
// 저장
localStorage.setItem('learningSessionId', sessionId.toString());

// 조회
const learningSessionId = localStorage.getItem('learningSessionId');

// 삭제 (세션 완료 시)
localStorage.removeItem('learningSessionId');
```

**옵션 2: sessionStorage**
```typescript
// 브라우저 탭이 닫히면 자동 삭제
sessionStorage.setItem('learningSessionId', sessionId.toString());
```

**옵션 3: 상태 관리 (Redux, Zustand 등)**
```typescript
// Redux 예시
dispatch(setLearningSessionId(sessionId));

// Zustand 예시
useLearningStore.setState({ learningSessionId: sessionId });
```

**옵션 4: React Context / Vue Provide**
```typescript
// React Context 예시
const { learningSessionId, setLearningSessionId } = useLearningContext();
setLearningSessionId(sessionId);
```

#### 3. 세션 복원 (페이지 새로고침 대응)

```typescript
// 컴포넌트 마운트 시 저장된 세션 ID 확인
useEffect(() => {
  const savedSessionId = localStorage.getItem('learningSessionId');
  
  if (savedSessionId) {
    // 세션 상태 확인
    fetch(`/api/study/session/${savedSessionId}`)
      .then(res => res.json())
      .then(session => {
        if (session.status === "IN_PROGRESS") {
          // 진행 중인 세션이 있으면 이어서 진행
          setLearningSessionId(Number(savedSessionId));
        } else {
          // 완료된 세션이면 새로 시작
          localStorage.removeItem('learningSessionId');
        }
      });
  }
}, []);
```

#### 4. 세션 완료 시 정리

```typescript
// SUMMARY 단계 완료 후
const result = await advance({
  sessionId: learningSessionId,
  step: "SUMMARY",
  score: null,
  detailsJson: null
});

if (result.movedTo === "END") {
  // 세션 완료 → 저장된 ID 삭제
  localStorage.removeItem('learningSessionId');
}
```

---

## 📡 API 사용 가이드

### Advance API

**엔드포인트:**
```http
POST /api/study/session/advance
Content-Type: application/json
```

**요청:**
```typescript
interface AdvanceRequest {
  sessionId: number;      // LearningSession ID (⚠️ StudySession ID 아님!)
  step: string;           // 현재 완료할 단계 코드
  score?: number;         // 점수 (선택, null 가능)
  detailsJson?: string;   // 메타데이터 JSON 문자열 (선택)
}
```

**응답:**
```typescript
interface AdvanceResponse {
  sessionId: number;
  status: string;         // "IN_PROGRESS" | "DONE"
  movedTo: string;        // 다음 단계 코드 또는 "END"
}
```

**예시:**
```typescript
// MINI 단계 완료
const response = await fetch('/api/study/session/advance', {
  method: 'POST',
  headers: { 'Content-Type': 'application/json' },
  body: JSON.stringify({
    sessionId: 123,
    step: "MINI",
    score: 75,  // 4문제 중 3문제 맞춤
    detailsJson: JSON.stringify({
      total: 4,
      correct: 3,
      wrongQuestionIds: [5]
    })
  })
});

const result = await response.json();
// result.movedTo === "MCQ" (다음 단계)
```

---

## 🔧 단계별 마이그레이션 가이드

### 1. CONCEPT 단계

**이전:**
```typescript
// completeConcept() 호출 시 자동으로 MINI로 전이
await completeConcept(learningSessionId);
```

**변경 후:**
```typescript
// CONCEPT 화면을 본 후 advance 호출
await advance({
  sessionId: learningSessionId,
  step: "CONCEPT",
  score: null,
  detailsJson: null
});
// 응답: movedTo === "MINI"
```

### 2. MINI 단계

#### 2-1. 여러 문제 한 번에 제출 (submitMini)

**이전:**
```typescript
// submitMini() 호출 시 자동으로 상태 변경
const result = await submitMini(learningSessionId, {
  topicId: 1,
  answers: [...]
});
// 내부적으로 MINI → MCQ 전이
```

**변경 후:**
```typescript
// 1. 문제 제출
const submitResult = await submitMini(learningSessionId, {
  topicId: 1,
  answers: [...]
});

// 2. 모든 문제를 풀었을 때만 advance 호출
if (submitResult.status === "COMPLETE") {
  const advanceResult = await advance({
    sessionId: learningSessionId,
    step: "MINI",
    score: submitResult.calculatedScore,
    detailsJson: JSON.stringify(submitResult.metadata)
  });
  // advanceResult.movedTo === "MCQ"
}
```

#### 2-2. 한 문제씩 채점 (gradeOneMini) ⚠️ 중요

**현재 사용 중인 방식:**
```typescript
// ✅ 효율적인 방법: 로컬에서 카운트 관리, 마지막 문제에서만 세션 조회
const questions = [...]; // 4문제
let answeredCount = 0;

for (let i = 0; i < questions.length; i++) {
  const question = questions[i];
  const result = await gradeOneMini(learningSessionId, {
    topicId: 1,
    questionId: question.id,
    answer: userAnswer
  });
  
  answeredCount++;
  const isLastQuestion = (i === questions.length - 1);
  
  // 마지막 문제(4번째)에서만 세션 조회 및 advance 호출
  if (isLastQuestion) {
    const session = await getSession(learningSessionId);
    const miniStep = session.steps.find(s => s.step === "MINI");
    const metadata = JSON.parse(miniStep.detailsJson || "{}");
    
    // advance 호출
    const advanceResult = await advance({
      sessionId: learningSessionId,
      step: "MINI",
      score: metadata.scorePct || 0,
      detailsJson: miniStep.detailsJson
    });
    // advanceResult.movedTo === "MCQ"
  }
}
```

**❌ 비효율적인 방법 (매번 세션 조회):**
```typescript
// 매번 세션을 조회하는 것은 불필요한 API 호출
for (const question of questions) {
  await gradeOneMini(learningSessionId, { ... });
  
  // ❌ 매번 조회 - 불필요!
  const session = await getSession(learningSessionId);
  // ...
}
```

### 3. MCQ 단계

#### 3-1. 여러 문제 한 번에 제출 (submitMcq)

**이전:**
```typescript
const result = await submitMcq(learningSessionId, {
  topicId: 1,
  answers: [...]
});
// 내부적으로 MCQ → REVIEW_WRONG 또는 SUMMARY 전이
```

**변경 후:**
```typescript
// 1. 문제 제출
const submitResult = await submitMcq(learningSessionId, {
  topicId: 1,
  answers: [...]
});

// 2. 모든 문제를 풀었을 때만 advance 호출
if (submitResult.status === "COMPLETE") {
  const advanceResult = await advance({
    sessionId: learningSessionId,
    step: "MCQ",
    score: submitResult.calculatedScore,
    detailsJson: JSON.stringify(submitResult.metadata)
  });
  
  // 오답이 없으면 자동으로 SUMMARY로 이동
  // 오답이 있으면 REVIEW_WRONG으로 이동
  // advanceResult.movedTo === "REVIEW_WRONG" 또는 "SUMMARY"
}
```

#### 3-2. 한 문제씩 채점 (gradeOneMcq) ⚠️ 중요

**현재 사용 중인 방식:**
```typescript
// ✅ 효율적인 방법: 로컬에서 카운트 관리, 마지막 문제에서만 세션 조회
const questions = [...]; // 5문제
let answeredCount = 0;

for (let i = 0; i < questions.length; i++) {
  const question = questions[i];
  const result = await gradeOneMcq(learningSessionId, {
    topicId: 1,
    questionId: question.id,
    label: userAnswer
  });
  
  answeredCount++;
  const isLastQuestion = (i === questions.length - 1);
  
  // 마지막 문제(5번째)에서만 세션 조회 및 advance 호출
  if (isLastQuestion) {
    const session = await getSession(learningSessionId);
    const mcqStep = session.steps.find(s => s.step === "MCQ");
    const metadata = JSON.parse(mcqStep.detailsJson || "{}");
    
    // advance 호출
    const advanceResult = await advance({
      sessionId: learningSessionId,
      step: "MCQ",
      score: metadata.scorePct || 0,
      detailsJson: mcqStep.detailsJson
    });
    
    // 오답이 없으면 자동으로 SUMMARY로 이동
    // 오답이 있으면 REVIEW_WRONG으로 이동
    // advanceResult.movedTo === "REVIEW_WRONG" 또는 "SUMMARY"
  }
}
```

### 4. PRACTICAL 단계

#### 4-1. 여러 문제 한 번에 제출 (submitPractical)

**이전:**
```typescript
const result = await submitPractical(learningSessionId, {
  topicId: 1,
  answers: [...]
});
// 내부적으로 PRACTICAL → REVIEW_WRONG 또는 SUMMARY 전이
```

**변경 후:**
```typescript
// 1. 문제 제출
const submitResult = await submitPractical(learningSessionId, {
  topicId: 1,
  answers: [...]
});

// 2. 모든 문제를 풀었을 때만 advance 호출
if (submitResult.status === "COMPLETE") {
  const advanceResult = await advance({
    sessionId: learningSessionId,
    step: "PRACTICAL",
    score: submitResult.calculatedScore,
    detailsJson: JSON.stringify(submitResult.metadata)
  });
  
  // 오답이 없으면 자동으로 SUMMARY로 이동
  // 오답이 있으면 REVIEW_WRONG으로 이동
  // advanceResult.movedTo === "REVIEW_WRONG" 또는 "SUMMARY"
}
```

#### 4-2. 한 문제씩 채점 (gradeOnePractical) ⚠️ 중요

**현재 사용 중인 방식:**
```typescript
// ✅ 효율적인 방법: 로컬에서 카운트 관리, 마지막 문제에서만 세션 조회
const questions = [...]; // 5문제 (SHORT 3 + LONG 2)
let answeredCount = 0;

for (let i = 0; i < questions.length; i++) {
  const question = questions[i];
  const result = await gradeOnePractical(learningSessionId, {
    topicId: 1,
    questionId: question.id,
    userText: userAnswer
  });
  
  answeredCount++;
  const isLastQuestion = (i === questions.length - 1);
  
  // 마지막 문제(5번째)에서만 세션 조회 및 advance 호출
  if (isLastQuestion) {
    const session = await getSession(learningSessionId);
    const practicalStep = session.steps.find(s => s.step === "PRACTICAL");
    const metadata = JSON.parse(practicalStep.detailsJson || "{}");
    
    // advance 호출
    const advanceResult = await advance({
      sessionId: learningSessionId,
      step: "PRACTICAL",
      score: practicalStep.score || 0,
      detailsJson: practicalStep.detailsJson
    });
    
    // 오답이 없으면 자동으로 SUMMARY로 이동
    // 오답이 있으면 REVIEW_WRONG으로 이동
    // advanceResult.movedTo === "REVIEW_WRONG" 또는 "SUMMARY"
  }
}
```

### 5. REVIEW_WRONG 단계

**이전:**
```typescript
// 오답 정리 화면을 본 후 수동으로 SUMMARY로 이동
// 프론트엔드에서 오답 여부 확인 후 조건부 처리
```

**변경 후:**
```typescript
// 오답 정리 화면을 본 후 advance 호출
await advance({
  sessionId: learningSessionId,
  step: "REVIEW_WRONG",
  score: null,
  detailsJson: null
});
// 응답: movedTo === "SUMMARY"
```

### 6. SUMMARY 단계

**이전:**
```typescript
// summary() 호출 시 자동으로 DONE 처리
await summary(learningSessionId);
```

**변경 후:**
```typescript
// 요약 화면을 본 후 advance 호출
const result = await advance({
  sessionId: learningSessionId,
  step: "SUMMARY",
  score: null,
  detailsJson: null
});
// 응답: movedTo === "END", status === "DONE"
```

---

## ⚠️ 주의사항

### 1. Grade-One 사용 시 필수 확인사항

**중요:** `gradeOneMini`, `gradeOneMcq`, `gradeOnePractical`을 사용하는 경우:

1. **각 문제를 풀 때마다 `gradeOne` API 호출**
   - 문제를 풀 때마다 즉시 채점 결과를 받을 수 있음
   - 메타데이터는 자동으로 누적됨

2. **로컬에서 문제 개수 관리 (권장)**
   - 프론트엔드는 문제 개수를 알고 있음 (MINI: 4개, MCQ: 5개, PRACTICAL: 5개)
   - 로컬에서 카운트를 관리하여 마지막 문제에서만 세션 조회
   - **매번 세션을 조회할 필요 없음** (성능 최적화)

3. **마지막 문제에서만 세션 조회 및 `advance` 호출**
   ```typescript
   // ✅ 효율적인 방법
   const questions = [...]; // 문제 목록
   
   for (let i = 0; i < questions.length; i++) {
     await gradeOneMini(learningSessionId, { ... });
     
     // 마지막 문제에서만 세션 조회
     if (i === questions.length - 1) {
       const session = await getSession(learningSessionId);
       const miniStep = session.steps.find(s => s.step === "MINI");
       const metadata = JSON.parse(miniStep.detailsJson || "{}");
       
       // advance 호출
       await advance({
         sessionId: learningSessionId,
         step: "MINI",
         score: metadata.scorePct,
         detailsJson: miniStep.detailsJson
       });
     }
   }
   ```

4. **`advance` 호출 전 검증**
   - 백엔드에서도 모든 문제를 풀었는지 검증함
   - 미완료 시 에러 발생

### 2. 완료 조건 검증

**중요:** `advance` API는 다음 조건을 검증합니다:
- MINI: 4문제 모두 풀어야 함
- MCQ: 5문제 모두 풀어야 함
- PRACTICAL: 할당된 모든 문제를 풀어야 함

**모든 문제를 풀지 않았는데 `advance`를 호출하면 에러가 발생합니다:**
```json
{
  "status": 400,
  "message": "MINI 단계의 모든 문제를 풀어야 합니다. (완료: 2/4)"
}
```

### 2. 단계 상태 확인

`advance` 호출 전에 현재 단계가 `IN_PROGRESS` 또는 `READY` 상태인지 확인하세요:
```typescript
// 세션 상태 조회
const session = await getSession(learningSessionId);
const currentStep = session.steps.find(s => s.state === "IN_PROGRESS");

if (currentStep && currentStep.step === "MINI") {
  // MINI 단계 진행 중
}
```

### 3. 오답 자동 건너뛰기

**프론트엔드에서 오답 여부를 확인할 필요 없음:**
```typescript
// ❌ 불필요한 로직
const hasWrongAnswers = metadata.wrongQuestionIds.length > 0;
if (!hasWrongAnswers) {
  // REVIEW_WRONG 건너뛰기
}

// ✅ 백엔드가 자동 처리
const result = await advance({
  sessionId: learningSessionId,
  step: "MCQ",
  score: score,
  detailsJson: JSON.stringify(metadata)
});
// result.movedTo가 자동으로 "SUMMARY" 또는 "REVIEW_WRONG"으로 설정됨
```

### 4. 에러 처리

`advance` API 호출 시 다음 에러가 발생할 수 있습니다:

**단계가 진행 가능한 상태가 아닐 때:**
```json
{
  "status": 400,
  "message": "단계가 진행 가능한 상태가 아닙니다. 현재 상태: COMPLETE"
}
```

**모든 문제를 풀지 않았을 때:**
```json
{
  "status": 400,
  "message": "MINI 단계의 모든 문제를 풀어야 합니다. (완료: 2/4)"
}
```

**세션 소유자가 아닐 때:**
```json
{
  "status": 403,
  "message": "세션 소유자가 아닙니다."
}
```

---

## 📝 체크리스트

마이그레이션 시 다음 사항을 확인하세요:

- [ ] **세션 시작 시 `learningSessionId` 저장 (localStorage 권장)**
- [ ] **페이지 새로고침 시 저장된 세션 ID로 세션 복원**
- [ ] **세션 완료 시 저장된 ID 삭제**
- [ ] `completeConcept()` 호출 후 `advance` API 호출 추가
- [ ] `submitMini()` 호출 후 모든 문제 완료 시 `advance` API 호출 추가
- [ ] `submitMcq()` 호출 후 모든 문제 완료 시 `advance` API 호출 추가
- [ ] `submitPractical()` 호출 후 모든 문제 완료 시 `advance` API 호출 추가
- [ ] **`gradeOneMini` 사용 시: 모든 문제 완료 확인 후 `advance` 호출 추가**
- [ ] **`gradeOneMcq` 사용 시: 모든 문제 완료 확인 후 `advance` 호출 추가**
- [ ] **`gradeOnePractical` 사용 시: 모든 문제 완료 확인 후 `advance` 호출 추가**
- [ ] REVIEW_WRONG 화면 종료 시 `advance` API 호출 추가
- [ ] SUMMARY 화면 종료 시 `advance` API 호출 추가
- [ ] 오답 여부 확인 로직 제거 (백엔드가 자동 처리)
- [ ] 단계 전이 조건부 로직 제거 (백엔드가 자동 처리)
- [ ] `advance` 응답의 `movedTo` 필드를 사용하여 다음 화면으로 이동
- [ ] 에러 처리 추가 (완료 조건 미충족, 상태 오류 등)

---

## 🔍 예시 코드

### React 예시

```typescript
// hooks/useLearningSession.ts
export const useLearningSession = () => {
  const advanceStep = async (
    sessionId: number,
    step: string,
    score?: number,
    metadata?: any
  ) => {
    const response = await fetch('/api/study/session/advance', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sessionId,
        step,
        score: score ?? null,
        detailsJson: metadata ? JSON.stringify(metadata) : null
      })
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.message || '단계 전이 실패');
    }

    return await response.json();
  };

  return { advanceStep };
};

// components/MiniStep.tsx
const MiniStep = ({ learningSessionId }: Props) => {
  const { advanceStep } = useLearningSession();
  const [answers, setAnswers] = useState<Answer[]>([]);

  const handleSubmit = async () => {
    try {
      // 1. 문제 제출
      const submitResult = await submitMini(learningSessionId, {
        topicId: 1,
        answers
      });

      // 2. 모든 문제 완료 시 advance 호출
      if (submitResult.status === "COMPLETE") {
        const advanceResult = await advanceStep(
          learningSessionId,
          "MINI",
          submitResult.calculatedScore,
          submitResult.metadata
        );

        // 3. 다음 단계로 이동
        if (advanceResult.movedTo === "MCQ") {
          navigate(`/study/mcq/${learningSessionId}`);
        }
      }
    } catch (error) {
      // 에러 처리
      if (error.message.includes("모든 문제를 풀어야 합니다")) {
        alert("모든 문제를 풀어주세요.");
      } else {
        alert("오류가 발생했습니다: " + error.message);
      }
    }
  };

  return (
    <div>
      {/* 문제 UI */}
      <button onClick={handleSubmit}>제출</button>
    </div>
  );
};
```

### Vue 예시

```typescript
// composables/useLearningSession.ts
export const useLearningSession = () => {
  const advanceStep = async (
    sessionId: number,
    step: string,
    score?: number,
    metadata?: any
  ) => {
    const response = await fetch('/api/study/session/advance', {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({
        sessionId,
        step,
        score: score ?? null,
        detailsJson: metadata ? JSON.stringify(metadata) : null
      })
    });

    if (!response.ok) {
      const error = await response.json();
      throw new Error(error.message || '단계 전이 실패');
    }

    return await response.json();
  };

  return { advanceStep };
};

// components/MiniStep.vue
<script setup lang="ts">
import { useLearningSession } from '@/composables/useLearningSession';

const { advanceStep } = useLearningSession();
const answers = ref<Answer[]>([]);

const handleSubmit = async () => {
  try {
    // 1. 문제 제출
    const submitResult = await submitMini(learningSessionId.value, {
      topicId: 1,
      answers: answers.value
    });

    // 2. 모든 문제 완료 시 advance 호출
    if (submitResult.status === "COMPLETE") {
      const advanceResult = await advanceStep(
        learningSessionId.value,
        "MINI",
        submitResult.calculatedScore,
        submitResult.metadata
      );

      // 3. 다음 단계로 이동
      if (advanceResult.movedTo === "MCQ") {
        router.push(`/study/mcq/${learningSessionId.value}`);
      }
    }
  } catch (error) {
    // 에러 처리
    if (error.message.includes("모든 문제를 풀어야 합니다")) {
      alert("모든 문제를 풀어주세요.");
    } else {
      alert("오류가 발생했습니다: " + error.message);
    }
  }
};
</script>
```

---

## 📞 문의사항

마이그레이션 중 문제가 발생하거나 질문이 있으시면 백엔드 팀에 문의해주세요.

---

**작성일:** 2025-01-27  
**버전:** 1.0  
**작성자:** Backend Team

