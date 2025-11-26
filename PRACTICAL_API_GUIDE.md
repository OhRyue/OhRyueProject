# 실기(Practical) API 연동 가이드

## 📌 주요 변경 사항

**실기가 필기와 동일한 패턴으로 동작하도록 변경되었습니다!** 
- 이전: LearningSession 없이 바로 문제부터 시작
- 현재: **LearningSession 기반 플로우** (필기와 동일)
- **반드시 `POST /api/study/session/start`부터 시작**해야 합니다!
- 세션당 문제가 고정됨 (같은 `learningSessionId`로 재요청하면 동일한 문제 반환)

---

## 🔄 학습 플로우

```
1. 개념 보기
   ↓
2. 미니체크(OX 4문)
   ↓
3. 실기 세트 (SHORT 3문 + LONG 2문 = 총 5문)
   ↓
4. 주관식의 오답 보기
   ↓
5. 결과 보기(요약)
```

---

## 📡 API 엔드포인트

### Base URL
```
/api/study/practical
```

### 0. 세션 시작 (필수!)

**⚠️ 중요: 실기 학습을 시작하기 전에 반드시 세션을 시작해야 합니다!**

```http
POST /api/study/session/start
Content-Type: application/json

{
  "topicId": 1,
  "mode": "PRACTICAL",  // 또는 "WRITTEN"
  "resume": false  // true: 이전 세션 재개, false: 새로 시작
}
```

**응답:**
```json
{
  "sessionId": 123,  // learningSessionId - 이 값을 저장해두세요!
  "status": "IN_PROGRESS"
}
```

**중요:**
- 반환된 `sessionId`(learningSessionId)를 **모든 후속 요청에서 사용**합니다
- 이 값을 저장하지 않으면 학습을 이어갈 수 없습니다

---

### 0-1. 세션 상태 조회 (정기적으로 호출 권장)

**⚠️ 중요: 프론트엔드에서 정기적으로 세션 상태를 조회하여 현재 진행 단계를 확인하는 것을 권장합니다!**

```http
GET /api/study/session/{sessionId}
```

**파라미터:**
- `sessionId`: LearningSession ID (경로 변수, **필수**)

**응답 예시:**
```json
{
  "sessionId": 123,
  "topicId": 1,
  "mode": "PRACTICAL",
  "status": "IN_PROGRESS",
  "currentStep": "PRACTICAL_SET",  // 현재 진행 중인 단계
  "steps": [
    {
      "stepId": 1,
      "step": "CONCEPT",
      "state": "COMPLETE",
      "scorePct": null,
      "metadata": null
    },
    {
      "stepId": 2,
      "step": "MINI",
      "state": "COMPLETE",
      "scorePct": 100,
      "metadata": "{\"total\":4,\"correct\":4,\"passed\":true}"
    },
    {
      "stepId": 3,
      "step": "PRACTICAL",
      "state": "IN_PROGRESS",  // 현재 진행 중
      "scorePct": null,
      "metadata": null
    },
    {
      "stepId": 4,
      "step": "SUMMARY",
      "state": "READY",
      "scorePct": null,
      "metadata": null
    }
  ]
}
```

**사용 시나리오:**
1. **페이지 진입 시**: 저장된 `sessionId`로 세션 상태 조회하여 현재 단계 확인
2. **정기적인 폴링**: 사용자가 다른 페이지로 이동했다가 돌아왔을 때를 대비해 주기적으로 조회
3. **단계 전환 확인**: 각 단계 완료 후 다음 단계로 자동 전환되었는지 확인

**프론트엔드 구현 예시:**
```typescript
// 세션 상태를 주기적으로 조회 (예: 5초마다)
setInterval(async () => {
  if (learningSessionId) {
    const response = await fetch(`/api/study/session/${learningSessionId}`);
    const session = await response.json();
    
    // 현재 단계에 따라 UI 업데이트
    if (session.currentStep === 'PRACTICAL_SET') {
      // 실기 세트 화면 표시
    } else if (session.currentStep === 'SUMMARY') {
      // 요약 화면으로 이동
    }
  }
}, 5000);
```

---

### 1. 개념 보기

#### 📥 개념 가져오기
```http
GET /api/study/practical/concept/{topicId}?sessionId={learningSessionId}
```

**파라미터:**
- `topicId`: 토픽 ID (경로 변수)
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**응답 예시:**
```json
{
  "topicId": 1,
  "title": "토픽 제목",
  "sections": [
    {
      "orderNo": 1,
      "subCode": "1.1",
      "title": "섹션 제목",
      "importance": 3,
      "blocks": [
        {
          "type": "heading",
          "text": "제목 텍스트",
          "items": null,
          "url": null,
          "alt": null,
          "caption": null,
          "headers": null,
          "rows": null
        },
        {
          "type": "paragraph",
          "text": "본문 내용...",
          "items": null,
          "url": null,
          "alt": null,
          "caption": null,
          "headers": null,
          "rows": null
        }
      ]
    }
  ]
}
```

#### ✅ 개념 완료 처리
```http
POST /api/study/practical/concept/complete?sessionId={learningSessionId}
```

**파라미터:**
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**설명:**
- 개념 보기를 완료했을 때 호출
- CONCEPT 단계를 COMPLETE로 변경하고 MINI 단계를 IN_PROGRESS로 변경

---

### 2. 미니체크 (OX 문제)

#### 📥 문제 가져오기
```http
GET /api/study/practical/mini/{topicId}?sessionId={learningSessionId}
```

**파라미터:**
- `topicId`: 토픽 ID (경로 변수)
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**응답 예시:**
```json
{
  "sessionId": 123,
  "mode": "PRACTICAL",
  "step": "PRACTICAL_MINI",
  "status": "IN_PROGRESS",
  "nextStep": null,  // 통과 시 "PRACTICAL_SET"
  "meta": {},
  "payload": {
    "items": [
      {
        "questionId": 1,
        "text": "문제 본문..."
      },
      // ... 총 4문
    ]
  },
  "learningSessionId": 123  // LearningSession ID
}
```

#### ✅ 제출
```http
POST /api/study/practical/mini/submit?sessionId={learningSessionId}
Content-Type: application/json

{
  "topicId": 1,
  "answers": [
    {
      "questionId": 1,
      "answer": true  // true: O, false: X
    },
    // ... 4개
  ]
}
```

**응답:**
```json
{
  "sessionId": 123,
  "status": "COMPLETE",  // 전부 정답 시
  "nextStep": "PRACTICAL_SET",  // 항상 PRACTICAL_SET으로 이동 가능
  "payload": {
    "total": 4,
    "correct": 4,
    "passed": true,
    "items": [...],
    "wrongQuestionIds": []
  }
}
```

#### ⚡ 단건 즉시 채점
```http
POST /api/study/practical/mini/grade-one?sessionId={learningSessionId}
Content-Type: application/json

{
  "topicId": 1,
  "questionId": 1,
  "answer": true
}
```

**파라미터:**
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

---

### 3. 실기 세트 (SHORT/LONG 문제)

#### 📥 문제 가져오기
```http
GET /api/study/practical/set/{topicId}?sessionId={learningSessionId}
```

**파라미터:**
- `topicId`: 토픽 ID (경로 변수)
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**중요:** 
- **세션 기반으로 동작합니다**
- MINI 단계의 StudySession에 PRACTICAL 문제가 할당됨
- 같은 `learningSessionId`로 재요청하면 동일한 문제 반환

**응답 예시:**
```json
{
  "sessionId": 123,
  "mode": "PRACTICAL",
  "step": "PRACTICAL_SET",
  "status": "IN_PROGRESS",
  "nextStep": null,
  "meta": {},
  "payload": {
    "items": [
      {
        "questionId": 10,
        "type": "SHORT",  // 또는 "LONG"
        "text": "문제 본문...",
        "imageUrl": "https://..."  // 선택
      },
        // SHORT 3문 + LONG 2문 = 총 5문
    ]
  },
  "learningSessionId": 123
}
```

#### ✅ 제출 방법

**방법 1: 단건 즉시 채점 (권장) ⭐**

**⚠️ 중요: 프론트엔드에서는 각 문제를 풀 때마다 이 방법을 사용해야 합니다!**

각 문제를 풀 때마다 `POST /api/study/practical/grade-one`를 호출하세요. 자세한 내용은 아래 "⚡ 단건 즉시 채점" 섹션을 참고하세요.

**방법 2: 배치 제출 (선택)**

모든 문제를 한 번에 제출하는 방법입니다. 단건 즉시 채점을 사용하는 경우에는 이 방법을 사용할 필요가 없습니다.

```http
POST /api/study/practical/submit?sessionId={learningSessionId}
Content-Type: application/json

{
  "topicId": 1,
  "answers": [
    {
      "questionId": 10,
      "userText": "사용자가 입력한 답안 텍스트"
    },
    // ... 5개
  ]
}
```

**응답 (오답이 있는 경우):**
```json
{
  "sessionId": 123,
  "status": "COMPLETE",  // 모든 문제를 다 풀었을 때
  "nextStep": "PRACTICAL_REVIEW_WRONG",  // ⚠️ 주의: 실제 단계 코드는 "REVIEW_WRONG"
  "payload": {
    "total": 5,
    "avgScore": 75,
    "items": [
      {
        "questionId": 10,
        "score": 90,  // 0~100 (AI 채점)
        "baseExplanation": "기본 해설...",
        "aiExplanation": "맞춤 해설..."  // AI가 생성한 해설 (저장됨)
      },
      // ...
    ],
    "wrongQuestionIds": [12, 13]  // 60점 미만 문제 ID
  }
}
```

**응답 (오답이 없는 경우):**
```json
{
  "sessionId": 123,
  "status": "COMPLETE",  // 모든 문제 60점 이상
  "nextStep": "PRACTICAL_SUMMARY",
  "payload": {
    "total": 5,
    "avgScore": 85,
    "items": [...],
    "wrongQuestionIds": []  // 오답 없음
  }
}
```

**중요:**
- 제출 시 생성된 AI 해설은 `StudySessionItem`에 저장됩니다
- 오답 조회 시 저장된 AI 해설을 가져오며, **다시 생성하지 않습니다**
- 같은 문제를 여러 번 제출한 경우, 가장 최근에 제출한 답안의 AI 해설이 저장됩니다

**채점 기준:**
- `score >= 60`: 통과
- `score < 60`: 오답으로 간주

**제출 완료 후 동작:**
1. 모든 문제를 다 풀었을 때 (`status: "COMPLETE"`):
   - PRACTICAL 단계가 자동으로 COMPLETE로 변경됨
   - `wrongQuestionIds`에 오답이 있으면:
     - REVIEW_WRONG 단계가 자동으로 IN_PROGRESS로 변경됨
     - `nextStep`은 `"PRACTICAL_REVIEW_WRONG"` (⚠️ 참고: 실제 단계 코드는 `"REVIEW_WRONG"`)
   - `wrongQuestionIds`가 비어있으면:
     - SUMMARY 단계가 자동으로 IN_PROGRESS로 변경됨
     - `nextStep`은 `"PRACTICAL_SUMMARY"`

#### ⚡ 단건 즉시 채점

**⚠️ 중요: 프론트엔드에서 각 문제를 풀 때마다 이 API를 반드시 호출해야 합니다!**

```http
POST /api/study/practical/grade-one?sessionId={learningSessionId}
Content-Type: application/json

{
  "topicId": 1,
  "questionId": 10,
  "userText": "사용자가 입력한 답안"
}
```

**파라미터:**
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**응답:**
```json
{
  "score": 85,                  // 0~100 (AI 채점)
  "baseExplanation": "기본 해설...",
  "aiExplanation": "맞춤 해설..."  // AI가 생성한 해설 (저장됨)
}
```

**중요:**
- **각 문제를 풀 때마다 이 API를 호출해야 합니다**
- 내부적으로 `submitPractical`을 호출하므로, 답안이 세션에 저장됩니다
- 마지막 문제를 이 API로 제출하면, 모든 문제가 다 풀린 상태가 되면 **자동으로 PRACTICAL 단계가 COMPLETE로 변경**됩니다
- 모든 문제를 `grade-one`으로 풀었을 때는 별도의 배치 제출(`/submit`) 없이도 완료 처리됩니다
- AI 해설은 각 호출 시 생성되어 저장되며, 나중에 오답 조회 시 저장된 해설을 가져옵니다

**플로우:**
1. 사용자가 문제를 풀고 답안 입력
2. `POST /api/study/practical/grade-one?sessionId={learningSessionId}` 호출
3. 응답으로 점수와 해설 받음 → 화면에 표시
4. 다음 문제로 이동
5. 마지막 문제 제출 시 → 자동으로 완료 처리 (배치 제출 불필요)

**주의:**
- 모든 문제를 `grade-one`으로 풀면 자동 완료되지만, 세션 상태를 확인하려면 `GET /api/study/session/{sessionId}`를 호출하여 현재 단계를 확인하세요

---

### 4. 주관식의 오답 보기

**⚠️ 중요: 실기 세트 제출 완료 후 오답이 있으면 이 단계로 자동 이동됩니다!**

#### 📥 오답 문제 조회 (방법 1: LearningSession 기반 - 권장)

**가장 간단한 방법입니다. 실기 세트 제출 후에는 이 방법을 사용하세요!**

```http
GET /api/study/wrong/practical/learning-session?learningSessionId={learningSessionId}
```

**파라미터:**
- `learningSessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**설명:**
- LearningSession의 PRACTICAL 단계에서 틀린 문제(60점 미만)만 조회
- 실기 세트 제출 후 자동으로 오답 단계로 이동했을 때 사용
- **이 방법을 사용하면 `step` 파라미터를 지정할 필요가 없습니다**

#### 📥 오답 문제 조회 (방법 2: StudySession 기반)

```http
GET /api/study/wrong/practical/session?sessionId={sessionId}&step=PRACTICAL_SET
```

**파라미터:**
- `sessionId`: StudySession ID (쿼리 파라미터, **필수**)
- `step`: 단계 코드 (쿼리 파라미터, 기본값: `PRACTICAL_SET`)

**설명:**
- 해당 세션에서 실기 세트(PRACTICAL_SET) 단계에서 틀린 문제(60점 미만)만 조회
- `step` 파라미터: `PRACTICAL_SET` (기본값), `PRACTICAL_MINI` 등
- **참고:** `PRACTICAL_REVIEW`는 별도의 리뷰 모드(6번 섹션)에서 사용됩니다

**공통 중요 사항:**
- **중요:** AI 해설은 문제 제출 시 생성되어 저장되며, 오답 조회 시 저장된 해설을 가져옵니다
- AI 해설을 다시 생성하지 않으며, 제출 시 생성된 해설만 반환됩니다

**응답 예시:**
```json
{
  "items": [
    {
      "questionId": 10,
      "type": "SHORT",
      "text": "문제 본문...",
      "myAnswer": "{\"answer\":\"사용자 답안\",\"score\":50,\"passed\":false}",
      "correctAnswer": "",
      "baseExplanation": "기본 해설...",
      "imageUrl": "https://...",
      "aiExplanation": "좋은 시도입니다! 하지만 답안에서 몇 가지 중요한 개념이 누락되었습니다. 방화벽의 주요 기능인 패킷 필터링과 접근 제어를 정확히 설명하셨지만, 상태 기반 검사(Stateful Inspection)에 대한 언급이 추가되면 더 완벽한 답변이 될 것입니다."
    }
  ]
}
```

**응답 필드 설명:**
- `questionId`: 문제 ID
- `type`: 문제 유형 (`SHORT` 또는 `LONG`)
- `text`: 문제 본문
- `myAnswer`: 사용자가 제출한 답안 (JSON 문자열)
- `correctAnswer`: 정답 (실기는 정답 개념이 없으므로 빈 문자열)
- `baseExplanation`: DB에 저장된 기본 해설
- `imageUrl`: 문제 이미지 URL (선택)
- `aiExplanation`: **AI가 생성한 맞춤 해설** (제출 시 생성되어 저장된 것)
  - 제출 시 생성된 해설이 있으면 해당 해설 반환
  - 해설이 없거나 저장되지 않은 경우 빈 문자열(`""`) 반환

#### ✅ 오답 정리 완료 처리

**오답 문제를 모두 확인한 후, 요약 화면으로 이동하기 위해 호출합니다.**

```http
POST /api/study/session/advance
Content-Type: application/json

{
  "sessionId": 123,              // LearningSession ID (⚠️ StudySession ID가 아님!)
  "step": "REVIEW_WRONG",        // ⚠️ 주의: "PRACTICAL_REVIEW_WRONG"이 아니라 "REVIEW_WRONG"
  "score": null,                 // 선택 (오답 정리 단계는 점수 없음)
  "detailsJson": null            // 선택 (추가 메타데이터)
}
```

**응답:**
```json
{
  "sessionId": 123,
  "status": "SUMMARY",           // 다음 단계 코드
  "movedTo": "IN_PROGRESS"       // LearningSession 상태
}
```

**⚠️ 중요:**
- `sessionId`는 **LearningSession ID**입니다! StudySession ID가 아닙니다!
- `step` 파라미터는 `"REVIEW_WRONG"`을 사용해야 합니다 (응답의 `nextStep: "PRACTICAL_REVIEW_WRONG"`과는 다름)
- 응답의 `status`가 `"SUMMARY"`이면 요약 화면으로 이동하면 됩니다

**프론트엔드 플로우:**
1. 실기 세트 제출 완료 → `nextStep: "PRACTICAL_REVIEW_WRONG"` 수신
2. 오답이 있으면 오답 조회 API 호출 → 오답 화면 표시
3. 사용자가 오답 확인 완료 후 "다음" 버튼 클릭
4. `POST /api/study/session/advance` 호출 (step: `"REVIEW_WRONG"`)
5. 응답의 `status === "SUMMARY"` 확인
6. 요약 화면으로 이동

---

### 5. 요약

#### 📊 진행 요약 조회
```http
GET /api/study/practical/summary?topicId={topicId}&sessionId={learningSessionId}
```

**파라미터:**
- `topicId`: 토픽 ID (쿼리 파라미터)
- `sessionId`: LearningSession ID (쿼리 파라미터, **필수**)

**응답:**
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "practicalTotal": 5,
    "practicalPassed": 4,  // 60점 이상 문제 수
    "summary": "AI가 생성한 요약 텍스트...",
    "completed": true  // miniPassed && practicalCompleted
  }
}
```

---

### 6. 리뷰 (선택, Micro 학습 플로우와 별개)

**⚠️ 참고:** 리뷰는 Micro 학습 플로우에 포함되지 않는 별도 기능입니다.

#### 📥 리뷰 세트 가져오기
```http
GET /api/study/practical/review/{rootTopicId}
```

**설명:**
- 루트 토픽과 모든 하위 토픽에서 문제 선발
- SHORT 6문 + LONG 4문 = 총 10문
- **세션 기반으로 동작** (동일한 `sessionId`로 재요청 시 같은 문제)
- Micro 학습 플로우와는 독립적으로 동작합니다

#### ✅ 리뷰 제출
```http
POST /api/study/practical/review/submit
Content-Type: application/json

{
  "rootTopicId": 1,
  "answers": [
    {
      "questionId": 20,
      "userText": "답안..."
    },
    // ... 10개
  ]
}
```

---

## ⚠️ 주의사항

### 1. 세션 기반 동작
- **실기는 이제 세션 기반입니다!**
- 같은 `sessionId`로 재요청하면 동일한 문제가 반환됩니다
- 사용자가 문제를 보고 돌아와도 동일한 문제가 보여야 합니다
- 프론트엔드에서 `sessionId`를 저장하고 재사용해야 합니다

### 2. 필기와의 차이점

| 항목 | 필기 (Written) | 실기 (Practical) |
|------|---------------|------------------|
| 개념 보기 | ✅ 있음 | ✅ 있음 |
| LearningSession | ✅ 사용 (sessionId 필수) | ✅ 사용 (sessionId 필수) |
| StudySession | ✅ 사용 | ✅ 사용 |
| 세션 ID 위치 | 쿼리 파라미터 | 쿼리 파라미터 (동일) |
| 미니체크 | OX 4문 | OX 4문 (동일) |
| 메인 세트 | MCQ 5문 | SHORT 3문 + LONG 2문 |

### 3. 세션 관리

**필기 (Written):**
```javascript
// 1. LearningSession 시작 (필수)
POST /api/study/session/start
→ learningSessionId 반환

// 2. 각 단계에서 learningSessionId를 쿼리로 전달
GET /api/study/written/mini/{topicId}?sessionId={learningSessionId}
```

**실기 (Practical):**
```javascript
// 1. LearningSession 시작 (필수)
POST /api/study/session/start
→ learningSessionId 반환

// 2. 각 단계에서 learningSessionId를 쿼리로 전달
GET /api/study/practical/mini/{topicId}?sessionId={learningSessionId}
```

### 4. 플로우 제어

**미니체크:**
- `status: "COMPLETE"` → `nextStep: "PRACTICAL_SET"`
- **중요:** 미니체크 통과 여부와 관계없이 항상 `PRACTICAL_SET`으로 이동 가능
- **4문제를 모두 풀면 (grade-one 포함):**
  - MINI 단계가 `COMPLETE`로 변경됨
  - PRACTICAL 단계가 자동으로 `IN_PROGRESS`로 변경됨
  - 세션 상태 조회 시 `currentStep`이 `"PRACTICAL"`로 반환됨

**실기 세트:**
- 모든 문제 60점 이상 → `status: "COMPLETE"`, `nextStep: "PRACTICAL_SUMMARY"`
- 하나라도 60점 미만 → `status: "IN_PROGRESS"`, `nextStep: "PRACTICAL_SET"` (재시도 가능)

**세션 상태 조회 (`GET /api/study/session/{sessionId}`) 동작:**
- `currentStep` 결정 로직:
  1. 먼저 `IN_PROGRESS` 상태인 단계를 찾음
  2. 없으면 `COMPLETE`된 단계 다음의 첫 번째 `READY` 단계를 찾음
  3. 여전히 없으면 첫 번째 `READY` 단계를 찾음

**예시:**
- CONCEPT: COMPLETE, MINI: COMPLETE, PRACTICAL: IN_PROGRESS → `currentStep: "PRACTICAL"`
- CONCEPT: COMPLETE, MINI: IN_PROGRESS, PRACTICAL: READY → `currentStep: "MINI"`
- CONCEPT: COMPLETE, MINI: COMPLETE, PRACTICAL: READY → `currentStep: "PRACTICAL"`

---

## 🔧 구현 예시

### React/Vue 예시

```typescript
```

---

## 🐛 에러 처리

### 세션에 할당되지 않은 문제 제출 시
```json
{
  "status": 400,
  "message": "세션에 할당되지 않은 문제입니다: {questionId}"
}
```
→ **원인:** 다른 세션의 문제를 제출하려고 시도
→ **해결:** 같은 `learningSessionId`의 문제만 제출해야 함

### 세션이 초기화되지 않았을 때
```json
{
  "status": 500,
  "message": "StudySession이 초기화되지 않았습니다. 세션을 먼저 시작해주세요."
}
```
→ **원인:** `POST /api/study/session/start`를 호출하지 않았거나, 잘못된 `learningSessionId` 사용
→ **해결:** 반드시 세션 시작 API를 먼저 호출하고, 반환된 `sessionId`를 사용

---

## 📝 체크리스트

프론트엔드 구현 시 확인사항:

- [ ] **`POST /api/study/session/start`를 먼저 호출하는가?** (가장 중요!)
- [ ] `learningSessionId`를 저장하고 모든 요청에 전달하는가?
- [ ] **정기적으로 `GET /api/study/session/{sessionId}`를 호출하여 세션 상태를 확인하는가?**
- [ ] **개념 보기** 단계가 첫 단계로 포함되어 있는가?
- [ ] 개념 완료 처리를 호출하는가?
- [ ] 같은 `learningSessionId`로 재요청 시 동일한 문제가 표시되는가?
- [ ] 제출 시 세션에 할당된 문제만 제출하는가?
- [ ] 미니체크 통과 여부와 관계없이 실기 세트로 이동 가능한가?
- [ ] 실기 세트 완료 후 오답 보기가 가능한가?
- [ ] 오답 조회 시 AI 해설이 표시되는가? (제출 시 생성된 해설)
- [ ] 실기 세트 완료 후 오답 정리 플로우가 올바르게 동작하는가?
- [ ] `advance` API 호출 시 `step` 파라미터를 올바르게 사용하는가? (`"REVIEW_WRONG"`)
- [ ] 실기 세트 완료 시 (60점 이상) 요약 페이지로 이동하는가?
- [ ] 에러 처리 (세션 할당되지 않은 문제) 구현했는가?

---

## 🔄 실기 세트 완료 후 상세 플로우

**mini 문제를 다 풀고, 주관식 문제(실기 세트)를 다 풀었을 때부터의 동작:**

### 1. 실기 세트 제출 완료

**API 호출:**
```http
POST /api/study/practical/submit?sessionId={learningSessionId}
```

**응답 확인:**
- `status: "COMPLETE"` → 모든 문제를 다 풀었음을 의미
- `payload.wrongQuestionIds` 배열 확인
  - 비어있으면: 오답 없음 → 바로 요약 화면으로
  - 값이 있으면: 오답 있음 → 오답 정리 단계로

**백엔드 자동 동작:**
- PRACTICAL 단계가 자동으로 `COMPLETE`로 변경됨
- 오답이 있으면: REVIEW_WRONG 단계가 자동으로 `IN_PROGRESS`로 변경됨
- 오답이 없으면: SUMMARY 단계가 자동으로 `IN_PROGRESS`로 변경됨

### 2-A. 오답이 없는 경우 (바로 요약으로)

1. **요약 조회**
   ```http
   GET /api/study/practical/summary?topicId={topicId}&sessionId={learningSessionId}
   ```
2. 요약 화면 표시
3. 완료!

### 2-B. 오답이 있는 경우 (오답 정리 후 요약으로)

**단계 1: 오답 문제 조회**

```http
GET /api/study/wrong/practical/learning-session?learningSessionId={learningSessionId}
```

**응답 예시:**
```json
{
  "items": [
    {
      "questionId": 12,
      "type": "SHORT",
      "text": "문제 본문...",
      "myAnswer": "{\"answer\":\"사용자 답안\",\"score\":50,\"passed\":false}",
      "correctAnswer": "",
      "baseExplanation": "기본 해설...",
      "aiExplanation": "AI 맞춤 해설..."
    }
  ]
}
```

**프론트엔드 동작:**
- 오답 문제 목록을 화면에 표시
- 각 문제의 본문, 사용자 답안, 해설(AI 해설 포함) 표시
- 사용자가 모든 오답을 확인할 수 있도록 UI 제공

**단계 2: 오답 정리 완료 처리**

사용자가 오답을 모두 확인한 후 "다음" 버튼 클릭 시:

```http
POST /api/study/session/advance
Content-Type: application/json

{
  "sessionId": {learningSessionId},    // ⚠️ LearningSession ID!
  "step": "REVIEW_WRONG",              // ⚠️ "PRACTICAL_REVIEW_WRONG"이 아님!
  "score": null,
  "detailsJson": null
}
```

**⚠️ 중요 주의사항:**
- `sessionId`는 **LearningSession ID**입니다 (StudySession ID가 아님)
- `step`은 **`"REVIEW_WRONG"`**을 사용해야 합니다
  - 제출 응답의 `nextStep: "PRACTICAL_REVIEW_WRONG"`과는 다름!
  - 실제 단계 코드는 `"REVIEW_WRONG"`입니다

**응답:**
```json
{
  "sessionId": 123,
  "status": "SUMMARY",        // 다음 단계
  "movedTo": "IN_PROGRESS"
}
```

**단계 3: 요약 화면 표시**

```http
GET /api/study/practical/summary?topicId={topicId}&sessionId={learningSessionId}
```

**응답:**
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "practicalTotal": 5,
    "practicalPassed": 3,  // 60점 이상 문제 수
    "summary": "AI가 생성한 요약 텍스트...",
    "completed": true
  }
}
```

### 📋 프론트엔드 구현 예시

#### 예시 1: grade-one API를 사용하는 방법 (권장)

```typescript
// 각 문제를 풀 때마다 호출
async function handleQuestionSubmit(
  learningSessionId: number,
  topicId: number,
  questionId: number,
  userText: string,
  totalQuestions: number,
  currentIndex: number
) {
  // 1. 단건 즉시 채점
  const gradeResult = await gradeOneQuestion(learningSessionId, topicId, questionId, userText);
  
  // 2. 점수와 해설 표시
  showGradeResult(gradeResult);
  
  // 3. 마지막 문제인지 확인
  if (currentIndex >= totalQuestions - 1) {
    // 마지막 문제 제출 후 세션 상태 확인
    // grade-one API 호출 시 백엔드에서 자동으로 완료 처리됨
    setTimeout(() => {
      checkSessionStatusAndProceed(learningSessionId, topicId);
    }, 1000); // 완료 처리 완료 대기
  }
}

// 단건 즉시 채점
async function gradeOneQuestion(
  learningSessionId: number,
  topicId: number,
  questionId: number,
  userText: string
) {
  const response = await fetch(
    `/api/study/practical/grade-one?sessionId=${learningSessionId}`,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify({
        topicId,
        questionId,
        userText
      })
    }
  );
  return await response.json(); // { score, baseExplanation, aiExplanation }
}

// 세션 상태 확인 후 다음 단계로 진행
async function checkSessionStatusAndProceed(
  learningSessionId: number,
  topicId: number
) {
  // 1. 세션 상태 조회
  const sessionStatus = await getSessionStatus(learningSessionId);
  
  // 2. PRACTICAL 단계가 COMPLETE인지 확인
  const practicalStep = sessionStatus.steps.find(step => step.step === 'PRACTICAL');
  
  if (practicalStep?.state === 'COMPLETE') {
    // 모든 문제를 다 풀었음
    const wrongQuestionIds = JSON.parse(practicalStep.metadata || '{}').wrongQuestionIds || [];
    
    // 3. 오답이 없으면 바로 요약으로
    if (wrongQuestionIds.length === 0) {
      const summary = await loadSummary(topicId, learningSessionId);
      showSummaryScreen(summary);
      return;
    }
    
    // 4. 오답이 있으면 오답 정리 단계
    await handleWrongReview(learningSessionId, topicId);
  }
}

// 오답 정리 처리
async function handleWrongReview(learningSessionId: number, topicId: number) {
  // 1. 오답 문제 조회
  const wrongQuestions = await loadWrongQuestions(learningSessionId);
  
  // 2. 오답 화면 표시
  showWrongReviewScreen(wrongQuestions, async () => {
    // 사용자가 "다음" 버튼 클릭 시
    
    // 3. 오답 정리 완료 처리
    const advanceResult = await completeWrongReview(learningSessionId);
    
    if (advanceResult.status === 'SUMMARY') {
      // 4. 요약 화면 표시
      const summary = await loadSummary(topicId, learningSessionId);
      showSummaryScreen(summary);
    }
  });
}

// 세션 상태 조회
async function getSessionStatus(learningSessionId: number) {
  const response = await fetch(`/api/study/session/${learningSessionId}`, {
    headers: { 'Authorization': `Bearer ${token}` }
  });
  return await response.json();
}
```

#### 예시 2: 배치 제출을 사용하는 방법 (선택)

```typescript
// 모든 문제를 한 번에 제출
async function handleBatchSubmit(
  learningSessionId: number,
  topicId: number,
  answers: Array<{ questionId: number, userText: string }>
) {
  const response = await fetch(
    `/api/study/practical/submit?sessionId=${learningSessionId}`,
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify({
        topicId,
        answers
      })
    }
  );
  const submitResponse = await response.json();
  
  // 제출 완료 후 처리
  if (submitResponse.status === 'COMPLETE') {
    const wrongQuestionIds = submitResponse.payload.wrongQuestionIds || [];
    
    if (wrongQuestionIds.length === 0) {
      const summary = await loadSummary(topicId, learningSessionId);
      showSummaryScreen(summary);
    } else {
      await handleWrongReview(learningSessionId, topicId);
    }
  }
}

// 오답 문제 조회
async function loadWrongQuestions(learningSessionId: number) {
  const response = await fetch(
    `/api/study/wrong/practical/learning-session?learningSessionId=${learningSessionId}`,
    {
      headers: { 'Authorization': `Bearer ${token}` }
    }
  );
  const data = await response.json();
  return data.items; // WrongRecapSet.Item[]
}

// 오답 정리 완료 처리
async function completeWrongReview(learningSessionId: number) {
  const response = await fetch('/api/study/session/advance', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      sessionId: learningSessionId,  // LearningSession ID
      step: 'REVIEW_WRONG',          // ⚠️ 실제 단계 코드
      score: null,
      detailsJson: null
    })
  });
  return await response.json(); // AdvanceResp
}

// 요약 조회
async function loadSummary(topicId: number, learningSessionId: number) {
  const response = await fetch(
    `/api/study/practical/summary?topicId=${topicId}&sessionId=${learningSessionId}`,
    {
      headers: { 'Authorization': `Bearer ${token}` }
    }
  );
  const data = await response.json();
  return data.payload; // SummaryResp
}
```

### ⚠️ 주요 주의사항

1. **`grade-one` API 사용 필수:**
   - **프론트엔드에서 각 문제를 풀 때마다 반드시 `POST /api/study/practical/grade-one`를 호출해야 합니다**
   - 각 호출마다 답안이 세션에 저장되고, AI 해설이 생성되어 저장됩니다
   - 마지막 문제를 `grade-one`으로 제출하면 자동으로 완료 처리됩니다
   - 모든 문제를 `grade-one`으로 풀었을 때는 별도의 배치 제출(`/submit`) 없이도 완료 처리됩니다

2. **`nextStep` 값과 실제 단계 코드의 차이:**
   - 제출 응답: `nextStep: "PRACTICAL_REVIEW_WRONG"` (표시용)
   - advance API: `step: "REVIEW_WRONG"` (실제 단계 코드)
   - **항상 `"REVIEW_WRONG"`을 사용하세요!**

3. **세션 ID 구분:**
   - `learningSessionId`: LearningSession ID (대부분의 API에서 사용)
   - `sessionId` (StudySession): 오답 조회 API의 방법 2에서만 사용
   - **혼동하지 않도록 주의!**

4. **오답이 없을 때:**
   - 오답 조회 API를 호출할 필요 없음
   - 바로 요약 화면으로 이동 가능

5. **세션 상태 확인:**
   - 정기적으로 `GET /api/study/session/{sessionId}`를 호출하여 현재 단계 확인
   - 특히 페이지 재진입 시 유용
   - `grade-one`으로 모든 문제를 풀었을 때는 세션 상태를 확인하여 자동 완료 여부를 체크하세요

---

## 📞 문의

추가 질문이나 이슈가 있으면 백엔드 담당자에게 문의해주세요!

