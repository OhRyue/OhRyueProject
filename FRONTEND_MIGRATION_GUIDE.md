# 실기 주관식 채점 체계 변경 가이드

## 📋 변경 개요


### 변경 일자
2024년 (현재 날짜)

### 영향 범위
- 실기 주관식 문제 채점 API
- 실기 요약 API
- 보조학습(Assist) 실기 API

---

## 🔄 주요 변경 사항

### 1. 채점 결과 필드 변경

#### 이전 (점수 기반)
```typescript
{
  score: 85,  // 0~100
  baseExplanation: "...",
  aiExplanation: "..."
}
```

#### 변경 후 (맞음/틀림 기반)
```typescript
{
  correct: true,  // true=맞음, false=틀림
  baseExplanation: "...",
  aiExplanation: "..."
}
```

### 2. 평균 점수 필드 제거

`avgScore` 필드가 모든 응답에서 제거되었습니다. 대신 `correct` (맞은 문제 수) 필드를 사용합니다.

---

## 📡 API별 변경 상세

### 1. 실기 단건 즉시 채점

**엔드포인트:** `POST /api/study/practical/grade-one?sessionId={learningSessionId}`

#### 이전 응답
```json
{
  "score": 85,
  "baseExplanation": "기본 해설...",
  "aiExplanation": "AI 해설..."
}
```

#### 변경 후 응답
```json
{
  "correct": true,
  "baseExplanation": "기본 해설...",
  "aiExplanation": "AI 해설..."
}
```

#### TypeScript 타입 변경
```typescript
// 이전
interface PracticalGradeOneResp {
  score: number;  // 0~100
  baseExplanation: string;
  aiExplanation: string;
}

// 변경 후
interface PracticalGradeOneResp {
  correct: boolean;  // true=맞음, false=틀림
  baseExplanation: string;
  aiExplanation: string;
}
```

---

### 2. 실기 제출 (배치)

**엔드포인트:** `POST /api/study/practical/submit?sessionId={learningSessionId}`

#### 이전 응답
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "total": 5,
    "avgScore": 75,
    "items": [
      {
        "questionId": 10,
        "score": 90,
        "baseExplanation": "...",
        "aiExplanation": "..."
      }
    ],
    "wrongQuestionIds": [12, 13]
  }
}
```

#### 변경 후 응답
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "total": 5,
    "correct": 3,  // 맞은 문제 수 
    "items": [
      {
        "questionId": 10,
        "correct": true,  // score → correct
        "baseExplanation": "...",
        "aiExplanation": "..."
      }
    ],
    "wrongQuestionIds": [12, 13]  // correct=false인 문제 ID
  }
}
```

#### TypeScript 타입 변경
```typescript
// 이전
interface PracticalSubmitItem {
  questionId: number;
  score: number;  // 0~100
  baseExplanation: string;
  aiExplanation: string;
}

interface PracticalSubmitResp {
  total: number;
  avgScore: number;  // 평균 점수
  items: PracticalSubmitItem[];
  wrongQuestionIds: number[];  // score < 60인 문제
}

// 변경 후
interface PracticalSubmitItem {
  questionId: number;
  correct: boolean;  // true=맞음, false=틀림
  baseExplanation: string;
  aiExplanation: string;
}

interface PracticalSubmitResp {
  total: number;
  correct: number;  // 맞은 문제 수 (avgScore 제거)
  items: PracticalSubmitItem[];
  wrongQuestionIds: number[];  // correct=false인 문제
}
```

---

### 3. 실기 요약

**엔드포인트:** `GET /api/study/practical/summary?topicId={topicId}&sessionId={learningSessionId}`

#### 이전 응답
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 4,
    "summary": "AI 요약...",
    "completed": true
  }
}
```

#### 변경 후 응답
```json
{
  "sessionId": 123,
  "status": "COMPLETE",
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 4,  // 맞은 문제 수 (실기 주관식)
    "summary": "AI 요약...",
    "completed": true
  }
}
```

#### TypeScript 타입 변경
```typescript
// 이전
interface SummaryResp {
  miniTotal: number;
  miniCorrect: number;
  miniPassed: boolean;
  mcqTotal: number;
  mcqCorrect: number;
  avgScore: number;  // 평균 점수
  aiSummary: string;
  completed: boolean;
}

// 변경 후
interface SummaryResp {
  miniTotal: number;
  miniCorrect: number;
  miniPassed: boolean;
  mcqTotal: number;
  mcqCorrect: number;  // 맞은 문제 수
  aiSummary: string;
  completed: boolean;
  // avgScore 필드 제거됨
}
```

---

### 4. 실기 리뷰 제출

**엔드포인트:** `POST /api/study/practical/review/submit`

#### 변경 사항
- `avgScore` → `correct` (맞은 문제 수)
- `score` → `correct` (boolean)

응답 구조는 실기 제출과 동일하게 변경되었습니다.

---

### 5. 보조학습(Assist) 실기 제출

**엔드포인트:** `POST /api/study/assist/practical/submit`

#### 변경 사항
- `avgScore` → `correct` (맞은 문제 수)
- `score` → `correct` (boolean)

---

## 🔧 마이그레이션 가이드

### 1. 점수 표시 로직 변경

#### 이전 코드
```typescript
// 점수 표시
if (item.score >= 60) {
  // 통과
} else {
  // 오답
}

// 평균 점수 표시
const avgScore = resp.avgScore;
console.log(`평균 점수: ${avgScore}점`);
```

#### 변경 후 코드
```typescript
// 맞음/틀림 판단
if (item.correct) {
  // 맞음
} else {
  // 틀림
}

// 정확도 계산 (필요한 경우)
const accuracy = resp.total > 0 
  ? Math.round((resp.correct / resp.total) * 100) 
  : 0;
console.log(`정확도: ${accuracy}%`);
```

### 2. UI 컴포넌트 수정

#### 점수 표시 → 맞음/틀림 표시
```typescript
// 이전
<div>점수: {item.score}점</div>
{item.score >= 60 ? '통과' : '오답'}

// 변경 후
<div>{item.correct ? '맞음 ✓' : '틀림 ✗'}</div>
```

#### 평균 점수 표시 → 정확도 표시
```typescript
// 이전
<div>평균 점수: {resp.avgScore}점</div>

// 변경 후
<div>
  정확도: {Math.round((resp.correct / resp.total) * 100)}%
  ({resp.correct}/{resp.total})
</div>
```

### 3. 타입 정의 업데이트

프로젝트의 모든 TypeScript 타입 정의를 위의 변경 사항에 맞게 업데이트하세요.

---

## ⚠️ 주의 사항

### 1. 하위 호환성
- 기존 API 응답과 호환되지 않습니다.
- **즉시 업데이트가 필요합니다.**

### 2. 오답 판단 기준
- **이전:** `score < 60` → 오답
- **변경 후:** `correct === false` → 오답

### 3. 통계 계산
- 평균 점수 대신 정확도(퍼센트)를 계산해야 합니다.
- 공식: `정확도 = (맞은 문제 수 / 전체 문제 수) * 100`

---

## 📝 체크리스트

프론트엔드 마이그레이션 시 다음 항목을 확인하세요:

- [ ] `PracticalGradeOneResp` 타입의 `score` → `correct` 변경
- [ ] `PracticalSubmitItem` 타입의 `score` → `correct` 변경
- [ ] `PracticalSubmitResp` 타입의 `avgScore` → `correct` 변경
- [ ] `SummaryResp` 타입의 `avgScore` 필드 제거
- [ ] 점수 표시 UI → 맞음/틀림 표시 UI 변경
- [ ] 평균 점수 표시 → 정확도 표시 변경
- [ ] 오답 판단 로직 변경 (`score < 60` → `correct === false`)
- [ ] 통계 계산 로직 변경 (평균 점수 → 정확도)

---

## 📞 문의

변경 사항에 대한 문의사항이 있으시면 백엔드 팀에 연락해주세요.

---

## 📚 참고

### 변경된 API 엔드포인트 목록
1. `POST /api/study/practical/grade-one`
2. `POST /api/study/practical/submit`
3. `GET /api/study/practical/summary`
4. `POST /api/study/practical/review/submit`
5. `POST /api/study/assist/practical/submit`

### 변경되지 않은 API
- 필기 관련 API는 변경되지 않았습니다.
- 실기 미니체크(OX) API는 변경되지 않았습니다.

