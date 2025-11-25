# 오답 정리 → 최종 결과 프론트엔드 흐름

## 개요
MCQ 제출 완료 후, 오답 문제를 확인하고 최종 결과 화면으로 넘어가는 프론트엔드 흐름을 정리합니다.

---

## 전체 흐름 순서

```
MCQ 제출 완료 
  ↓
1. 세션 상태 확인 (REVIEW_WRONG 단계로 자동 전환됨)
  ↓
2. 오답 문제 조회
  ↓
3. 오답 문제 화면에서 문제들을 확인
  ↓
4. 오답 정리 완료 처리 (advance API 호출)
  ↓
5. 최종 결과(요약) 화면 표시
```

---

## 1. MCQ 제출 완료 후 세션 상태 확인

MCQ 제출 후 응답에서 `nextStep`이 `"REVIEW_WRONG"`으로 변경되었는지 확인합니다.

**이전 단계**: `POST /api/study/written/mcq/submit?sessionId={sessionId}`

**응답 예시**:
```json
{
  "sessionId": 6,
  "mode": "MICRO",
  "step": "MCQ",
  "status": "COMPLETE",
  "nextStep": "REVIEW_WRONG",  // ⭐ 오답 정리 단계로 이동
  "learningSessionId": 4
}
```

**프론트엔드 동작**:
- `nextStep === "REVIEW_WRONG"`이면 오답 정리 화면으로 이동
- `learningSessionId` 저장 (오답 조회 API에 사용)

---

## 2. 오답 문제 조회

**API**: `GET /api/study/wrong/written/learning-session`

**Query Parameters**:
- `learningSessionId` (Long, 필수): LearningSession ID

**Headers**:
```
Authorization: Bearer {jwt-token}
```

**응답 구조**:
```json
{
  "items": [
    {
      "questionId": 202,
      "type": "MCQ",
      "text": "문제 본문...",
      "myAnswer": "B",
      "correctAnswer": "C",
      "baseExplanation": "오답입니다. 정답은 C입니다...",
      "imageUrl": "https://..." // 선택
    },
    {
      "questionId": 205,
      "type": "MCQ",
      "text": "문제 본문...",
      "myAnswer": "A",
      "correctAnswer": "B",
      "baseExplanation": "오답입니다. 정답은 B입니다...",
      "imageUrl": null
    }
  ]
}
```

**프론트엔드 동작**:
- 오답 문제 목록을 화면에 표시
- 각 문제의 본문, 사용자 답안, 정답, 해설을 표시
- 사용자가 모든 오답 문제를 확인할 수 있도록 UI 제공

**예시 코드**:
```javascript
// 오답 문제 조회
const response = await fetch(
  `http://localhost:3000/api/study/wrong/written/learning-session?learningSessionId=${learningSessionId}`,
  {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`
    }
  }
);

const wrongRecapData = await response.json();
// wrongRecapData.items 배열에 오답 문제들이 포함됨
```

---

## 3. 오답 정리 완료 처리

사용자가 모든 오답 문제를 확인한 후, 다음 단계(SUMMARY)로 진행하기 위해 advance API를 호출합니다.

**API**: `POST /api/study/session/advance`

**Request Body**:
```json
{
  "sessionId": 4,              // LearningSession ID
  "step": "REVIEW_WRONG",      // 완료할 단계 코드
  "score": null,               // 선택 (오답 정리 단계는 점수 없음)
  "detailsJson": null          // 선택 (추가 메타데이터)
}
```

**Headers**:
```
Content-Type: application/json
Authorization: Bearer {jwt-token}
```

**응답 구조**:
```json
{
  "sessionId": 4,
  "status": "SUMMARY",         // 다음 단계 코드
  "movedTo": "IN_PROGRESS"     // LearningSession 상태
}
```

**프론트엔드 동작**:
- 사용자가 "다음" 또는 "결과 보기" 버튼 클릭 시 호출
- 응답의 `status === "SUMMARY"`이면 최종 결과 화면으로 이동

**예시 코드**:
```javascript
// 오답 정리 완료 처리
const advanceResponse = await fetch(
  'http://localhost:3000/api/study/session/advance',
  {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json',
      'Authorization': `Bearer ${token}`
    },
    body: JSON.stringify({
      sessionId: learningSessionId,
      step: 'REVIEW_WRONG',
      score: null,
      detailsJson: null
    })
  }
);

const advanceData = await advanceResponse.json();
if (advanceData.status === 'SUMMARY') {
  // 최종 결과 화면으로 이동
  navigateToSummary();
}
```

---

## 4. 최종 결과(요약) 조회

**API**: `GET /api/study/written/summary`

**Query Parameters**:
- `topicId` (Long, 필수): 토픽 ID
- `sessionId` (Long, 필수): **LearningSession ID** (주의: StudySession ID가 아님)

**Headers**:
```
Authorization: Bearer {jwt-token}
```

**응답 구조**:
```json
{
  "sessionId": 6,
  "mode": "MICRO",
  "step": "MICRO_SUMMARY",
  "status": "COMPLETE",
  "nextStep": null,
  "meta": {
    "miniTotal": 4,
    "miniCorrect": 2,
    "mcqTotal": 5,
    "mcqCorrect": 3
  },
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 2,
    "miniPassed": false,
    "mcqTotal": 5,
    "mcqCorrect": 3,
    "aiSummary": "이번 학습에서 총 9문제를 풀었고, 5문제를 맞췄습니다. 특히...",
    "completed": false
  },
  "learningSessionId": 4
}
```

**Response 필드 설명**:
- `payload.miniTotal`: 미니체크 총 풀이 수
- `payload.miniCorrect`: 미니체크 정답 수
- `payload.miniPassed`: 미니체크 전부 정답 여부
- `payload.mcqTotal`: MCQ 총 풀이 수
- `payload.mcqCorrect`: MCQ 정답 수
- `payload.aiSummary`: AI가 생성한 학습 요약 (폴백 포함)
- `payload.completed`: 완료 여부 (최소 시도)

**프론트엔드 동작**:
- 최종 결과 화면에 요약 정보 표시
- 미니체크/MCQ 통계 표시
- AI 요약 텍스트 표시
- 학습 완료 여부 표시

**예시 코드**:
```javascript
// 최종 결과 조회
const summaryResponse = await fetch(
  `http://localhost:3000/api/study/written/summary?topicId=${topicId}&sessionId=${learningSessionId}`,
  {
    method: 'GET',
    headers: {
      'Authorization': `Bearer ${token}`
    }
  }
);

const summaryData = await summaryResponse.json();
// summaryData.payload에 요약 정보가 포함됨
```

---

## 전체 흐름 예시 코드

```javascript
// 1. MCQ 제출 완료 후 (이미 완료된 상태)
const mcqSubmitResponse = await submitMcq(/* ... */);
const learningSessionId = mcqSubmitResponse.learningSessionId;

// 2. 오답 문제 조회
async function loadWrongQuestions(learningSessionId) {
  const response = await fetch(
    `http://localhost:3000/api/study/wrong/written/learning-session?learningSessionId=${learningSessionId}`,
    {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`
      }
    }
  );
  const data = await response.json();
  return data.items; // 오답 문제 배열
}

// 3. 오답 정리 완료 처리
async function completeWrongReview(learningSessionId) {
  const response = await fetch(
    'http://localhost:3000/api/study/session/advance',
    {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'Authorization': `Bearer ${token}`
      },
      body: JSON.stringify({
        sessionId: learningSessionId,
        step: 'REVIEW_WRONG',
        score: null,
        detailsJson: null
      })
    }
  );
  const data = await response.json();
  return data; // { sessionId, status: "SUMMARY", movedTo }
}

// 4. 최종 결과 조회
async function loadSummary(topicId, learningSessionId) {
  const response = await fetch(
    `http://localhost:3000/api/study/written/summary?topicId=${topicId}&sessionId=${learningSessionId}`,
    {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`
      }
    }
  );
  const data = await response.json();
  return data.payload; // SummaryResp
}

// 전체 흐름 실행
async function handleWrongReviewFlow(learningSessionId, topicId) {
  // 1. 오답 문제 로드
  const wrongQuestions = await loadWrongQuestions(learningSessionId);
  
  if (wrongQuestions.length === 0) {
    // 오답이 없으면 바로 요약 화면으로
    const summary = await loadSummary(topicId, learningSessionId);
    showSummaryScreen(summary);
    return;
  }
  
  // 2. 오답 문제 화면 표시
  showWrongReviewScreen(wrongQuestions, async () => {
    // 사용자가 "다음" 버튼 클릭 시
    
    // 3. 오답 정리 완료 처리
    const advanceResult = await completeWrongReview(learningSessionId);
    
    if (advanceResult.status === 'SUMMARY') {
      // 4. 최종 결과 조회 및 표시
      const summary = await loadSummary(topicId, learningSessionId);
      showSummaryScreen(summary);
    }
  });
}
```

---

## 주의사항

1. **sessionId 혼동 주의**:
   - `POST /api/study/session/advance`: `sessionId`는 **LearningSession ID**
   - `GET /api/study/written/summary`: `sessionId` 파라미터는 **LearningSession ID**
   - `GET /api/study/wrong/written/learning-session`: `learningSessionId` 파라미터 사용

2. **오답이 없는 경우**:
   - `wrongRecapData.items.length === 0`이면 오답 화면을 건너뛰고 바로 요약 화면으로 이동 가능

3. **에러 처리**:
   - 각 API 호출 시 에러 처리 필요
   - 401 Unauthorized: JWT 토큰 재발급
   - 404 Not Found: 세션이 없는 경우

4. **로딩 상태**:
   - 각 API 호출 중 로딩 상태 표시 권장

---

## API 요약

| 단계 | API | Method | 파라미터 | 설명 |
|------|-----|--------|----------|------|
| 1. 오답 조회 | `/api/study/wrong/written/learning-session` | GET | `learningSessionId` | MCQ 오답 문제 목록 조회 |
| 2. 오답 완료 | `/api/study/session/advance` | POST | `sessionId`, `step: "REVIEW_WRONG"` | REVIEW_WRONG 단계 완료 처리 |
| 3. 최종 결과 | `/api/study/written/summary` | GET | `topicId`, `sessionId` | 학습 요약 정보 조회 |

---

이제 프론트엔드에서 이 흐름을 구현할 수 있습니다!

