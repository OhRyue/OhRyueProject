# `/api/study/written/summary` API 응답 예시

## API 정보

**Endpoint**: `GET /api/study/written/summary`

**Query Parameters**:
- `topicId` (Long, 필수): 토픽 ID
- `sessionId` (Long, 필수): LearningSession ID

**Headers**:
```
Authorization: Bearer {jwt-token}
```

---

## 응답 구조

```typescript
{
  sessionId: number;                    // StudySession ID
  mode: string;                         // "MICRO"
  step: string;                         // "MICRO_SUMMARY"
  status: string;                       // "COMPLETE" | "IN_PROGRESS"
  nextStep: string | null;              // null (마지막 단계)
  meta: Record<string, any> | null;     // 세션 메타 정보
  payload: {
    miniTotal: number;                  // 미니체크 총 풀이 수
    miniCorrect: number;                // 미니체크 정답 수
    miniPassed: boolean;                // 미니체크 전부 정답 여부
    mcqTotal: number;                   // MCQ 총 풀이 수
    mcqCorrect: number;                 // MCQ 정답 수
    aiSummary: string;                  // AI 학습 요약 (폴백 포함)
    completed: boolean;                 // 완료 여부 (miniPassed && mcqCompleted)
  };
  learningSessionId: number;            // LearningSession ID
}
```

---

## 예시 1: 완전히 완료한 경우 (모두 정답)

```json
{
  "sessionId": 6,
  "mode": "MICRO",
  "step": "MICRO_SUMMARY",
  "status": "COMPLETE",
  "nextStep": null,
  "meta": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 5,
    "finalScorePct": 100
  },
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 5,
    "aiSummary": "축하합니다! 이번 학습에서 총 9문제를 모두 맞추셨습니다. '정보처리기사 필기 - 데이터베이스' 토픽의 핵심 개념을 잘 이해하고 계시는 것으로 보입니다. 특히 데이터베이스 설계와 정규화 관련 문제들을 완벽하게 해결하셨네요. 계속해서 이렇게 꾸준히 학습하시면 좋은 결과를 얻으실 수 있을 것입니다.",
    "completed": true
  },
  "learningSessionId": 4
}
```

---

## 예시 2: 일부 오답이 있는 경우

```json
{
  "sessionId": 7,
  "mode": "MICRO",
  "step": "MICRO_SUMMARY",
  "status": "COMPLETE",
  "nextStep": null,
  "meta": {
    "miniTotal": 4,
    "miniCorrect": 3,
    "miniPassed": false,
    "mcqTotal": 5,
    "mcqCorrect": 4,
    "finalScorePct": 78
  },
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 3,
    "miniPassed": false,
    "mcqTotal": 5,
    "mcqCorrect": 4,
    "aiSummary": "이번 학습에서 총 9문제 중 7문제를 맞추셨습니다 (정답률 78%). '정보처리기사 필기 - 네트워크' 토픽에서 특히 OSI 7계층 모델과 TCP/IP 프로토콜 관련 내용을 다시 한 번 확인해보시면 좋을 것 같습니다. 네트워크 계층의 역할과 프로토콜 헤더 구조에 대해 집중적으로 학습하시길 권장합니다.",
    "completed": false
  },
  "learningSessionId": 5
}
```

---

## 예시 3: 오답이 많은 경우

```json
{
  "sessionId": 8,
  "mode": "MICRO",
  "step": "MICRO_SUMMARY",
  "status": "IN_PROGRESS",
  "nextStep": null,
  "meta": {
    "miniTotal": 4,
    "miniCorrect": 1,
    "miniPassed": false,
    "mcqTotal": 5,
    "mcqCorrect": 2,
    "finalScorePct": 33
  },
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 1,
    "miniPassed": false,
    "mcqTotal": 5,
    "mcqCorrect": 2,
    "aiSummary": "이번 학습에서 총 9문제 중 3문제를 맞추셨습니다 (정답률 33%). '정보처리기사 필기 - 운영체제' 토픽의 기본 개념을 다시 학습하시는 것을 권장합니다. 특히 프로세스 관리, 메모리 관리, 파일 시스템 관련 내용을 중심으로 개념을 정리하시고, 각 개념 간의 관계를 명확히 이해하시면 도움이 될 것입니다. 개념 보기 단계를 다시 확인해보시고, 관련 문제를 여러 번 연습해보시길 바랍니다.",
    "completed": false
  },
  "learningSessionId": 6
}
```

---

## 예시 4: 미니체크만 완료, MCQ 일부만 풀은 경우

```json
{
  "sessionId": 9,
  "mode": "MICRO",
  "step": "MICRO_SUMMARY",
  "status": "IN_PROGRESS",
  "nextStep": null,
  "meta": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 3
  },
  "payload": {
    "miniTotal": 4,
    "miniCorrect": 4,
    "miniPassed": true,
    "mcqTotal": 5,
    "mcqCorrect": 3,
    "aiSummary": "미니체크는 모두 맞추셨습니다! 하지만 MCQ에서는 일부 오답이 있습니다. 총 9문제 중 7문제를 맞추셨네요 (정답률 78%). '정보처리기사 필기 - 소프트웨어공학' 토픽에서 요구사항 분석과 설계 단계의 차이, 그리고 UML 다이어그램의 종류와 용도에 대해 다시 한 번 학습해보시면 도움이 될 것 같습니다.",
    "completed": false
  },
  "learningSessionId": 7
}
```

---

## 필드 상세 설명

### `sessionId` (Long)
- StudySession ID
- 해당 세션의 StudySession 테이블의 ID

### `mode` (String)
- 항상 `"MICRO"` (필기 학습 모드)

### `step` (String)
- 항상 `"MICRO_SUMMARY"` (요약 단계)

### `status` (String)
- `"COMPLETE"`: 학습 완료
- `"IN_PROGRESS"`: 학습 진행 중

### `nextStep` (String | null)
- 항상 `null` (마지막 단계이므로 다음 단계 없음)

### `meta` (Map | null)
- 세션의 메타데이터
- `miniTotal`, `miniCorrect`, `miniPassed` 등 포함
- `finalScorePct`: 최종 점수 퍼센트

### `payload` (SummaryResp)
- **`miniTotal`**: 미니체크 총 풀이 수 (항상 4)
- **`miniCorrect`**: 미니체크 정답 수 (0~4)
- **`miniPassed`**: 미니체크 전부 정답 여부 (miniCorrect === 4)
- **`mcqTotal`**: MCQ 총 풀이 수 (항상 5)
- **`mcqCorrect`**: MCQ 정답 수 (0~5)
- **`aiSummary`**: AI가 생성한 학습 요약 텍스트
  - 정답률, 약점 분석, 개선 방향 등을 포함
  - AI 생성 실패 시 폴백 메시지 포함
- **`completed`**: 완료 여부
  - `miniPassed === true && mcqCompleted === true` 이면 `true`
  - 그 외에는 `false`

### `learningSessionId` (Long)
- LearningSession ID
- 세션 관련 다른 API 호출 시 사용

---

## 프론트엔드 사용 예시

### JavaScript/TypeScript

```typescript
interface SummaryResponse {
  sessionId: number;
  mode: string;
  step: string;
  status: string;
  nextStep: string | null;
  meta: Record<string, any> | null;
  payload: {
    miniTotal: number;
    miniCorrect: number;
    miniPassed: boolean;
    mcqTotal: number;
    mcqCorrect: number;
    aiSummary: string;
    completed: boolean;
  };
  learningSessionId: number;
}

async function fetchSummary(topicId: number, learningSessionId: number): Promise<SummaryResponse> {
  const response = await fetch(
    `http://localhost:3000/api/study/written/summary?topicId=${topicId}&sessionId=${learningSessionId}`,
    {
      method: 'GET',
      headers: {
        'Authorization': `Bearer ${token}`
      }
    }
  );
  
  if (!response.ok) {
    throw new Error(`HTTP error! status: ${response.status}`);
  }
  
  return await response.json();
}

// 사용 예시
const summaryData = await fetchSummary(11101, 4);

console.log(`총 정답률: ${((summaryData.payload.miniCorrect + summaryData.payload.mcqCorrect) / 9 * 100).toFixed(1)}%`);
console.log(`완료 여부: ${summaryData.payload.completed ? '완료' : '미완료'}`);
console.log(`AI 요약: ${summaryData.payload.aiSummary}`);
```

---

## 에러 응답

### 400 Bad Request
```json
{
  "message": "토픽이 일치하지 않습니다."
}
```

### 401 Unauthorized
- JWT 토큰이 없거나 유효하지 않음

### 404 Not Found
```json
{
  "message": "session not found"
}
```

---

## 주의사항

1. **sessionId 파라미터는 LearningSession ID**
   - URL의 `sessionId` 파라미터는 **LearningSession ID**를 의미합니다
   - 응답의 `sessionId` 필드는 **StudySession ID**를 의미합니다
   - 혼동하지 않도록 주의하세요

2. **completed 필드**
   - `payload.completed === true` 이면 학습 완료
   - `miniPassed === true && mcqCompleted === true` 일 때만 `true`

3. **aiSummary**
   - AI가 생성한 요약 텍스트
   - 정답률, 약점 분석, 개선 방향 등 포함
   - AI 서비스 실패 시에도 폴백 메시지가 포함됨

4. **meta 필드**
   - `null`일 수 있음
   - 있으면 세션의 추가 메타데이터 포함

