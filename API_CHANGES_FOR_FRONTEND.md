# Versus API 변경 사항 (프론트엔드)

## 📋 변경 개요

`GET /api/versus/rooms/{roomId}/scoreboard` 엔드포인트의 응답에 **현재 진행 중인 문제 정보**가 추가되었습니다.

---

## 🔄 변경된 엔드포인트

### `GET /api/versus/rooms/{roomId}/scoreboard`

**변경 사항:**
- ✅ `currentQuestion` 필드 추가 (현재 진행 중인 문제 정보)
- ❌ `currentQuestionElapsedMs` 필드 제거 (경과 시간 계산 제거)

---

## 📝 응답 스키마

### 변경 전
```json
{
  "roomId": 1313,
  "status": "ONGOING",
  "items": [...],
  "currentQuestionElapsedMs": 3500
}
```

### 변경 후
```json
{
  "roomId": 1313,
  "status": "ONGOING",
  "items": [
    {
      "userId": "user1",
      "correctCount": 5,
      "totalCount": 10,
      "score": 5375,
      "totalTimeMs": 56822,
      "rank": 1,
      "alive": true,
      "revived": false
    }
  ],
  "currentQuestion": {
    "questionId": 92,
    "roundNo": 1,
    "phase": "MAIN",
    "orderNo": 1,
    "timeLimitSec": 5,
    "endTime": "2025-12-02T03:15:06Z"
  }
}
```

---

## 📊 필드 설명

### `currentQuestion` (CurrentQuestionInfo)

| 필드 | 타입 | 설명 | 예시 |
|------|------|------|------|
| `questionId` | `Long` | 현재 진행 중인 문제 ID | `92` |
| `roundNo` | `Integer` | 현재 라운드 번호 | `1` |
| `phase` | `String` | 현재 페이즈 (MAIN, FINAL, REVIVAL) | `"MAIN"` |
| `orderNo` | `Integer` | 문제 순서 번호 | `1` |
| `timeLimitSec` | `Integer` | 시간 제한 (초) | `5` |
| `endTime` | `String` (ISO 8601) | 문제 종료 시간 (UTC) | `"2025-12-02T03:15:06Z"` |

**주의사항:**
- `currentQuestion`은 `status`가 `ONGOING`일 때만 `null`이 아닙니다.
- `status`가 `WAIT` 또는 `DONE`이면 `currentQuestion`은 `null`입니다.
- `endTime`은 UTC 기준입니다 (ISO 8601 형식, `Z` 접미사 포함).

---

## 🔄 문제 시작 시점 및 endTime 획득 방법

### 첫 번째 문제
- **시점**: 매치가 시작될 때 (`MATCH_STARTED` 이벤트와 함께)
- **방법**: 
  - 봇전: `POST /api/versus/match/duel/bot` 호출 후 자동으로 매치가 시작되고 첫 번째 문제가 시작됩니다.
  - 일반 매칭: 매칭 완료 후 자동으로 매치가 시작되고 첫 번째 문제가 시작됩니다.
  - 수동 시작: `POST /api/versus/rooms/{roomId}/start` 호출 시 첫 번째 문제가 시작됩니다.
- **endTime 획득**: 매치 시작 후 `GET /api/versus/rooms/{roomId}/scoreboard`를 호출하면 `currentQuestion.endTime`에 첫 번째 문제의 종료 시간이 포함됩니다.

### 두 번째 문제 이후
- **시점**: 이전 문제의 답안 제출 후 (`POST /api/versus/rooms/{roomId}/answers` 호출 후)
- **방법**: 
  - 모든 참가자가 답안을 제출하면 자동으로 다음 문제가 시작됩니다.
  - `POST /api/versus/rooms/{roomId}/answers` 응답의 `status`가 `ONGOING`이면 다음 문제가 시작된 것입니다.
- **endTime 획득**: 답안 제출 후 `GET /api/versus/rooms/{roomId}/scoreboard`를 호출하면 `currentQuestion.endTime`에 새로운 문제의 종료 시간이 포함됩니다.

### 요약
| 문제 번호 | endTime 획득 시점 | 호출 방법 |
|---------|-----------------|----------|
| 1번째 | 매치 시작 후 | `scoreboard` API 호출 |
| 2번째 이후 | 답안 제출 후 | `scoreboard` API 호출 |

---

## 💻 사용 예시

### 1. 현재 문제 정보 확인

```javascript
// 1초마다 scoreboard 호출
const response = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
const scoreboard = await response.json();

if (scoreboard.currentQuestion) {
  const { questionId, roundNo, orderNo, timeLimitSec, endTime } = scoreboard.currentQuestion;
  
  console.log(`현재 문제: ${questionId} (라운드 ${roundNo}, ${orderNo}번째 문제)`);
  console.log(`시간 제한: ${timeLimitSec}초`);
}
```

### 2. 카운트다운 계산

```javascript
const response = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
const scoreboard = await response.json();

if (scoreboard.currentQuestion && scoreboard.currentQuestion.endTime) {
  const endTime = new Date(scoreboard.currentQuestion.endTime); // UTC 기준
  const now = new Date(); // 현재 시간 (UTC 기준으로 자동 변환)
  
  const remainingMs = endTime.getTime() - now.getTime();
  const remainingSeconds = Math.max(0, Math.ceil(remainingMs / 1000));
  
  console.log(`남은 시간: ${remainingSeconds}초`);
  
  // UI 업데이트
  updateCountdown(remainingSeconds);
}
```

### 3. 실시간 카운트다운 (1초마다 업데이트)

```javascript
let countdownInterval;

function startCountdown(roomId) {
  countdownInterval = setInterval(async () => {
    try {
      const response = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
      const scoreboard = await response.json();
      
      if (!scoreboard.currentQuestion || !scoreboard.currentQuestion.endTime) {
        // 문제가 진행 중이 아니면 카운트다운 중지
        clearInterval(countdownInterval);
        return;
      }
      
      const endTime = new Date(scoreboard.currentQuestion.endTime);
      const now = new Date();
      const remainingMs = endTime.getTime() - now.getTime();
      const remainingSeconds = Math.max(0, Math.ceil(remainingMs / 1000));
      
      // UI 업데이트
      document.getElementById('countdown').textContent = `${remainingSeconds}초`;
      
      if (remainingSeconds === 0) {
        clearInterval(countdownInterval);
        // 시간 종료 처리
        handleTimeExpired();
      }
    } catch (error) {
      console.error('카운트다운 업데이트 실패:', error);
    }
  }, 1000); // 1초마다 업데이트
}

// 사용 예시
startCountdown(1313);

// 정리
function stopCountdown() {
  if (countdownInterval) {
    clearInterval(countdownInterval);
  }
}
```

### 4. 첫 번째 문제 시작 감지 및 카운트다운 시작

```javascript
// 봇전 시작 예시
async function startBotBattle(examMode, scopeType, topicId, difficulty) {
  // 1. 봇전 시작
  const startResponse = await fetch('/api/versus/match/duel/bot', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ examMode, scopeType, topicId, difficulty })
  });
  const { roomId } = await startResponse.json();
  
  // 2. 첫 번째 문제 정보 확인 (매치 시작 후 자동으로 첫 번째 문제가 시작됨)
  // 약간의 지연 후 scoreboard 호출 (서버에서 이벤트 기록 시간 고려)
  setTimeout(async () => {
    const scoreboardResponse = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
    const scoreboard = await scoreboardResponse.json();
    
    if (scoreboard.currentQuestion && scoreboard.currentQuestion.endTime) {
      // 첫 번째 문제의 endTime 획득 성공
      console.log('첫 번째 문제 시작:', scoreboard.currentQuestion.questionId);
      startCountdown(roomId);
    } else {
      // 아직 문제가 시작되지 않았으면 재시도
      setTimeout(() => checkFirstQuestion(roomId), 500);
    }
  }, 500);
}

// 일반 매칭 예시
async function waitForMatchAndStartCountdown(roomId) {
  // 매칭 완료 후 자동으로 매치가 시작되므로, scoreboard를 주기적으로 확인
  const checkInterval = setInterval(async () => {
    const scoreboardResponse = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
    const scoreboard = await scoreboardResponse.json();
    
    if (scoreboard.status === 'ONGOING' && scoreboard.currentQuestion) {
      // 매치가 시작되고 첫 번째 문제가 시작됨
      clearInterval(checkInterval);
      console.log('첫 번째 문제 시작:', scoreboard.currentQuestion.questionId);
      startCountdown(roomId);
    }
  }, 1000); // 1초마다 확인
}
```

### 5. 답안 제출 후 다음 문제 감지

### 5. 답안 제출 후 다음 문제 감지

```javascript
async function submitAnswer(roomId, questionId, userAnswer) {
  // 답안 제출
  const response = await fetch(`/api/versus/rooms/${roomId}/answers`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      questionId,
      userAnswer,
      correct: false, // 서버에서 검증하므로 참고용
      timeMs: null   // 서버에서 계산하므로 null 가능
    })
  });
  
  const scoreboard = await response.json();
  
  // 다음 문제가 시작되었는지 확인
  if (scoreboard.currentQuestion) {
    const { questionId: newQuestionId, endTime } = scoreboard.currentQuestion;
    
    if (newQuestionId !== questionId) {
      // 새로운 문제가 시작됨
      console.log('다음 문제 시작:', newQuestionId);
      console.log('종료 시간:', endTime);
      
      // 카운트다운 재시작
      startCountdown(roomId);
    }
  }
}
```

### 6. 현재 문제 표시 (사용자 A와 B 동기화)

```javascript
// 사용자 A와 B 모두 같은 API를 호출하면 동일한 currentQuestion을 받습니다
const response = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
const scoreboard = await response.json();

if (scoreboard.currentQuestion) {
  const { questionId, roundNo, orderNo } = scoreboard.currentQuestion;
  
  // 같은 questionId를 받으므로 같은 문제를 표시
  displayQuestion(questionId);
  
  // 몇 번째 문제인지 표시
  console.log(`라운드 ${roundNo} - ${orderNo}번째 문제`);
}
```

---

## ⚠️ 주의사항

### 1. 시간대 처리
- `endTime`은 **UTC 기준**입니다 (ISO 8601 형식: `2025-12-02T03:15:06Z`).
- JavaScript의 `Date` 객체는 UTC를 자동으로 처리하므로, 별도의 시간대 변환은 필요 없습니다.
- `new Date("2025-12-02T03:15:06Z")`는 UTC로 파싱되며, `getTime()`으로 밀리초를 얻을 수 있습니다.

### 2. null 체크
- `currentQuestion`이 `null`일 수 있습니다:
  - `status`가 `WAIT` (대기 중)
  - `status`가 `DONE` (종료)
  - 문제 시작 이벤트가 아직 발생하지 않음

### 3. 카운트다운 정확도
- 서버 시간과 클라이언트 시간이 완전히 동기화되어 있지 않을 수 있습니다.
- 하지만 UTC 기준으로 계산하므로 시간대 차이는 발생하지 않습니다.
- 클라이언트의 시스템 시간이 잘못 설정되어 있으면 오차가 발생할 수 있습니다.

### 4. 폴링 주기
- 권장: 1초마다 `scoreboard` API 호출
- 너무 자주 호출하면 서버 부하가 발생할 수 있으므로, 적절한 주기를 유지하세요.

---

## 🔍 관련 엔드포인트

### 문제 상세 정보 조회
현재 문제의 상세 정보(문제 내용, 선택지 등)를 보려면:

```javascript
// currentQuestion.questionId를 사용하여 문제 상세 정보 조회
const questionId = scoreboard.currentQuestion.questionId;
const questionResponse = await fetch(`/api/study/versus/questions/${questionId}`);
const question = await questionResponse.json();

// question.stem, question.payloadJson.choices 등을 사용
```

---

## 📌 마이그레이션 가이드

### 기존 코드 (변경 전)
```javascript
// ❌ 제거된 필드 사용
const elapsedMs = scoreboard.currentQuestionElapsedMs;
const remainingSeconds = Math.ceil((timeLimitSec * 1000 - elapsedMs) / 1000);
```

### 새로운 코드 (변경 후)
```javascript
// ✅ endTime 사용
const endTime = new Date(scoreboard.currentQuestion.endTime);
const now = new Date();
const remainingSeconds = Math.max(0, Math.ceil((endTime.getTime() - now.getTime()) / 1000));
```

---

## 📞 문의

추가 질문이나 문제가 있으면 백엔드 팀에 문의해주세요.

