# 문제 간 쉬는 시간(Intermission) 처리 가이드

## 📋 개요

문제 간 5초 쉬는 시간이 추가되었습니다. 프론트엔드에서는 **Scoreboard API**의 `intermission` 필드를 확인하여 쉬는 시간 UI를 표시할 수 있습니다.

**권장 방법**: Scoreboard API 사용 (타임라인 API 호출 불필요)

---

## 🔄 이벤트 흐름

### 문제 완료 후 흐름

1. **모든 참가자가 답안 제출** 또는 **시간 제한 경과**
2. `INTERMISSION_STARTED` 이벤트 발생 (쉬는 시간 시작)
3. **5초 대기**
4. `QUESTION_STARTED` 이벤트 발생 (다음 문제 시작)

---

## 📡 API 응답 구조

### Scoreboard API 응답 (권장)

`GET /api/versus/rooms/{roomId}/scoreboard` 응답에 `intermission` 필드가 포함됩니다.

```json
{
  "roomId": 1354,
  "status": "ONGOING",
  "items": [...],
  "currentQuestion": null,
  "intermission": {
    "nextQuestionId": 205,
    "nextRoundNo": 4,
    "nextPhase": "FINAL",
    "durationSec": 5,
    "startedAt": "2025-12-04T10:26:03.837006519Z",
    "questionStartAt": "2025-12-04T10:26:08.837006519Z"
  }
}
```

**주의사항:**
- `intermission`은 쉬는 시간 중일 때만 `null`이 아닙니다
- `currentQuestion`이 `null`이고 `intermission`이 있으면 쉬는 시간 중입니다
- `currentQuestion`이 있으면 문제 진행 중입니다

---

### `INTERMISSION_STARTED` 이벤트 (타임라인 API)

타임라인 API (`GET /api/versus/rooms/{roomId}/timeline`)에서도 확인할 수 있습니다.

```json
{
  "eventType": "INTERMISSION_STARTED",
  "createdAt": "2025-12-04T10:26:03.837006519Z",
  "payload": {
    "nextQuestionId": 205,
    "nextRoundNo": 4,
    "nextPhase": "FINAL",
    "durationSec": 5,
    "startedAt": "2025-12-04T10:26:03.837006519Z",
    "questionStartAt": "2025-12-04T10:26:08.837006519Z"
  }
}
```

### 필드 설명

| 필드 | 타입 | 설명 |
|------|------|------|
| `nextQuestionId` | `Long` | 다음 문제 ID |
| `nextRoundNo` | `Integer` | 다음 라운드 번호 |
| `nextPhase` | `String` | 다음 페이즈 (MAIN, FINAL, REVIVAL) |
| `durationSec` | `Integer` | 쉬는 시간 (초) - 항상 5초 |
| `startedAt` | `String` (ISO 8601) | 쉬는 시간 시작 시간 (UTC) |
| `questionStartAt` | `String` (ISO 8601) | 다음 문제 시작 시간 (UTC) |

---

## 💻 프론트엔드 구현 방법

### 방법 1: Scoreboard API 사용 (권장) ⭐

**가장 간단한 방법**: Scoreboard API 응답에 `intermission` 필드가 포함되어 있습니다.

```javascript
class VersusGame {
  constructor(roomId) {
    this.roomId = roomId;
    this.intermissionInterval = null;
    this.currentIntermission = null;
    this.scoreboardPollingInterval = null;
  }

  start() {
    // Scoreboard 폴링 시작 (1초마다)
    this.scoreboardPollingInterval = setInterval(() => {
      this.checkScoreboard();
    }, 1000);
  }

  stop() {
    if (this.scoreboardPollingInterval) {
      clearInterval(this.scoreboardPollingInterval);
    }
    this.clearIntermission();
  }

  async checkScoreboard() {
    try {
      const response = await fetch(`/api/versus/rooms/${this.roomId}/scoreboard`);
      const scoreboard = await response.json();
      
      // 쉬는 시간 정보 확인
      if (scoreboard.intermission) {
        this.handleIntermission(scoreboard.intermission);
      } else {
        // 쉬는 시간이 아니면 종료
        this.clearIntermission();
        
        // 현재 문제가 있으면 표시
        if (scoreboard.currentQuestion) {
          this.displayQuestion(scoreboard.currentQuestion);
        }
      }
    } catch (error) {
      console.error('Scoreboard 확인 실패:', error);
    }
  }

  handleIntermission(intermission) {
    const questionStartAt = new Date(intermission.questionStartAt);
    
    // 중복 처리 방지
    if (this.currentIntermission?.questionStartAt?.getTime() === questionStartAt.getTime()) {
      return;
    }

    this.currentIntermission = {
      nextQuestionId: intermission.nextQuestionId,
      nextRoundNo: intermission.nextRoundNo,
      nextPhase: intermission.nextPhase,
      questionStartAt: questionStartAt,
      durationSec: intermission.durationSec
    };

    this.showIntermissionUI();
    this.startIntermissionCountdown();
  }

  startIntermissionCountdown() {
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
    }

    this.intermissionInterval = setInterval(() => {
      if (!this.currentIntermission) {
        clearInterval(this.intermissionInterval);
        return;
      }

      const now = new Date();
      const questionStartAt = this.currentIntermission.questionStartAt;
      const remainingMs = questionStartAt.getTime() - now.getTime();
      const remainingSeconds = Math.max(0, Math.ceil(remainingMs / 1000));

      this.updateIntermissionCountdown(remainingSeconds);

      if (remainingMs <= 100) { // 0.1초 여유
        clearInterval(this.intermissionInterval);
        this.clearIntermission();
      }
    }, 100);
  }

  showIntermissionUI() {
    const { nextRoundNo, nextPhase } = this.currentIntermission;
    
    const overlay = document.getElementById('intermission-overlay');
    if (overlay) {
      overlay.style.display = 'flex';
      overlay.innerHTML = `
        <div class="intermission-content">
          <h2>잠시만 기다려주세요</h2>
          <p>다음 문제 준비 중...</p>
          <p>라운드 ${nextRoundNo} - ${nextPhase}</p>
          <div class="countdown" id="intermission-countdown">5</div>
        </div>
      `;
    }
  }

  updateIntermissionCountdown(seconds) {
    const countdown = document.getElementById('intermission-countdown');
    if (countdown) {
      countdown.textContent = seconds;
      countdown.className = seconds <= 3 ? 'countdown warning' : 'countdown';
    }
  }

  clearIntermission() {
    this.currentIntermission = null;
    const overlay = document.getElementById('intermission-overlay');
    if (overlay) {
      overlay.style.display = 'none';
    }
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
      this.intermissionInterval = null;
    }
  }

  displayQuestion(question) {
    // 문제 표시 로직
    console.log('문제 표시:', question);
  }
}

// 사용 예시
const game = new VersusGame(roomId);
game.start();
```

**장점:**
- ✅ 타임라인 API 호출 불필요
- ✅ Scoreboard API만 호출하면 됨 (이미 사용 중일 가능성 높음)
- ✅ 쉬는 시간 정보가 자동으로 포함됨
- ✅ 구현이 매우 간단

---

### 방법 2: 타임라인 폴링

주기적으로 타임라인을 확인하여 `INTERMISSION_STARTED` 이벤트를 감지합니다.

```javascript
class VersusGame {
  constructor(roomId) {
    this.roomId = roomId;
    this.intermissionInterval = null;
    this.currentIntermission = null;
  }

  /**
   * 타임라인 확인 및 쉬는 시간 감지
   */
  async checkTimeline() {
    try {
      const response = await fetch(`/api/versus/rooms/${this.roomId}/timeline?limit=10`);
      const timeline = await response.json();
      
      // 가장 최근 이벤트 확인
      const latestEvent = timeline[timeline.length - 1];
      
      if (latestEvent?.eventType === 'INTERMISSION_STARTED') {
        this.handleIntermission(latestEvent);
      } else if (latestEvent?.eventType === 'QUESTION_STARTED') {
        // 문제가 시작되면 쉬는 시간 종료
        this.clearIntermission();
      }
    } catch (error) {
      console.error('타임라인 확인 실패:', error);
    }
  }

  /**
   * 쉬는 시간 처리
   */
  handleIntermission(event) {
    // 이미 처리 중인 쉬는 시간이면 스킵
    if (this.currentIntermission?.questionStartAt === event.payload.questionStartAt) {
      return;
    }

    this.currentIntermission = {
      nextQuestionId: event.payload.nextQuestionId,
      nextRoundNo: event.payload.nextRoundNo,
      nextPhase: event.payload.nextPhase,
      questionStartAt: new Date(event.payload.questionStartAt)
    };

    // 쉬는 시간 UI 표시
    this.showIntermissionUI();

    // 카운트다운 시작
    this.startIntermissionCountdown();
  }

  /**
   * 쉬는 시간 카운트다운
   */
  startIntermissionCountdown() {
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
    }

    this.intermissionInterval = setInterval(() => {
      if (!this.currentIntermission) {
        clearInterval(this.intermissionInterval);
        return;
      }

      const now = new Date();
      const questionStartAt = this.currentIntermission.questionStartAt;
      const remainingMs = questionStartAt.getTime() - now.getTime();
      const remainingSeconds = Math.max(0, Math.ceil(remainingMs / 1000));

      // UI 업데이트
      this.updateIntermissionCountdown(remainingSeconds);

      if (remainingSeconds === 0) {
        // 쉬는 시간 종료
        clearInterval(this.intermissionInterval);
        this.clearIntermission();
        
        // 다음 문제 정보 확인
        this.loadNextQuestion();
      }
    }, 100); // 100ms마다 업데이트 (부드러운 카운트다운)
  }

  /**
   * 쉬는 시간 UI 표시
   */
  showIntermissionUI() {
    const { nextRoundNo, nextPhase } = this.currentIntermission;
    
    // 예시: 모달 또는 오버레이 표시
    const intermissionElement = document.getElementById('intermission-overlay');
    if (intermissionElement) {
      intermissionElement.style.display = 'block';
      intermissionElement.innerHTML = `
        <div class="intermission-content">
          <h2>잠시만 기다려주세요</h2>
          <p>다음 문제 준비 중...</p>
          <p>라운드 ${nextRoundNo} - ${nextPhase}</p>
          <div class="countdown" id="intermission-countdown">5</div>
        </div>
      `;
    }
  }

  /**
   * 쉬는 시간 카운트다운 UI 업데이트
   */
  updateIntermissionCountdown(remainingSeconds) {
    const countdownElement = document.getElementById('intermission-countdown');
    if (countdownElement) {
      countdownElement.textContent = remainingSeconds;
      
      // 3초 이하일 때 경고 스타일
      if (remainingSeconds <= 3) {
        countdownElement.classList.add('warning');
      }
    }
  }

  /**
   * 쉬는 시간 종료
   */
  clearIntermission() {
    this.currentIntermission = null;
    
    // UI 숨기기
    const intermissionElement = document.getElementById('intermission-overlay');
    if (intermissionElement) {
      intermissionElement.style.display = 'none';
    }
    
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
      this.intermissionInterval = null;
    }
  }

  /**
   * 다음 문제 로드
   */
  async loadNextQuestion() {
    try {
      const response = await fetch(`/api/versus/rooms/${this.roomId}/scoreboard`);
      const scoreboard = await response.json();
      
      if (scoreboard.currentQuestion) {
        // 다음 문제 표시
        this.displayQuestion(scoreboard.currentQuestion);
      }
    } catch (error) {
      console.error('다음 문제 로드 실패:', error);
    }
  }

  /**
   * 주기적으로 타임라인 확인 시작
   */
  startTimelinePolling() {
    // 1초마다 타임라인 확인
    setInterval(() => {
      this.checkTimeline();
    }, 1000);
  }
}

// 사용 예시
const game = new VersusGame(roomId);
game.startTimelinePolling();
```

---

### 간단한 예시 코드 (Scoreboard API 사용)

```javascript
// Scoreboard API만 사용하는 가장 간단한 방법
async function checkIntermission(roomId) {
  const response = await fetch(`/api/versus/rooms/${roomId}/scoreboard`);
  const scoreboard = await response.json();
  
  // 쉬는 시간 확인
  if (scoreboard.intermission) {
    const { questionStartAt, nextRoundNo, nextPhase } = scoreboard.intermission;
    const endTime = new Date(questionStartAt);
    const now = new Date();
    const remainingSeconds = Math.max(0, Math.ceil((endTime.getTime() - now.getTime()) / 1000));
    
    // 쉬는 시간 UI 표시
    showIntermission(remainingSeconds, nextRoundNo, nextPhase);
  } else if (scoreboard.currentQuestion) {
    // 문제 진행 중
    displayQuestion(scoreboard.currentQuestion);
  }
}

// 1초마다 확인
setInterval(() => checkIntermission(roomId), 1000);
```

---

## 🎨 UI 예시

### 쉬는 시간 오버레이

```html
<div id="intermission-overlay" style="display: none;">
  <div class="intermission-content">
    <h2>잠시만 기다려주세요</h2>
    <p>다음 문제 준비 중...</p>
    <div class="countdown" id="intermission-countdown">5</div>
  </div>
</div>
```

```css
#intermission-overlay {
  position: fixed;
  top: 0;
  left: 0;
  width: 100%;
  height: 100%;
  background: rgba(0, 0, 0, 0.8);
  display: flex;
  justify-content: center;
  align-items: center;
  z-index: 9999;
}

.intermission-content {
  text-align: center;
  color: white;
}

.intermission-content h2 {
  font-size: 2rem;
  margin-bottom: 1rem;
}

.countdown {
  font-size: 4rem;
  font-weight: bold;
  margin-top: 2rem;
  color: #4CAF50;
}

.countdown.warning {
  color: #FF9800;
  animation: pulse 0.5s infinite;
}

@keyframes pulse {
  0%, 100% { opacity: 1; }
  50% { opacity: 0.5; }
}
```

---

## ⚠️ 주의사항

### 1. 중복 처리 방지

같은 `questionStartAt`을 가진 `INTERMISSION_STARTED` 이벤트는 한 번만 처리해야 합니다.

```javascript
// 이미 처리한 쉬는 시간인지 확인
if (this.currentIntermission?.questionStartAt === event.payload.questionStartAt) {
  return; // 이미 처리됨
}
```

### 2. 시간 동기화

`questionStartAt`은 UTC 기준이므로 클라이언트 시간과 정확히 동기화됩니다.

```javascript
const questionStartAt = new Date(event.payload.questionStartAt); // UTC 자동 파싱
const now = new Date(); // 현재 시간 (UTC로 자동 변환)
const remainingMs = questionStartAt.getTime() - now.getTime();
```

### 3. 네트워크 지연 고려

API 호출 지연을 고려하여 약간의 여유를 두는 것이 좋습니다.

```javascript
// 0.1초 여유를 두고 카운트다운 종료
if (remainingMs <= 100) {
  clearInterval(this.intermissionInterval);
  this.clearIntermission();
  this.loadNextQuestion();
}
```

### 4. 첫 번째 문제

첫 번째 문제 시작 시에는 `INTERMISSION_STARTED` 이벤트가 없습니다. `MATCH_STARTED` 이벤트 후 바로 `QUESTION_STARTED` 이벤트가 발생합니다.

---

## 📝 완전한 예시 코드

```javascript
class GoldenbellGame {
  constructor(roomId) {
    this.roomId = roomId;
    this.intermissionInterval = null;
    this.currentIntermission = null;
    this.scoreboardPollingInterval = null;
  }

  start() {
    // Scoreboard 폴링 시작 (이미 사용 중일 가능성 높음)
    this.scoreboardPollingInterval = setInterval(async () => {
      await this.checkScoreboard();
    }, 1000); // 1초마다 확인
  }

  stop() {
    if (this.scoreboardPollingInterval) {
      clearInterval(this.scoreboardPollingInterval);
    }
    this.clearIntermission();
  }

  async checkScoreboard() {
    try {
      const response = await fetch(`/api/versus/rooms/${this.roomId}/scoreboard`);
      const scoreboard = await response.json();
      
      // 쉬는 시간 확인
      if (scoreboard.intermission) {
        this.handleIntermission(scoreboard.intermission);
      } else {
        // 쉬는 시간이 아니면 종료
        this.clearIntermission();
        
        // 현재 문제가 있으면 표시
        if (scoreboard.currentQuestion) {
          this.displayQuestion(scoreboard.currentQuestion);
        }
      }
    } catch (error) {
      console.error('Scoreboard 확인 실패:', error);
    }
  }

  handleIntermission(intermission) {
    const questionStartAt = new Date(intermission.questionStartAt);
    
    // 중복 처리 방지
    if (this.currentIntermission?.questionStartAt?.getTime() === questionStartAt.getTime()) {
      return;
    }

    this.currentIntermission = {
      nextQuestionId: intermission.nextQuestionId,
      nextRoundNo: intermission.nextRoundNo,
      nextPhase: intermission.nextPhase,
      questionStartAt: questionStartAt,
      durationSec: intermission.durationSec
    };

    this.showIntermissionUI();
    this.startIntermissionCountdown();
  }

  startIntermissionCountdown() {
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
    }

    this.intermissionInterval = setInterval(() => {
      if (!this.currentIntermission) {
        clearInterval(this.intermissionInterval);
        return;
      }

      const now = new Date();
      const questionStartAt = this.currentIntermission.questionStartAt;
      const remainingMs = questionStartAt.getTime() - now.getTime();
      const remainingSeconds = Math.max(0, Math.ceil(remainingMs / 1000));

      this.updateIntermissionCountdown(remainingSeconds);

      if (remainingMs <= 100) { // 0.1초 여유
        clearInterval(this.intermissionInterval);
        this.clearIntermission();
      }
    }, 100);
  }

  showIntermissionUI() {
    const overlay = document.getElementById('intermission-overlay');
    if (overlay) {
      overlay.style.display = 'flex';
    }
  }

  updateIntermissionCountdown(seconds) {
    const countdown = document.getElementById('intermission-countdown');
    if (countdown) {
      countdown.textContent = seconds;
      countdown.className = seconds <= 3 ? 'countdown warning' : 'countdown';
    }
  }

  clearIntermission() {
    this.currentIntermission = null;
    const overlay = document.getElementById('intermission-overlay');
    if (overlay) {
      overlay.style.display = 'none';
    }
    if (this.intermissionInterval) {
      clearInterval(this.intermissionInterval);
      this.intermissionInterval = null;
    }
  }

  displayQuestion(question) {
    // 문제 표시 로직
    console.log('문제 표시:', question);
  }
}

// 사용
const game = new GoldenbellGame(roomId);
game.start();

// 정리
// game.stop();
```

---

## 🎯 권장 방법: Scoreboard API 사용

**가장 간단하고 효율적인 방법**은 Scoreboard API를 사용하는 것입니다:

1. ✅ **이미 사용 중**: 대부분의 프론트엔드에서 이미 Scoreboard API를 호출하고 있을 가능성이 높습니다
2. ✅ **추가 API 호출 불필요**: 타임라인 API를 별도로 호출할 필요가 없습니다
3. ✅ **자동 포함**: 쉬는 시간 정보가 자동으로 포함됩니다
4. ✅ **간단한 로직**: `scoreboard.intermission`이 있으면 쉬는 시간, `scoreboard.currentQuestion`이 있으면 문제 진행 중

---

## 🎯 권장 방법: Scoreboard API 사용

**가장 간단하고 효율적인 방법**은 Scoreboard API를 사용하는 것입니다:

1. ✅ **이미 사용 중**: 대부분의 프론트엔드에서 이미 Scoreboard API를 호출하고 있을 가능성이 높습니다
2. ✅ **추가 API 호출 불필요**: 타임라인 API를 별도로 호출할 필요가 없습니다
3. ✅ **자동 포함**: 쉬는 시간 정보가 자동으로 포함됩니다
4. ✅ **간단한 로직**: `scoreboard.intermission`이 있으면 쉬는 시간, `scoreboard.currentQuestion`이 있으면 문제 진행 중

---

## 🔍 디버깅

### 쉬는 시간이 표시되지 않는 경우

1. 타임라인 API에서 `INTERMISSION_STARTED` 이벤트가 발생하는지 확인
2. `questionStartAt` 시간이 올바른지 확인 (UTC 기준)
3. 클라이언트 시간이 정확한지 확인

### 쉬는 시간이 너무 길거나 짧은 경우

- 서버와 클라이언트의 시간 동기화 문제일 수 있습니다
- `questionStartAt`을 기준으로 카운트다운하므로 서버 시간이 정확하면 문제없습니다

---

## 📞 문의

추가 질문이나 문제가 있으면 백엔드 팀에 문의해주세요.

