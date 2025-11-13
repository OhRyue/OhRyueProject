---

# CertPilot API Surface (Gateway `/api/**`, current paths)

> All protected endpoints expect `Authorization: Bearer <JWT>`  
> This document mirrors the **implemented** APIs (Nov 2025). Rows marked **TODO (not implemented yet)** remain drafts.  
> `cert-service` still exposes `/api/certs/**`; migrating to `/api/cert/**` is planned but not live.

---

## 🧭 1) Onboarding (Signup → Login → Initial Setup)

| Feature                              | Service  | Endpoint (via Gateway)                  | Notes |
| ------------------------------------ | -------- | --------------------------------------- | ----- |
| Signup                               | account  | `POST /api/account/signup`              | Sends 6-digit verification code e-mail. |
| Verify email code                    | account  | `POST /api/account/verify-email`        | Confirms verification code before login. |
| Login                                | account  | `POST /api/account/login`               | Returns `{accessToken, refreshToken}`. |
| Refresh access token                 | account  | `POST /api/account/refresh`             | Body `{refreshToken}` (differs from draft `/token/refresh`). |
| Logout                               | account  | `POST /api/account/logout`              | Deletes stored refresh token. |
| Onboarding status                    | account  | `GET /api/account/onboarding/status`    | Authenticated user only. |
| Mark onboarding complete             | account  | `POST /api/account/onboarding/complete` | Marks onboarding flag. |
| Account overview                     | account  | `GET /api/account/me`                   | Bundles account/profile/settings/goal/onboarding. |
| Profile retrieve                     | account  | `GET /api/account/profile`              | |
| Profile update                       | account  | `PUT /api/account/profile`              | |
| Goal certificate retrieve            | account  | `GET /api/account/goal`                 | Includes cached `ddayCached`. |
| Goal certificate upsert              | account  | `PUT /api/account/goal`                 | Body `GoalUpsertRequest`. |
| Settings retrieve                    | account  | `GET /api/account/settings`             | |
| Settings update                      | account  | `PUT /api/account/settings`             | |
| Settings reset                       | account  | `POST /api/account/settings/reset`      | Resets toggles to defaults. |
| Certificate list (for selection)     | cert     | `GET /api/certs/external/qualifications`| NOTE: still under `/api/certs/**`; rename planned. |
| TODO (draft) `/api/account/token/refresh` | account | — | **Not implemented** – use `POST /api/account/refresh`. |

---

## 🏠 2) Home / Left Menu / Store

| Feature / Card            | Service  | Endpoint (via Gateway)                         | Notes |
| ------------------------- | -------- | ---------------------------------------------- | ----- |
| My info card              | account  | `GET /api/account/me`                          | Includes profile/settings/goal snapshot. |
| Goal certificate summary  | account  | `GET /api/account/goal`                        | Contains round + cached D-day. |
| Home overview             | progress | `GET /api/progress/home/overview`              | Requires `Authorization` header & `userId`. |
| Progress card             | progress | `GET /api/progress/home/progress-card`         | |
| Live ranking              | progress | `GET /api/progress/home/ranking`               | Returns current top ranking data. |
| Quick stats (today vs yesterday) | progress | `GET /api/progress/home/quick-stats`      | |
| Quick start menu          | progress | `GET /api/progress/home/quick-menu`            | |
| Left sidebar summary      | progress | `GET /api/progress/sidebar`                    | Requires `Authorization` header & `userId`. |
| Store catalog             | progress | `GET /api/progress/store/catalog`              | Query: `userId`, optional `category`, `q`. |
| Store active items        | progress | `GET /api/progress/store/items`                | Optional filters. |
| Purchase item             | progress | `POST /api/progress/store/purchase`            | Params: `userId`, `itemId`. |
| Inventory list            | progress | `GET /api/progress/store/inventory`            | `userId` query. |
| Loadout set               | progress | `POST /api/progress/store/loadout`             | Params per slot. |
| Loadout get               | progress | `GET /api/progress/store/loadout`              | `userId` query. |
| TODO (draft) `GET /api/progress/today`        | progress | — | **Not implemented** – use quick-stats/overview. |
| TODO (draft) `GET /api/progress/rank/top5`    | progress | — | **Not implemented** – current endpoint is `GET /api/progress/rank/top`. |
| TODO (draft) goal exam D-day endpoint         | cert     | — | **Not implemented** – rely on goal response (`ddayCached`) or external schedule APIs. |

---

## 🎓 3) Main Study (Written / Practical / Sessions)

| Group / Feature                  | Service | Endpoint (via Gateway)                                     | Notes |
| -------------------------------- | ------- | ---------------------------------------------------------- | ----- |
| Topic list                       | study   | `GET /api/study/topics`                                    | Filters: `mode`, `parentId`. |
| Topic search                     | study   | `GET /api/study/topics/search`                             | Query `code`, `title`. |
| Start session                    | study   | `POST /api/study/session/start`                            | Body `{flow, topicId, mode, expectedCount, ...}`. |
| Session detail                   | study   | `GET /api/study/session/{sessionId}`                       | Returns current step metadata. |
| Session step advance             | study   | `POST /api/study/session/advance`                          | Marks current step complete. |
| TODO (draft) session finish      | study   | —                                                          | **Not implemented**. |
| TODO (draft) heartbeat / pause / resume | study | — | **Not implemented**. |
| Written concept blocks           | study   | `GET /api/study/written/concept/{topicId}`                 | |
| Written mini check load          | study   | `GET /api/study/written/mini/{topicId}?userId=...`         | 4 OX questions. |
| Written mini submit              | study   | `POST /api/study/written/mini/submit`                      | Body `MiniSubmitReq`. |
| Written mini grade-one           | study   | `POST /api/study/written/mini/grade-one`                   | |
| Written MCQ load                 | study   | `GET /api/study/written/mcq/{topicId}?userId=...`          | 5-question set. |
| Written MCQ submit               | study   | `POST /api/study/written/mcq/submit`                       | DB grading + GPT wrong-answer explanation. |
| Written MCQ grade-one            | study   | `POST /api/study/written/mcq/grade-one`                    | |
| Written review load              | study   | `GET /api/study/written/review/{rootTopicId}?userId=...`   | 20-question review across subtree. |
| Written review submit            | study   | `POST /api/study/written/review/submit?rootTopicId=...`    | Body `McqSubmitReq`. |
| Written summary                  | study   | `GET /api/study/written/summary?userId=...&topicId=...`    | Includes AI summary + completion flag. |
| Practical mini check load        | study   | `GET /api/study/practical/mini/{topicId}?userId=...`       | |
| Practical mini submit            | study   | `POST /api/study/practical/mini/submit`                    | |
| Practical mini grade-one         | study   | `POST /api/study/practical/mini/grade-one`                 | |
| Practical set load               | study   | `GET /api/study/practical/set/{topicId}?userId=...`        | Short/Long mix. |
| Practical submit                 | study   | `POST /api/study/practical/submit`                         | GPT grading + DB explanations. |
| Practical review load            | study   | `GET /api/study/practical/review/{rootTopicId}?userId=...` | |
| Practical review submit          | study   | `POST /api/study/practical/review/submit`                  | Body `PracticalReviewSubmitReq`. |
| Practical summary                | study   | `GET /api/study/practical/summary?userId=...&topicId=...`  | |
| Practical grade-one              | study   | `POST /api/study/practical/grade-one`                      | |
| Wrong recap by topic             | study   | `GET /api/study/wrong/recap`                               | Params: `topicId`, `userId`, optional `limit`. |
| Wrong recap by IDs               | study   | `GET /api/study/wrong/recap-by-ids`                        | Query `ids`, optional `userId`. |
| Wrong recap (written session)    | study   | `GET /api/study/wrong/written/session`                     | Params: `userId`, `sessionId`, optional `step`. |
| Wrong recap (practical session)  | study   | `GET /api/study/wrong/practical/session`                   | Params: `userId`, `sessionId`, optional `step`. |
| Recommendation - weak tags       | study   | `GET /api/study/reco/weak-tags`                            | Query `userId`, `topN`, `minTried`. |
| Recommendation - tag quiz        | study   | `POST /api/study/reco/tag-quiz`                            | Body `TagQuizReq`. |
| TODO (draft) session progress endpoint | study | — | **Not implemented** – progress data exposed via step metadata/summary. |
| TODO (draft) `GET /api/study/session/{sessionId}/wrong-list` | study | — | **Not implemented** – use `/api/study/wrong/*`. |
| TODO (draft) tag power refresh   | progress | — | **Not implemented** – no `/api/progress/tags/refresh`. |

---

## 🧩 4) Assist Study / Goals / Recommendations

| Feature / Flow                   | Service  | Endpoint (via Gateway)                            | Notes |
| -------------------------------- | -------- | ------------------------------------------------- | ----- |
| Written assist by category       | study    | `POST /api/study/assist/written/category`         | Body `CategoryStartReq`; returns question set envelope. |
| Written assist by difficulty     | study    | `POST /api/study/assist/written/difficulty`       | |
| Written assist by weakness       | study    | `POST /api/study/assist/written/weakness`         | |
| Practical assist by category     | study    | `POST /api/study/assist/practical/category`       | |
| Practical assist by difficulty   | study    | `POST /api/study/assist/practical/difficulty`     | |
| Practical assist by weakness     | study    | `POST /api/study/assist/practical/weakness`       | |
| Weak tag discovery (top-N)       | study    | `GET /api/study/reco/weak-tags`                   | |
| Tag-targeted quiz set            | study    | `POST /api/study/reco/tag-quiz`                   | |
| Daily assist goal summary        | progress | `GET /api/progress/goal/summary`                  | |
| Daily assist goal status         | progress | `GET /api/progress/goal/today`                    | |
| Set daily assist target          | progress | `POST /api/progress/goal/today/target`            | Params `userId`, `target`. |
| Increment daily progress         | progress | `POST /api/progress/goal/today/increment`         | |
| TODO (draft) assist catalog list | study    | —                                                | **Not implemented** – no `/catalog/categories` endpoint. |
| TODO (draft) assist session via `/session/start` | study | — | Current implementation exposes per-mode POST endpoints above. |
| TODO (draft) item-by-item APIs (`next`, `submit`, `resubmit`) | study | — | **Not implemented** – assist mode returns batched question sets. |
| TODO (draft) assist finish/summary endpoints | study | — | No `/finish` or `/summary` routes today. |

---

## 📊 5) Study & Progress Reports

| Report / Metric                         | Service  | Endpoint (via Gateway)                          | Notes |
| --------------------------------------- | -------- | ----------------------------------------------- | ----- |
| Study report summary                    | study    | `GET /api/study/report/summary`                 | |
| Study progress card                     | study    | `GET /api/study/report/progress-card`           | Requires `userId`, `certId`. |
| Study recent daily chart                | study    | `GET /api/study/report/recent-daily`            | Query `userId`, optional `days`. |
| Study tag ability (rich)                | study    | `GET /api/study/report-plus/tag-ability`        | Optional `mode`. |
| Study timeseries (weekly/monthly)       | study    | `GET /api/study/report-plus/timeseries`         | Query `range` (e.g., `WEEK`). |
| Progress overview (time/solved/accuracy)| progress | `GET /api/progress/report/overview`             | Query `userId`, `mode`. |
| Progress ability by tag                 | progress | `GET /api/progress/report/ability-by-tag`       | Query `userId`, `mode`, `limit`. |
| Progress recent records                 | progress | `GET /api/progress/report/recent-records`       | Query `userId`, optional `limit`. |
| Streak status                           | progress | `GET /api/progress/streak`                      | |
| Streak tick (mark today)                | progress | `POST /api/progress/streak/tick`                | |
| TODO (draft) `GET /api/progress/report/by-tag` | progress | — | Use `ability-by-tag`. |
| TODO (draft) `GET /api/progress/report/recent?limit=10` | progress | — | Use `recent-records`. |

---

## 🧾 6) Certificate Info (Q-Net & Internal)

| Section / Data                           | Service | Endpoint (via Gateway)                                | Notes |
| ---------------------------------------- | ------- | ----------------------------------------------------- | ----- |
| Current goal certificate detail          | cert    | `GET /api/certs/current?userId=...`                   | Includes fees, pass rules, etc. |
| Certificate tips                         | cert    | `GET /api/certs/{jmCd}/tips`                          | Internal tips/FAQ. |
| External qualification list              | cert    | `GET /api/certs/external/qualifications`              | Optional `seriesCd`. |
| External exam schedule                   | cert    | `GET /api/certs/external/exam-schedule`               | Optional `year`, `qualgbCd`, `jmCd`. |
| External open question list              | cert    | `GET /api/certs/external/open-questions`              | Params `jmCd`, `page`, `size`. |
| Trigger Q-Net sync                       | cert    | `POST /api/certs/external/sync/{source}`              | `source`: qualification / exam-schedule / open-questions / all. |
| Health ping                              | cert    | `GET /cert/ping`                                      | Direct service route (no `/api`). |
| TODO (draft) `/api/cert/list`            | cert    | —                                                     | **Not implemented** – use external qualifications above. |
| TODO (draft) `/api/cert/{certId}` detail | cert    | —                                                     | No separate REST endpoint yet; `/current` covers goal cert. |
| TODO (draft) subjects endpoint           | cert    | —                                                     | No `/subjects` route in current code. |
| TODO (draft) `/api/cert/*` renaming      | cert    | —                                                     | Gateway currently rewrites `/api/certs/**`. |

---

## ⚔️ 7) Versus / Events

| Feature / Flow            | Service | Endpoint (via Gateway)                | Notes |
| ------------------------- | ------- | ------------------------------------- | ----- |
| Service ping              | versus  | `GET /api/versus/ping`                | |
| List rooms                | versus  | `GET /api/versus/rooms`               | Optional filters `mode`, `status`. |
| Create room               | versus  | `POST /api/versus/rooms`              | Body `CreateRoomReq`. |
| Room detail               | versus  | `GET /api/versus/rooms/{roomId}`      | |
| Join room                 | versus  | `POST /api/versus/rooms/{roomId}/join`| Body `JoinRoomReq`. |
| Start room                | versus  | `POST /api/versus/rooms/{roomId}/start`| |
| Submit answer             | versus  | `POST /api/versus/rooms/{roomId}/answers` | Body `SubmitAnswerReq`. |
| Scoreboard                | versus  | `GET /api/versus/rooms/{roomId}/scoreboard` | |
| Timeline                  | versus  | `GET /api/versus/rooms/{roomId}/timeline`   | Query `limit` (default 50). |
| Room state snapshot       | versus  | `GET /api/versus/rooms/{roomId}/state`      | Combines detail + recent events. |
| TODO (draft) matchmaking enqueue | versus | — | **Not implemented** – no `/match/enqueue`/`dequeue` routes. |
| TODO (draft) tournament / goldenbell shortcuts | versus | — | Use generic room creation + start. |
| TODO (draft) `/ws/versus` WebSocket         | versus | — | REST-only implementation today. |

---

## 🏆 8) Ranking / XP / Badges

| Feature / Metric              | Service  | Endpoint (via Gateway)                   | Notes |
| ----------------------------- | -------- | ---------------------------------------- | ----- |
| XP wallet                     | progress | `GET /api/progress/xp/wallet`            | Query `userId`. |
| XP gain                       | progress | `POST /api/progress/xp/gain`             | Params `userId`, `delta`, optional `reason`, `refId`. |
| Recompute rank score          | progress | `POST /api/progress/rank/recompute`      | Param `userId`. |
| Top-N ranking                 | progress | `GET /api/progress/rank/top`             | Query `limit` (1–10). |
| Leaderboard by scope          | progress | `GET /api/progress/rankings/{scope}`     | Scope = `OVERALL`, `WEEKLY`, `HALL_OF_FAME`, `FRIENDS`; `userId` query. |
| Leaderboard history           | progress | `GET /api/progress/rankings/{scope}/history` | Optional `reference`. |
| Leaderboard recompute         | progress | `POST /api/progress/rankings/recompute`  | Body `LeaderboardRecomputeRequest`. |
| Badge status                  | progress | `GET /api/progress/badges`               | Query `userId`. |
| Badge reevaluation            | progress | `POST /api/progress/badges/evaluate`     | Body `BadgeEvaluateRequest`. |
| TODO (draft) `GET /api/progress/me` | progress | — | **Not implemented** – use `/api/account/me` + ranking endpoints. |
| TODO (draft) XP grant (sessionId)    | progress | — | **Not implemented** – idempotent grant API pending; use `POST /api/progress/xp/gain`. |

---

## 💬 9) Community

| Feature / Action            | Service    | Endpoint (via Gateway)                             | Notes |
| --------------------------- | ---------- | -------------------------------------------------- | ----- |
| List categories             | community  | `GET /api/community/categories`                    | |
| Posts list                  | community  | `GET /api/community/posts`                         | Rich filter set (`category`, `keyword`, `sort`, `page`, `size`, `days`, `today`, `anonymousOnly`, `mine`). |
| Post detail                 | community  | `GET /api/community/posts/{postId}`                | Optional header `X-User-Id`. |
| Hot posts                   | community  | `GET /api/community/posts/hot`                     | Query `days`, `limit`. |
| Today’s posts               | community  | `GET /api/community/posts/today`                   | Query `sort`, `limit`. |
| My activity summary         | community  | `GET /api/community/posts/my/activity`             | Requires `X-User-Id`. |
| Post metrics                | community  | `GET /api/community/posts/{postId}/metrics`        | |
| Create post                 | community  | `POST /api/community/posts`                        | Header `X-User-Id`. |
| Update post                 | community  | `PUT /api/community/posts/{postId}`                | Header `X-User-Id`. |
| Delete post                 | community  | `DELETE /api/community/posts/{postId}`             | Header `X-User-Id`. |
| List comments               | community  | `GET /api/community/posts/{postId}/comments`       | Pagination support (`page`, `size`). |
| Create comment              | community  | `POST /api/community/posts/{postId}/comments`      | Header `X-User-Id`. |
| Update comment              | community  | `PUT /api/community/comments/{commentId}`          | Header `X-User-Id`. |
| Delete comment              | community  | `DELETE /api/community/comments/{commentId}`       | Header `X-User-Id`. |
| Toggle reaction             | community  | `POST /api/community/reactions/toggle`             | Header `X-User-Id`. |
| Report post/comment         | community  | `POST /api/community/moderation/report`            | Header `X-User-Id`. |
| Block user                  | community  | `POST /api/community/moderation/blocks`            | Header `X-User-Id`. |
| Unblock user                | community  | `DELETE /api/community/moderation/blocks/{blockedUserId}` | Header `X-User-Id`. |
| Block list                  | community  | `GET /api/community/moderation/blocks`             | Header `X-User-Id`. |
| TODO (draft) post view log  | community  | —                                                 | **Not implemented** – no `/posts/{id}/view` endpoint. |

---

## ⚙️ 10) Settings / Data Management

| Section / Action            | Service  | Endpoint (via Gateway)                         | Notes |
| --------------------------- | -------- | ---------------------------------------------- | ----- |
| Profile view / update       | account  | `GET /api/account/profile` / `PUT /api/account/profile` | See Onboarding section. |
| Preferences view / update   | account  | `GET /api/account/settings` / `PUT /api/account/settings` | |
| Preferences reset           | account  | `POST /api/account/settings/reset`             | Restores defaults. |
| Store loadout management    | progress | `POST /api/progress/store/loadout` / `GET /api/progress/store/loadout` | Avatar/customisation slots. |
| TODO (draft) reset data API | progress | —                                             | **Not implemented** – no `/api/progress/reset`. |

---

## 🔑 Common Notes

* All services are exposed through Spring Cloud Gateway using the `/api/{service}/**` prefixes listed above.
* Cert-service currently responds under `/api/certs/**`; documentation reflects the live path and flags the planned rename.
* XP grant idempotency (`POST /api/progress/xp/grant`) is not live; the working endpoint is `POST /api/progress/xp/gain`.
* Controllers interacting with GPT/LLM vs. DB grading include JavaDoc in code clarifying each responsibility.
