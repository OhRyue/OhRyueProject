# 📘 CertPilot – AI 기반 자격증 학습 플랫폼 (MSA)

CertPilot은 국가기술자격 시험 준비를 위한
**AI 기반 학습 + 게임화(Gamify) 플랫폼**입니다.

Backend는 **Spring Cloud MSA**,
Frontend는 **React + Vite**,
DevOps는 **Docker + KT Cloud** 기반으로 구성됩니다.

---

## 🚀 기능 요약
### ✨ 학습

- 개념 → 미니체크(OX) → 문제풀이(MCQ/SHORT/LONG)

- AI 해설·AI 채점(OpenAI 기반)

- 카테고리/난이도/약점 기반 보조학습(Assist)

### ✨ 게임화

- 1:1 배틀

- 8인 토너먼트

- 20인 골든벨(OX→MCQ→단답→서술)

### ✨ 리포트

- 일간/주간 리포트

- 태그 능력지수

- XP/레벨/뱃지/포인트

---

## 🧩 아키텍처
```
Frontend → Nginx → Gateway → Eureka → MSA Services
                           ↓
                    MySQL / Redis
```

서비스 목록:

- account / cert / study / progress / versus / community

---

## 🔧 기술 스택

- **Backend**: Java 17, Spring Boot 3.5, Spring Cloud, JPA, Flyway

- **Infra**: Docker Compose, Redis, MySQL, Nginx, KT Cloud

- **Frontend**: React, TS, Vite, Tailwind

- **AI**: OpenAI 기반 설명/채점 + Fallback Rule Engine