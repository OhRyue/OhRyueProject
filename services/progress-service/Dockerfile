# ---------- Builder Stage ----------
FROM gradle:8.10.2-jdk17 AS builder

ENV GRADLE_USER_HOME=/home/gradle/.gradle

WORKDIR /workspace

COPY . .

RUN gradle :services:progress-service:bootJar --no-daemon -x test

# ---------- Runtime Stage ----------
FROM eclipse-temurin:17-jre
WORKDIR /app

# 타임존 설정
ENV TZ=Asia/Seoul
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone

COPY --from=builder /workspace/services/progress-service/build/libs/*.jar app.jar

EXPOSE 8083
ENTRYPOINT ["java","-Duser.timezone=Asia/Seoul","-jar","/app/app.jar"]
