# ---------- Builder ----------
FROM gradle:8.10.2-jdk17 AS builder

ENV GRADLE_USER_HOME=/home/gradle/.gradle

WORKDIR /workspace

# Gradle 캐시를 활용하기 위해 gradle wrapper와 build.gradle을 먼저 복사
COPY gradle ./gradle
COPY gradlew ./gradlew
COPY gradlew.bat ./gradlew.bat
COPY build.gradle ./build.gradle
COPY settings.gradle ./settings.gradle
COPY gradle.properties ./gradle.properties

# 의존성 다운로드 (캐시 활용)
RUN ./gradlew :infra:discovery:dependencies --no-daemon || true

# 나머지 소스 코드 복사
COPY . .

RUN ./gradlew :infra:discovery:bootJar --no-daemon -x test

# ---------- Runner ----------
FROM eclipse-temurin:17-jre
WORKDIR /app

COPY --from=builder /workspace/infra/discovery/build/libs/*.jar app.jar

EXPOSE 8761
ENTRYPOINT ["java","-jar","/app/app.jar"]
