# ---------- Builder ----------
FROM gradle:8.10.2-jdk17 AS builder

ENV GRADLE_USER_HOME=/home/gradle/.gradle

WORKDIR /workspace

COPY . .

RUN gradle :infra:gateway:bootJar --no-daemon -x test

# ---------- Runner ----------
FROM eclipse-temurin:17-jre
WORKDIR /app

COPY --from=builder /workspace/infra/gateway/build/libs/*.jar app.jar

EXPOSE 8080
ENTRYPOINT ["java","-jar","/app/app.jar"]
