# ---------- Builder Stage ----------
FROM eclipse-temurin:17-jdk AS builder
WORKDIR /workspace

COPY . ./
RUN chmod +x ./gradlew || true

RUN ./gradlew :services:report-service:bootJar --no-daemon -x test

# ---------- Runtime Stage ----------
FROM eclipse-temurin:17-jre
WORKDIR /app

COPY --from=builder /workspace/services/report-service/build/libs/*.jar app.jar

EXPOSE 8083
ENTRYPOINT ["java","-jar","/app/app.jar"]
