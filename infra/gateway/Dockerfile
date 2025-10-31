# ---------- Builder ----------
FROM eclipse-temurin:17-jdk AS builder
WORKDIR /workspace

COPY . ./
RUN chmod +x ./gradlew || true

RUN ./gradlew :infra:gateway:bootJar --no-daemon -x test

# ---------- Runner ----------
FROM eclipse-temurin:17-jre
WORKDIR /app

COPY --from=builder /workspace/infra/gateway/build/libs/*.jar app.jar

EXPOSE 8080
ENTRYPOINT ["java","-jar","/app/app.jar"]
