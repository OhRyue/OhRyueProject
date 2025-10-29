FROM eclipse-temurin:17-jre
WORKDIR /app
ARG JAR=build/libs/*.jar
COPY ${JAR} app.jar
ENV JAVA_OPTS="-XX:MaxRAMPercentage=75 -XX:+UseG1GC"
EXPOSE 8092
ENTRYPOINT ["sh","-c","java $JAVA_OPTS -Dspring.profiles.active=docker -jar app.jar"]
