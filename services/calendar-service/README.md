# calendar-service

Certification Schedule and Calendar Microservice

## Overview
Manages certification exam schedules with round tracking, status computation, and calendar integration support.

## Port
- **8085** (Docker)
- Local: 8085

## Endpoints

### GET /api/calendar/rounds
Get round schedules for a certificate with optional year filter.

**Query Parameters:**
- `certId` (required): Certificate ID
- `year` (optional, min: 2000): Year filter

**Returns:** List of rounds with status (BEFORE_REG, REGISTER_OPEN, BEFORE_EXAM, EXAM_TODAY, DONE) and D-day.

**Example:**
```bash
curl "http://localhost:8080/api/calendar/rounds?certId=1&year=2025"
```

### GET /api/calendar/me
Get FullCalendar-compatible event format.

**Query Parameters:**
- `certId` (required): Certificate ID
- `year` (optional, min: 2000): Year filter

**Returns:** List of events with `id`, `title`, `start` fields.

**Example:**
```bash
curl "http://localhost:8080/api/calendar/me?certId=1&year=2025"
```

## Database

### Table: cert_schedule
- `id` (BIGINT, PK, AUTO_INCREMENT)
- `cert_id` (BIGINT, NOT NULL)
- `year` (INT, NOT NULL)
- `term` (INT, NOT NULL)
- `reg_start` (DATE, nullable)
- `reg_end` (DATE, nullable)
- `exam_date` (DATE, nullable)
- Index: `idx_cert_year` on (cert_id, year)

### Flyway Migrations
- `V1__init_cert_schedule.sql`: Creates cert_schedule table and index

## Integration
- Uses OpenFeign to call `certs-service` for certificate names
- Circuit breaker/retry/timeout configured for resilience

## Dependencies
- Spring Boot 3.5.6
- Spring Data JPA
- OpenFeign 4.3.0
- Eureka Client 4.3.0
- Flyway 10.20.0
- MySQL Connector 9.1.0
- Resilience4j (circuit breaker, retry, time limiter)
- Apache HttpClient5

## Configuration
See `application-docker.yml` for Eureka, OpenFeign, datasource, Flyway, and Resilience4j configuration.

