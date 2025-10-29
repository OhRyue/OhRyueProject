# certs-service

Certificate Management Microservice

## Overview
Manages certificate information including name, field categorization, search and pagination.

## Port
- **8084** (Docker)
- Local: 8084

## Endpoints

### GET /api/certs
List certificates with optional field filter and pagination.

**Query Parameters:**
- `field` (optional): Filter by field (e.g., "IT", "회계")
- `page` (default: 0): Page number
- `size` (default: 20): Page size (max 100)

**Example:**
```bash
curl "http://localhost:8080/api/certs?field=IT&page=0&size=20"
```

### GET /api/certs/{id}
Get certificate details by ID.

**Example:**
```bash
curl "http://localhost:8080/api/certs/1"
```

### GET /api/certs/search
Search certificates by keyword (for autocomplete/recommendation).

**Query Parameters:**
- `keyword`: Search term
- `limit` (default: 10): Result limit

**Example:**
```bash
curl "http://localhost:8080/api/certs/search?keyword=정보&limit=10"
```

## Database

### Table: certificate
- `id` (BIGINT, PK, AUTO_INCREMENT)
- `name` (VARCHAR(255), NOT NULL)
- `field` (VARCHAR(100), nullable)
- Index on `field`

### Flyway Migrations
- `V1__init_certificate.sql`: Creates certificate table and index

## Dependencies
- Spring Boot 3.5.6
- Spring Data JPA
- Eureka Client 4.3.0
- Flyway 10.20.0
- MySQL Connector 9.1.0
- OpenAPI/Swagger 2.6.0
- Resilience4j

## Configuration
See `application-docker.yml` for Eureka, datasource, Flyway, and management endpoint configuration.

