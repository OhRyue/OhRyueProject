# auth-service

Authentication Microservice (Stub)

## Overview
Minimal stub for authentication. Returns demo user data. Actual authentication implementation is pending.

## Port
- **8086** (Docker)
- Local: 8086

## Endpoints

### POST /api/auth/login
Login stub - accepts any credentials and returns demo user.

**Request Body:**
```json
{
  "username": "demo",
  "password": "demo"
}
```

**Response:**
```json
{
  "id": 1,
  "username": "demo",
  "email": "demo@demo.local"
}
```

**Example:**
```bash
curl -X POST "http://localhost:8080/api/auth/login" \
  -H "Content-Type: application/json" \
  -d '{"username":"demo","password":"demo"}'
```

### GET /api/auth/me
Get current user info (stub).

**Returns:** Demo user information

**Example:**
```bash
curl "http://localhost:8080/api/auth/me"
```

## Status
⚠️ **TODO:** Implement real authentication
- JWT-based authentication
- User database integration
- Password hashing
- Session management

## Dependencies
- Spring Boot 3.5.6
- Eureka Client 4.3.0
- OpenAPI/Swagger 2.6.0

## Configuration
See `application-docker.yml` for Eureka and management endpoint configuration.

## Notes
- No database dependencies (stub)
- Minimal configuration
- Ready for future authentication implementation

