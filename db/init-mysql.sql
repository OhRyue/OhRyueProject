-- =========================================================
-- CertPilot: MySQL bootstrap (databases + users + grants)
-- =========================================================

SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

-- ===== Databases =====
CREATE DATABASE IF NOT EXISTS certpilot_account CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_cert    CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_study   CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;

CREATE DATABASE IF NOT EXISTS certpilot_versus  CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_progress CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_community CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;

-- ===== Service Users (원하시는 명명 규칙에 맞춰 동시 지원) =====
-- account-service: 현재 app/apppw를 사용 중이므로 app 사용자 유지 + account 사용자도 함께 생성
CREATE USER IF NOT EXISTS 'account'@'%' IDENTIFIED BY 'accountpw';
GRANT ALL ON certpilot_account.* TO 'account'@'%';

CREATE USER IF NOT EXISTS 'app'@'%' IDENTIFIED BY 'apppw';
GRANT ALL ON certpilot_account.* TO 'app'@'%';
GRANT ALL ON certpilot_cert.*    TO 'app'@'%';
GRANT ALL ON certpilot_study.*   TO 'app'@'%';
GRANT ALL ON certpilot_versus.*  TO 'app'@'%';
GRANT ALL ON certpilot_progress.*  TO 'app'@'%';
GRANT ALL ON certpilot_community.* TO 'app'@'%';

-- cert-service
CREATE USER IF NOT EXISTS 'cert'@'%' IDENTIFIED BY 'certpw';
GRANT ALL ON certpilot_cert.* TO 'cert'@'%';

-- study-service
CREATE USER IF NOT EXISTS 'study'@'%' IDENTIFIED BY 'studypw';
GRANT ALL ON certpilot_study.* TO 'study'@'%';

-- study 계정에 cert DB에 대한 SELECT 권한을 부여합니다.
GRANT SELECT ON certpilot_cert.* TO 'study'@'%';

-- versus-service
CREATE USER IF NOT EXISTS 'versus'@'%' IDENTIFIED BY 'versuspw';
GRANT ALL ON certpilot_versus.* TO 'versus'@'%';

-- progress-service
CREATE USER IF NOT EXISTS 'progress'@'%' IDENTIFIED BY 'progresspw';
GRANT ALL ON certpilot_progress.* TO 'progress'@'%';

-- community-service
CREATE USER IF NOT EXISTS 'community'@'%' IDENTIFIED BY 'communitypw';
GRANT ALL ON certpilot_community.* TO 'community'@'%';

FLUSH PRIVILEGES;

SET FOREIGN_KEY_CHECKS = 1;
