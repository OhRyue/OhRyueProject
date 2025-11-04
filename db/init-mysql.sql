-- ===== Databases =====
CREATE DATABASE IF NOT EXISTS certpilot_account CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_cert    CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_gamify  CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_report  CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_study   CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;
CREATE DATABASE IF NOT EXISTS certpilot_versus  CHARACTER SET utf8mb4 COLLATE utf8mb4_0900_ai_ci;

-- ===== Service Users =====
CREATE USER IF NOT EXISTS 'account'@'%' IDENTIFIED BY 'accountpw';
GRANT ALL ON certpilot_account.* TO 'account'@'%';

CREATE USER IF NOT EXISTS 'cert'@'%' IDENTIFIED BY 'certpw';
GRANT ALL ON certpilot_cert.* TO 'cert'@'%';

CREATE USER IF NOT EXISTS 'gamify'@'%' IDENTIFIED BY 'gamifypw';
GRANT ALL ON certpilot_gamify.* TO 'gamify'@'%';

CREATE USER IF NOT EXISTS 'report'@'%' IDENTIFIED BY 'reportpw';
GRANT ALL ON certpilot_report.* TO 'report'@'%';

CREATE USER IF NOT EXISTS 'study'@'%' IDENTIFIED BY 'studypw';
GRANT ALL ON certpilot_study.* TO 'study'@'%';

CREATE USER IF NOT EXISTS 'versus'@'%' IDENTIFIED BY 'versuspw';
GRANT ALL ON certpilot_versus.* TO 'versus'@'%';

-- ===== (app/apppw) sees all service DBs =====
CREATE USER IF NOT EXISTS 'app'@'%' IDENTIFIED BY 'apppw';
GRANT ALL ON certpilot_account.* TO 'app'@'%';
GRANT ALL ON certpilot_cert.*    TO 'app'@'%';
GRANT ALL ON certpilot_gamify.*  TO 'app'@'%';
GRANT ALL ON certpilot_report.*  TO 'app'@'%';
GRANT ALL ON certpilot_study.*   TO 'app'@'%';
GRANT ALL ON certpilot_versus.*  TO 'app'@'%';

FLUSH PRIVILEGES;
