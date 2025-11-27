#!/bin/bash

# Versus-Service 관련 테스트 실행 스크립트

set -e

echo "=========================================="
echo "Versus-Service 테스트 실행 스크립트"
echo "=========================================="
echo ""

# 색상 정의
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
NC='\033[0m' # No Color

# 프로젝트 루트 디렉토리
PROJECT_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$PROJECT_ROOT"

# 테스트 실행 함수
run_test() {
    local service=$1
    local test_class=$2
    local description=$3
    
    echo -e "${YELLOW}▶ 실행 중: $description${NC}"
    echo "서비스: $service"
    echo "테스트 클래스: $test_class"
    echo ""
    
    if [ -z "$test_class" ]; then
        ./gradlew :services:$service:test --info
    else
        ./gradlew :services:$service:test --tests "$test_class" --info
    fi
    
    if [ $? -eq 0 ]; then
        echo -e "${GREEN}✓ $description 성공${NC}"
    else
        echo -e "${RED}✗ $description 실패${NC}"
        return 1
    fi
    echo ""
}

# 전체 테스트 실행
run_all_tests() {
    echo "전체 테스트 실행 중..."
    echo ""
    
    run_test "study-service" "" "study-service 전체 테스트"
    run_test "progress-service" "" "progress-service 전체 테스트"
    run_test "versus-service" "" "versus-service 전체 테스트"
}

# 개별 테스트 실행
run_individual_tests() {
    echo "개별 테스트 실행 중..."
    echo ""
    
    run_test "study-service" "com.OhRyue.certpilot.study.controller.VersusQuestionControllerTest" \
        "study-service API 테스트"
    
    run_test "progress-service" "com.OhRyue.certpilot.progress.controller.VersusResultControllerTest" \
        "progress-service API 테스트"
    
    run_test "versus-service" "com.OhRyue.certpilot.versus.integration.VersusServiceE2ETest" \
        "versus-service E2E 테스트"
}

# 테스트 리포트 생성
generate_report() {
    echo "테스트 리포트 생성 중..."
    ./gradlew test jacocoTestReport
    echo ""
    echo "테스트 리포트 위치:"
    echo "  - HTML: build/reports/tests/test/index.html"
    echo "  - 커버리지: build/reports/jacoco/test/html/index.html"
}

# 메인 메뉴
main() {
    case "${1:-all}" in
        all)
            run_all_tests
            ;;
        individual)
            run_individual_tests
            ;;
        report)
            generate_report
            ;;
        study)
            run_test "study-service" "com.OhRyue.certpilot.study.controller.VersusQuestionControllerTest" \
                "study-service API 테스트"
            ;;
        progress)
            run_test "progress-service" "com.OhRyue.certpilot.progress.controller.VersusResultControllerTest" \
                "progress-service API 테스트"
            ;;
        versus)
            run_test "versus-service" "com.OhRyue.certpilot.versus.integration.VersusServiceE2ETest" \
                "versus-service E2E 테스트"
            ;;
        *)
            echo "사용법: $0 [all|individual|report|study|progress|versus]"
            echo ""
            echo "옵션:"
            echo "  all        - 모든 서비스의 전체 테스트 실행"
            echo "  individual - 개별 API/E2E 테스트만 실행"
            echo "  report     - 테스트 리포트 생성"
            echo "  study      - study-service 테스트만 실행"
            echo "  progress   - progress-service 테스트만 실행"
            echo "  versus     - versus-service 테스트만 실행"
            exit 1
            ;;
    esac
}

main "$@"





