# Versus-Service 관련 테스트 실행 스크립트 (PowerShell)

param(
    [Parameter(Position=0)]
    [ValidateSet("all", "individual", "report", "study", "progress", "versus")]
    [string]$Mode = "all"
)

$ErrorActionPreference = "Stop"

Write-Host "==========================================" -ForegroundColor Cyan
Write-Host "Versus-Service 테스트 실행 스크립트" -ForegroundColor Cyan
Write-Host "==========================================" -ForegroundColor Cyan
Write-Host ""

# 프로젝트 루트 디렉토리
$PROJECT_ROOT = Split-Path -Parent $PSScriptRoot
Set-Location $PROJECT_ROOT

# 테스트 실행 함수
function Run-Test {
    param(
        [string]$Service,
        [string]$TestClass,
        [string]$Description
    )
    
    Write-Host "▶ 실행 중: $Description" -ForegroundColor Yellow
    Write-Host "서비스: $Service"
    Write-Host "테스트 클래스: $TestClass"
    Write-Host ""
    
    if ([string]::IsNullOrEmpty($TestClass)) {
        & ./gradlew.bat ":services:$Service`:test" --info
    } else {
        & ./gradlew.bat ":services:$Service`:test" "--tests" "$TestClass" --info
    }
    
    if ($LASTEXITCODE -eq 0) {
        Write-Host "✓ $Description 성공" -ForegroundColor Green
    } else {
        Write-Host "✗ $Description 실패" -ForegroundColor Red
        exit 1
    }
    Write-Host ""
}

# 전체 테스트 실행
function Run-AllTests {
    Write-Host "전체 테스트 실행 중..." -ForegroundColor Cyan
    Write-Host ""
    
    Run-Test "study-service" "" "study-service 전체 테스트"
    Run-Test "progress-service" "" "progress-service 전체 테스트"
    Run-Test "versus-service" "" "versus-service 전체 테스트"
}

# 개별 테스트 실행
function Run-IndividualTests {
    Write-Host "개별 테스트 실행 중..." -ForegroundColor Cyan
    Write-Host ""
    
    Run-Test "study-service" "com.OhRyue.certpilot.study.controller.VersusQuestionControllerTest" `
        "study-service API 테스트"
    
    Run-Test "progress-service" "com.OhRyue.certpilot.progress.controller.VersusResultControllerTest" `
        "progress-service API 테스트"
    
    Run-Test "versus-service" "com.OhRyue.certpilot.versus.integration.VersusServiceE2ETest" `
        "versus-service E2E 테스트"
}

# 테스트 리포트 생성
function Generate-Report {
    Write-Host "테스트 리포트 생성 중..." -ForegroundColor Cyan
    & ./gradlew.bat test jacocoTestReport
    Write-Host ""
    Write-Host "테스트 리포트 위치:" -ForegroundColor Green
    Write-Host "  - HTML: build/reports/tests/test/index.html"
    Write-Host "  - 커버리지: build/reports/jacoco/test/html/index.html"
}

# 메인 로직
switch ($Mode) {
    "all" {
        Run-AllTests
    }
    "individual" {
        Run-IndividualTests
    }
    "report" {
        Generate-Report
    }
    "study" {
        Run-Test "study-service" "com.OhRyue.certpilot.study.controller.VersusQuestionControllerTest" `
            "study-service API 테스트"
    }
    "progress" {
        Run-Test "progress-service" "com.OhRyue.certpilot.progress.controller.VersusResultControllerTest" `
            "progress-service API 테스트"
    }
    "versus" {
        Run-Test "versus-service" "com.OhRyue.certpilot.versus.integration.VersusServiceE2ETest" `
            "versus-service E2E 테스트"
    }
    default {
        Write-Host "사용법: .\run-tests.ps1 [all|individual|report|study|progress|versus]" -ForegroundColor Yellow
        Write-Host ""
        Write-Host "옵션:"
        Write-Host "  all        - 모든 서비스의 전체 테스트 실행"
        Write-Host "  individual - 개별 API/E2E 테스트만 실행"
        Write-Host "  report     - 테스트 리포트 생성"
        Write-Host "  study      - study-service 테스트만 실행"
        Write-Host "  progress   - progress-service 테스트만 실행"
        Write-Host "  versus     - versus-service 테스트만 실행"
        exit 1
    }
}





