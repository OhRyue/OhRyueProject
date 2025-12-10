package com.OhRyue.certpilot.account.exception;

/**
 * 온보딩이 완료되지 않은 사용자가 온보딩이 필요한 API에 접근할 때 발생하는 예외
 */
public class OnboardingRequiredException extends RuntimeException {
    
    public OnboardingRequiredException() {
        super("온보딩이 필요한 사용자입니다.");
    }
    
    public OnboardingRequiredException(String message) {
        super(message);
    }
}









