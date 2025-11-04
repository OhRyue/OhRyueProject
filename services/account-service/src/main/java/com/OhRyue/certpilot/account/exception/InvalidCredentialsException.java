package com.OhRyue.certpilot.account.exception;

/*
    로그인 실패 전용 exception
    - 아이디/비밀번호 틀렸을 시 던지는 예외 클래스
 */
public class InvalidCredentialsException extends RuntimeException {
    public InvalidCredentialsException() { super("Invalid credentials"); }
}
