package com.OhRyue.common.auth;

import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;

import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

/**
 * JWT 인증 이후, 현재 로그인한 사용자의 정보를
 * 서비스/컨트롤러에서 편하게 꺼내 쓰기 위한 유틸입니다.
 *
 * - getCurrentUserId() : 필수로 로그인된 상황에서만 사용 (없으면 예외)
 * - getCurrentUserIdOrNull() : 로그인 없으면 null 반환
 * - getCurrentUserRoles() : ROLE 목록 조회
 */
public final class AuthUserUtil {

    private AuthUserUtil() {
        // static util
    }

    /**
     * 현재 SecurityContext에 설정된 userId를 반환합니다.
     * 인증 정보가 없거나, userId를 찾을 수 없으면 IllegalStateException을 던집니다.
     */
    public static String getCurrentUserId() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();

        if (authentication == null) {
            throw new IllegalStateException("인증 정보가 없습니다. (authentication == null)");
        }

        Object principal = authentication.getPrincipal();
        if (principal == null) {
            throw new IllegalStateException("인증 정보에 principal이 없습니다.");
        }

        if (principal instanceof String) {
            String userId = (String) principal;
            if (userId.isBlank()) {
                throw new IllegalStateException("principal이 비어 있습니다.");
            }
            return userId;
        }

        // 혹시 UserDetails 를 쓰는 경우 대비 (추측입니다)
        try {
            Class<?> userDetailsClass = Class.forName("org.springframework.security.core.userdetails.UserDetails");
            if (userDetailsClass.isInstance(principal)) {
                String username = (String) userDetailsClass
                        .getMethod("getUsername")
                        .invoke(principal);
                if (username == null || username.isBlank()) {
                    throw new IllegalStateException("UserDetails.username 이 비어 있습니다.");
                }
                return username;
            }
        } catch (ClassNotFoundException e) {
            // UserDetails 가 classpath 에 없으면 무시
        } catch (Exception reflectionError) {
            throw new IllegalStateException("principal 에서 userId 를 추출하는 중 오류가 발생했습니다.", reflectionError);
        }

        throw new IllegalStateException("지원되지 않는 principal 타입입니다: " + principal.getClass());
    }

    /**
     * 현재 로그인한 userId 를 Optional 느낌으로 쓰고 싶을 때:
     * - 인증 없으면 null 반환
     */
    public static String getCurrentUserIdOrNull() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || authentication.getPrincipal() == null) {
            return null;
        }

        Object principal = authentication.getPrincipal();
        if (principal instanceof String s) {
            return s.isBlank() ? null : s;
        }

        try {
            Class<?> userDetailsClass = Class.forName("org.springframework.security.core.userdetails.UserDetails");
            if (userDetailsClass.isInstance(principal)) {
                String username = (String) userDetailsClass
                        .getMethod("getUsername")
                        .invoke(principal);
                return (username == null || username.isBlank()) ? null : username;
            }
        } catch (Exception ignored) {
            // 인증이 애매한 환경에서 null 로 처리
        }

        return null;
    }

    /**
     * 현재 인증 정보에서 ROLE 목록을 String 리스트로 반환합니다.
     * 예: ["ROLE_USER", "ROLE_ADMIN"]
     */
    public static List<String> getCurrentUserRoles() {
        Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
        if (authentication == null || authentication.getAuthorities() == null) {
            return Collections.emptyList();
        }

        return authentication.getAuthorities().stream()
                .map(GrantedAuthority::getAuthority)
                .collect(Collectors.toList());
    }

    /**
     * 현재 사용자가 특정 ROLE 을 가지고 있는지 검사합니다.
     * 예: hasRole("ROLE_ADMIN")
     */
    public static boolean hasRole(String role) {
        if (role == null || role.isBlank()) {
            return false;
        }
        return getCurrentUserRoles().stream()
                .anyMatch(r -> r.equals(role));
    }
}
