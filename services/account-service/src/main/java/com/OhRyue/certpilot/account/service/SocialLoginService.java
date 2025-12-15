package com.OhRyue.certpilot.account.service;

import com.google.api.client.googleapis.auth.oauth2.GoogleIdToken;
import com.google.api.client.googleapis.auth.oauth2.GoogleIdTokenVerifier;
import com.google.api.client.http.javanet.NetHttpTransport;
import com.google.api.client.json.gson.GsonFactory;
import com.OhRyue.certpilot.account.domain.AccountStatus;
import com.OhRyue.certpilot.account.domain.SignupType;
import com.OhRyue.certpilot.account.domain.SocialAccount;
import com.OhRyue.certpilot.account.domain.SocialProvider;
import com.OhRyue.certpilot.account.domain.UserAccount;
import com.OhRyue.certpilot.account.repo.SocialAccountRepository;
import com.OhRyue.certpilot.account.repo.UserAccountRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.client.RestClientException;
import org.springframework.web.client.RestTemplate;

import java.time.LocalDateTime;
import java.util.Collections;
import java.util.Locale;
import java.util.Map;
import java.util.Optional;

@Slf4j
@Service
@RequiredArgsConstructor
public class SocialLoginService {

    private final UserAccountRepository userAccountRepository;
    private final SocialAccountRepository socialAccountRepository;
    private final RestTemplate restTemplate = new RestTemplate();

    @Value("${google.oauth2.client-id}")
    private String googleClientId;

    @Value("${naver.oauth2.client-id}")
    private String naverClientId;

    @Value("${naver.oauth2.client-secret}")
    private String naverClientSecret;

    /**
     * 구글 소셜 로그인 처리 (가입/로그인)
     * - 소셜 계정이 이미 존재하는 경우 → 로그인
     * - 소셜 계정이 없고, 동일한 email의 user_account가 존재하는 경우 → 예외 반환
     * - 둘 다 없는 경우 → 신규 가입
     * 
     * @param idToken 구글 id_token
     * @return UserAccount (기존 계정이 있으면 기존 계정, 없으면 새로 생성)
     */
    @Transactional
    public UserAccount loginWithGoogle(String idToken) {
        // 1. id_token 검증 및 사용자 정보 추출
        GoogleUserInfo googleUserInfo = verifyAndExtractGoogleIdToken(idToken);
        
        String googleId = googleUserInfo.getSub();
        String email = normalizeEmail(googleUserInfo.getEmail());
        String name = googleUserInfo.getName();

        log.info("🔍 [Google Login] id_token 검증 완료 - email: {}, name: {}", email, name);

        // 2. 소셜 계정으로 이미 가입된 경우 확인
        Optional<SocialAccount> existingSocialAccount = socialAccountRepository
                .findByProviderAndProviderId(SocialProvider.GOOGLE, googleId);

        if (existingSocialAccount.isPresent()) {
            // 기존 소셜 계정이 있으면 해당 사용자 반환 (로그인)
            String userId = existingSocialAccount.get().getUserId();
            UserAccount user = userAccountRepository.findById(userId)
                    .orElseThrow(() -> new IllegalArgumentException("소셜 계정에 연결된 사용자를 찾을 수 없습니다."));
            
            user.setLastLoginAt(LocalDateTime.now());
            log.info("✅ [Google Login] 기존 소셜 계정으로 로그인 - userId: {}, email: {}", userId, email);
            return userAccountRepository.save(user);
        }

        // 3. 이메일로 기존 계정 확인
        Optional<UserAccount> existingUserByEmail = userAccountRepository.findByEmail(email);
        
        if (existingUserByEmail.isPresent()) {
            // 이미 이메일로 가입된 계정이 있으면 예외 반환 (자동 연결 금지)
            log.warn("❌ [Google Login] 이미 이메일로 가입된 계정 존재 - email: {}", email);
            throw new IllegalArgumentException("이미 이메일로 가입된 계정입니다. 이메일 로그인 후 소셜 계정을 연결해주세요.");
        }

        // 4. 새 계정 생성
        log.info("✨ [Google Login] 새 계정 생성 - email: {}, name: {}", email, name);
        
        // userId는 이메일의 @ 앞부분을 사용 (중복 시 숫자 추가)
        String userId = generateUserIdFromEmail(email);
        
        UserAccount newUser = UserAccount.builder()
                .id(userId)
                .email(email)
                .passwordHash(null) // 소셜 로그인은 비밀번호 없음
                .status(AccountStatus.ACTIVE)
                .signupType(SignupType.SOCIAL)
                .createdAt(LocalDateTime.now())
                .lastLoginAt(LocalDateTime.now())
                .build();
        
        UserAccount savedUser = userAccountRepository.save(newUser);
        
        // 소셜 계정 정보 저장
        SocialAccount socialAccount = SocialAccount.builder()
                .userId(savedUser.getId())
                .provider(SocialProvider.GOOGLE)
                .providerId(googleId)
                .createdAt(LocalDateTime.now())
                .build();
        socialAccountRepository.save(socialAccount);
        
        log.info("✅ [Google Login] 신규 계정 생성 완료 - userId: {}, email: {}", savedUser.getId(), email);
        return savedUser;
    }

    /**
     * 구글 소셜 계정 연결 (로그인 상태에서만 가능)
     * - 현재 로그인한 사용자 계정에 구글 소셜 계정을 연결
     * 
     * @param userId 현재 로그인한 사용자 ID
     * @param idToken 구글 id_token
     */
    @Transactional
    public void connectGoogleAccount(String userId, String idToken) {
        // 1. id_token 검증 및 사용자 정보 추출
        GoogleUserInfo googleUserInfo = verifyAndExtractGoogleIdToken(idToken);
        
        String googleId = googleUserInfo.getSub();
        String email = normalizeEmail(googleUserInfo.getEmail());

        log.info("🔗 [Google Connect] 소셜 계정 연결 요청 - userId: {}, email: {}", userId, email);

        // 2. 현재 사용자 계정 확인
        UserAccount user = userAccountRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("사용자를 찾을 수 없습니다."));

        // 3. 이미 해당 소셜 계정이 다른 사용자에게 연결되어 있는지 확인
        Optional<SocialAccount> existingSocialAccount = socialAccountRepository
                .findByProviderAndProviderId(SocialProvider.GOOGLE, googleId);
        
        if (existingSocialAccount.isPresent()) {
            String existingUserId = existingSocialAccount.get().getUserId();
            if (!existingUserId.equals(userId)) {
                throw new IllegalArgumentException("이미 다른 계정에 연결된 구글 계정입니다.");
            }
            // 이미 현재 사용자에게 연결되어 있으면 성공 처리
            log.info("✅ [Google Connect] 이미 연결된 소셜 계정 - userId: {}", userId);
            return;
        }

        // 4. 이메일 일치 확인 (보안을 위해)
        String userEmail = normalizeEmail(user.getEmail());
        if (userEmail != null && !userEmail.equals(email)) {
            log.warn("⚠️ [Google Connect] 이메일 불일치 - userId: {}, userEmail: {}, googleEmail: {}", 
                    userId, userEmail, email);
            // 이메일이 다르더라도 연결은 허용 (사용자가 다른 이메일로 가입했을 수 있음)
        }

        // 5. 소셜 계정 정보 저장
        SocialAccount socialAccount = SocialAccount.builder()
                .userId(userId)
                .provider(SocialProvider.GOOGLE)
                .providerId(googleId)
                .createdAt(LocalDateTime.now())
                .build();
        socialAccountRepository.save(socialAccount);
        
        log.info("✅ [Google Connect] 소셜 계정 연결 완료 - userId: {}, googleId: {}", userId, googleId);
    }

    /**
     * 구글 id_token 검증 및 사용자 정보 추출
     * - 토큰 서명 검증
     * - aud(Client ID) 검증
     * - exp 만료 검증
     * - sub, email, name 추출
     */
    private GoogleUserInfo verifyAndExtractGoogleIdToken(String idTokenString) {
        if (googleClientId == null || googleClientId.isBlank()) {
            throw new IllegalArgumentException("Google Client ID가 설정되지 않았습니다.");
        }

        try {
            NetHttpTransport transport = new NetHttpTransport();
            GsonFactory jsonFactory = new GsonFactory();

            GoogleIdTokenVerifier verifier = new GoogleIdTokenVerifier.Builder(transport, jsonFactory)
                    .setAudience(Collections.singletonList(googleClientId))
                    .build();

            GoogleIdToken idToken = verifier.verify(idTokenString);
            
            if (idToken == null) {
                log.error("❌ [Google Login] id_token 검증 실패 - 토큰이 null입니다.");
                throw new IllegalArgumentException("유효하지 않은 구글 id_token입니다.");
            }

            GoogleIdToken.Payload payload = idToken.getPayload();

            // sub (Google 사용자 ID)
            String sub = payload.getSubject();
            if (sub == null || sub.isBlank()) {
                throw new IllegalArgumentException("id_token에 sub 클레임이 없습니다.");
            }

            // email
            String email = (String) payload.get("email");
            if (email == null || email.isBlank()) {
                throw new IllegalArgumentException("id_token에 email 클레임이 없습니다.");
            }

            // name
            String name = (String) payload.get("name");

            log.debug("✅ [Google Login] id_token 검증 성공 - sub: {}, email: {}, name: {}", sub, email, name);

            return GoogleUserInfo.builder()
                    .sub(sub)
                    .email(email)
                    .name(name)
                    .build();

        } catch (IllegalArgumentException e) {
            log.error("❌ [Google Login] id_token 검증 실패 - error: {}", e.getMessage());
            throw e;
        } catch (Exception e) {
            log.error("❌ [Google Login] id_token 검증 중 오류 발생 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("구글 id_token 검증에 실패했습니다: " + e.getMessage());
        }
    }

    /**
     * 이메일에서 userId 생성 (중복 시 숫자 추가)
     */
    private String generateUserIdFromEmail(String email) {
        String baseUserId = email.split("@")[0].toLowerCase(Locale.ROOT)
                .replaceAll("[^a-z0-9]", ""); // 영문자와 숫자만 허용
        
        String userId = baseUserId;
        int suffix = 1;
        
        while (userAccountRepository.findById(userId).isPresent()) {
            userId = baseUserId + suffix;
            suffix++;
        }
        
        return userId;
    }

    private String normalizeEmail(String email) {
        return email == null ? null : email.trim().toLowerCase(Locale.ROOT);
    }

    /**
     * 카카오 소셜 로그인 처리 (가입/로그인)
     * - 소셜 계정이 이미 존재하는 경우 → 로그인
     * - 소셜 계정이 없고, 동일한 email의 user_account가 존재하는 경우 → 예외 반환
     * - 둘 다 없는 경우 → 신규 가입
     * 
     * @param accessToken 카카오 access_token
     * @return UserAccount (기존 계정이 있으면 기존 계정, 없으면 새로 생성)
     */
    @Transactional
    public UserAccount loginWithKakao(String accessToken) {
        KakaoUserInfo kakaoUserInfo = fetchKakaoUserInfo(accessToken);
        
        String kakaoId = kakaoUserInfo.getId().toString();
        String email = normalizeEmail(kakaoUserInfo.getEmail());
        String nickname = kakaoUserInfo.getNickname();

        log.info("🔍 [Kakao Login] 사용자 정보 조회 완료 - email: {}, nickname: {}", email, nickname);

        Optional<SocialAccount> existingSocialAccount = socialAccountRepository
                .findByProviderAndProviderId(SocialProvider.KAKAO, kakaoId);

        if (existingSocialAccount.isPresent()) {
            String userId = existingSocialAccount.get().getUserId();
            UserAccount user = userAccountRepository.findById(userId)
                    .orElseThrow(() -> new IllegalArgumentException("소셜 계정에 연결된 사용자를 찾을 수 없습니다."));
            
            user.setLastLoginAt(LocalDateTime.now());
            log.info("✅ [Kakao Login] 기존 소셜 계정으로 로그인 - userId: {}, email: {}", userId, email);
            return userAccountRepository.save(user);
        }

        if (email != null) {
            Optional<UserAccount> existingUserByEmail = userAccountRepository.findByEmail(email);
            
            if (existingUserByEmail.isPresent()) {
                log.warn("❌ [Kakao Login] 이미 이메일로 가입된 계정 존재 - email: {}", email);
                throw new IllegalArgumentException("이미 이메일로 가입된 계정입니다. 이메일 로그인 후 소셜 계정을 연결해주세요.");
            }
        }

        log.info("✨ [Kakao Login] 새 계정 생성 - email: {}, nickname: {}", email, nickname);
        
        String userId;
        if (email != null) {
            userId = generateUserIdFromEmail(email);
        } else {
            userId = generateUserIdFromKakaoId(kakaoId);
        }
        
        UserAccount newUser = UserAccount.builder()
                .id(userId)
                .email(email)
                .passwordHash(null)
                .status(AccountStatus.ACTIVE)
                .signupType(SignupType.SOCIAL)
                .createdAt(LocalDateTime.now())
                .lastLoginAt(LocalDateTime.now())
                .build();
        
        UserAccount savedUser = userAccountRepository.save(newUser);
        
        SocialAccount socialAccount = SocialAccount.builder()
                .userId(savedUser.getId())
                .provider(SocialProvider.KAKAO)
                .providerId(kakaoId)
                .createdAt(LocalDateTime.now())
                .build();
        socialAccountRepository.save(socialAccount);
        
        log.info("✅ [Kakao Login] 신규 계정 생성 완료 - userId: {}, email: {}", savedUser.getId(), email);
        return savedUser;
    }

    private KakaoUserInfo fetchKakaoUserInfo(String accessToken) {
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", "Bearer " + accessToken);
            HttpEntity<String> entity = new HttpEntity<>(headers);

            ResponseEntity<Map> response = restTemplate.exchange(
                    "https://kapi.kakao.com/v2/user/me",
                    HttpMethod.GET,
                    entity,
                    Map.class
            );

            if (response.getStatusCode().is2xxSuccessful() && response.getBody() != null) {
                Map<String, Object> body = response.getBody();
                
                if (body.get("id") == null) {
                    log.error("❌ [Kakao Login] 카카오 API 응답에 id가 없습니다.");
                    throw new IllegalArgumentException("카카오 사용자 정보에 id가 없습니다.");
                }
                
                Long id = Long.valueOf(body.get("id").toString());
                
                Map<String, Object> kakaoAccount = (Map<String, Object>) body.get("kakao_account");
                String email = null;
                String nickname = null;
                
                if (kakaoAccount != null) {
                    email = kakaoAccount.get("email") != null ? kakaoAccount.get("email").toString() : null;
                    
                    Map<String, Object> profile = (Map<String, Object>) kakaoAccount.get("profile");
                    if (profile != null) {
                        nickname = profile.get("nickname") != null ? profile.get("nickname").toString() : null;
                    }
                }

                return KakaoUserInfo.builder()
                        .id(id)
                        .email(email)
                        .nickname(nickname)
                        .build();
            } else {
                log.error("❌ [Kakao Login] 카카오 API 응답 실패 - status: {}", response.getStatusCode());
                throw new IllegalArgumentException("카카오 사용자 정보 조회에 실패했습니다.");
            }
        } catch (RestClientException e) {
            log.error("❌ [Kakao Login] 카카오 API 호출 실패 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("카카오 access_token이 유효하지 않거나 API 호출에 실패했습니다: " + e.getMessage());
        } catch (Exception e) {
            log.error("❌ [Kakao Login] 카카오 사용자 정보 파싱 실패 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("카카오 사용자 정보를 가져오는 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    private String generateUserIdFromKakaoId(String kakaoId) {
        String baseUserId = "kakao_" + kakaoId;
        String userId = baseUserId;
        int suffix = 1;
        
        while (userAccountRepository.findById(userId).isPresent()) {
            userId = baseUserId + "_" + suffix;
            suffix++;
        }
        
        return userId;
    }

    /**
     * 네이버 소셜 로그인 처리 (가입/로그인)
     * - 소셜 계정이 이미 존재하는 경우 → 로그인
     * - 소셜 계정이 없고, email이 존재하며 동일 email의 user_account가 존재하는 경우 → 예외 반환
     * - email이 없으면 → provider_id 기준으로 신규 회원가입 허용
     * - 둘 다 없는 경우 → 신규 가입
     * 
     * @param code 네이버 authorization code
     * @param state 네이버 state 값
     * @return UserAccount (기존 계정이 있으면 기존 계정, 없으면 새로 생성)
     */
    @Transactional
    public UserAccount loginWithNaver(String code, String state) {
        log.info("🔍 [Naver Login] 네이버 로그인 시작 - code: {}, state: {}", code != null ? code.substring(0, Math.min(10, code.length())) + "..." : "null", state);
        
        String accessToken = exchangeNaverCodeForToken(code, state);
        NaverUserInfo naverUserInfo = fetchNaverUserInfo(accessToken);
        
        String naverId = naverUserInfo.getId();
        String email = normalizeEmail(naverUserInfo.getEmail());
        String name = naverUserInfo.getName();

        log.info("🔍 [Naver Login] 사용자 정보 조회 완료 - naverId: {}, email: {}, name: {}", naverId, email, name);

        Optional<SocialAccount> existingSocialAccount = socialAccountRepository
                .findByProviderAndProviderId(SocialProvider.NAVER, naverId);

        if (existingSocialAccount.isPresent()) {
            String userId = existingSocialAccount.get().getUserId();
            UserAccount user = userAccountRepository.findById(userId)
                    .orElseThrow(() -> new IllegalArgumentException("소셜 계정에 연결된 사용자를 찾을 수 없습니다."));
            
            user.setLastLoginAt(LocalDateTime.now());
            log.info("✅ [Naver Login] 기존 소셜 계정으로 로그인 - userId: {}, email: {}", userId, email);
            return userAccountRepository.save(user);
        }

        if (email != null) {
            Optional<UserAccount> existingUserByEmail = userAccountRepository.findByEmail(email);
            
            if (existingUserByEmail.isPresent()) {
                log.warn("❌ [Naver Login] 이미 이메일로 가입된 계정 존재 - email: {}", email);
                throw new IllegalArgumentException("이미 이메일로 가입된 계정입니다. 이메일 로그인 후 소셜 계정을 연결해주세요.");
            }
        }

        log.info("✨ [Naver Login] 새 계정 생성 - email: {}, name: {}", email, name);
        
        String userId;
        if (email != null) {
            userId = generateUserIdFromEmail(email);
        } else {
            userId = generateUserIdFromNaverId(naverId);
        }
        
        UserAccount newUser = UserAccount.builder()
                .id(userId)
                .email(email)
                .passwordHash(null)
                .status(AccountStatus.ACTIVE)
                .signupType(SignupType.SOCIAL)
                .createdAt(LocalDateTime.now())
                .lastLoginAt(LocalDateTime.now())
                .build();
        
        UserAccount savedUser = userAccountRepository.save(newUser);
        
        SocialAccount socialAccount = SocialAccount.builder()
                .userId(savedUser.getId())
                .provider(SocialProvider.NAVER)
                .providerId(naverId)
                .createdAt(LocalDateTime.now())
                .build();
        socialAccountRepository.save(socialAccount);
        
        log.info("✅ [Naver Login] 신규 계정 생성 완료 - userId: {}, email: {}", savedUser.getId(), email);
        return savedUser;
    }

    private String exchangeNaverCodeForToken(String code, String state) {
        if (naverClientId == null || naverClientId.isBlank() || 
            naverClientSecret == null || naverClientSecret.isBlank()) {
            log.error("❌ [Naver Login] 네이버 OAuth 설정이 완료되지 않았습니다. - clientId: {} (null: {}, blank: {}), clientSecret: {} (null: {}, blank: {})", 
                    naverClientId != null ? "'" + naverClientId + "'" : "null", 
                    naverClientId == null,
                    naverClientId != null && naverClientId.isBlank(),
                    naverClientSecret != null ? "'" + naverClientSecret + "'" : "null",
                    naverClientSecret == null,
                    naverClientSecret != null && naverClientSecret.isBlank());
            throw new IllegalArgumentException("네이버 OAuth 설정이 완료되지 않았습니다. NAVER_CLIENT_ID와 NAVER_CLIENT_SECRET 환경 변수를 확인해주세요.");
        }

        try {
            String url = String.format(
                "https://nid.naver.com/oauth2.0/token?grant_type=authorization_code&client_id=%s&client_secret=%s&code=%s&state=%s",
                naverClientId, naverClientSecret, code, state
            );

            log.debug("🔍 [Naver Login] 토큰 교환 요청 - code: {}, state: {}", code, state);
            ResponseEntity<Map> response = restTemplate.exchange(
                    url,
                    HttpMethod.GET,
                    null,
                    Map.class
            );

            if (response.getStatusCode().is2xxSuccessful() && response.getBody() != null) {
                Map<String, Object> body = response.getBody();
                
                if (body.get("access_token") == null) {
                    log.error("❌ [Naver Login] 네이버 토큰 교환 응답에 access_token이 없습니다. - 응답: {}", body);
                    String error = body.get("error") != null ? body.get("error").toString() : null;
                    String errorDescription = body.get("error_description") != null ? body.get("error_description").toString() : null;
                    throw new IllegalArgumentException("네이버 토큰 교환에 실패했습니다." + 
                            (error != null ? " 오류: " + error : "") + 
                            (errorDescription != null ? " 설명: " + errorDescription : ""));
                }
                
                String accessToken = body.get("access_token").toString();
                log.debug("✅ [Naver Login] 토큰 교환 성공");
                return accessToken;
            } else {
                log.error("❌ [Naver Login] 네이버 토큰 교환 실패 - status: {}, body: {}", response.getStatusCode(), response.getBody());
                throw new IllegalArgumentException("네이버 토큰 교환에 실패했습니다. 상태 코드: " + response.getStatusCode());
            }
        } catch (IllegalArgumentException e) {
            throw e;
        } catch (RestClientException e) {
            log.error("❌ [Naver Login] 네이버 토큰 교환 API 호출 실패 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("네이버 authorization code가 유효하지 않거나 API 호출에 실패했습니다: " + e.getMessage());
        } catch (Exception e) {
            log.error("❌ [Naver Login] 네이버 토큰 교환 중 오류 발생 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("네이버 토큰 교환 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    private NaverUserInfo fetchNaverUserInfo(String accessToken) {
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.set("Authorization", "Bearer " + accessToken);
            HttpEntity<String> entity = new HttpEntity<>(headers);

            ResponseEntity<Map> response = restTemplate.exchange(
                    "https://openapi.naver.com/v1/nid/me",
                    HttpMethod.GET,
                    entity,
                    Map.class
            );

            if (response.getStatusCode().is2xxSuccessful() && response.getBody() != null) {
                Map<String, Object> body = response.getBody();
                
                String resultcode = body.get("resultcode") != null ? body.get("resultcode").toString() : null;
                String message = body.get("message") != null ? body.get("message").toString() : null;
                
                boolean isEmailNotVerified = "024".equals(resultcode) || 
                                           (message != null && (message.contains("email_not_verified") || message.contains("이메일")));
                
                Map<String, Object> responseData = (Map<String, Object>) body.get("response");
                
                if (responseData == null) {
                    log.error("❌ [Naver Login] 네이버 API 응답에 response가 없습니다. - resultcode: {}, message: {}", resultcode, message);
                    throw new IllegalArgumentException("네이버 사용자 정보에 response가 없습니다.");
                }
                
                if (responseData.get("id") == null) {
                    log.error("❌ [Naver Login] 네이버 API 응답에 id가 없습니다.");
                    throw new IllegalArgumentException("네이버 사용자 정보에 id가 없습니다.");
                }
                
                String id = responseData.get("id").toString();
                String email = responseData.get("email") != null ? responseData.get("email").toString() : null;
                String name = responseData.get("name") != null ? responseData.get("name").toString() : null;

                if (isEmailNotVerified) {
                    log.info("ℹ️ [Naver Login] 네이버 이메일 미인증 상태 - id: {}, email: null (정상 처리)", id);
                    email = null;
                }

                return NaverUserInfo.builder()
                        .id(id)
                        .email(email)
                        .name(name)
                        .build();
            } else {
                log.error("❌ [Naver Login] 네이버 API 응답 실패 - status: {}", response.getStatusCode());
                throw new IllegalArgumentException("네이버 사용자 정보 조회에 실패했습니다.");
            }
        } catch (IllegalArgumentException e) {
            throw e;
        } catch (RestClientException e) {
            log.error("❌ [Naver Login] 네이버 API 호출 실패 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("네이버 access_token이 유효하지 않거나 API 호출에 실패했습니다: " + e.getMessage());
        } catch (Exception e) {
            log.error("❌ [Naver Login] 네이버 사용자 정보 파싱 실패 - error: {}", e.getMessage(), e);
            throw new IllegalArgumentException("네이버 사용자 정보를 가져오는 중 오류가 발생했습니다: " + e.getMessage());
        }
    }

    private String generateUserIdFromNaverId(String naverId) {
        String baseUserId = "naver_" + naverId;
        String userId = baseUserId;
        int suffix = 1;
        
        while (userAccountRepository.findById(userId).isPresent()) {
            userId = baseUserId + "_" + suffix;
            suffix++;
        }
        
        return userId;
    }

    /**
     * 구글 사용자 정보 DTO
     */
    @lombok.Data
    @lombok.Builder
    private static class GoogleUserInfo {
        private String sub;  // Google 사용자 ID
        private String email;
        private String name;
    }

    /**
     * 카카오 사용자 정보 DTO
     */
    @lombok.Data
    @lombok.Builder
    private static class KakaoUserInfo {
        private Long id;
        private String email;
        private String nickname;
    }

    /**
     * 네이버 사용자 정보 DTO
     */
    @lombok.Data
    @lombok.Builder
    private static class NaverUserInfo {
        private String id;
        private String email;
        private String name;
    }
}
