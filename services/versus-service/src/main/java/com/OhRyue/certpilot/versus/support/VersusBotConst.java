package com.OhRyue.certpilot.versus.support;

import java.util.Random;

/**
 * Versus 봇 상수 정의
 * - 난이도별 봇 ID 및 닉네임
 * - 난이도별 정답률 및 시간 지연 설정
 */
public final class VersusBotConst {

    /**
     * 봇 난이도
     */
    public enum BotDifficulty {
        // 패자부활전 테스트를 위해 정답률을 낮춤
        // 필요시 더 낮춰서 탈락 확률을 높일 수 있음
        // 딜레이를 더 현실적으로 조정: 최소 2초, 최대 8초
        EASY("EASY", "연습 봇(EASY)", 0.20, 0.40, 2000, 8000),      // 20-40% 정답률, 2-8초 딜레이
        NORMAL("NORMAL", "연습 봇(NORMAL)", 0.30, 0.60, 2000, 7000), // 30-60% 정답률, 2-7초 딜레이
        HARD("HARD", "연습 봇(HARD)", 0.50, 0.80, 2000, 6000);        // 50-80% 정답률, 2-6초 딜레이

        private final String code;
        private final String nickname;
        private final double minCorrectRate;
        private final double maxCorrectRate;
        private final int minDelayMs;
        private final int maxDelayMs;

        BotDifficulty(String code, String nickname, double minCorrectRate, double maxCorrectRate,
                     int minDelayMs, int maxDelayMs) {
            this.code = code;
            this.nickname = nickname;
            this.minCorrectRate = minCorrectRate;
            this.maxCorrectRate = maxCorrectRate;
            this.minDelayMs = minDelayMs;
            this.maxDelayMs = maxDelayMs;
        }

        public String getCode() { return code; }
        public String getNickname() { return nickname; }
        public double getMinCorrectRate() { return minCorrectRate; }
        public double getMaxCorrectRate() { return maxCorrectRate; }
        public int getMinDelayMs() { return minDelayMs; }
        public int getMaxDelayMs() { return maxDelayMs; }

        /**
         * 랜덤 정답 여부 결정
         */
        public boolean decideCorrect(Random random) {
            double rate = minCorrectRate + random.nextDouble() * (maxCorrectRate - minCorrectRate);
            return random.nextDouble() < rate;
        }

        /**
         * 랜덤 딜레이 시간 계산
         */
        public int calculateDelay(Random random) {
            return minDelayMs + random.nextInt(maxDelayMs - minDelayMs + 1);
        }
    }

    /**
     * 봇 UserId 생성
     */
    public static String generateBotUserId(BotDifficulty difficulty, int index) {
        return String.format("BOT_%s_%d", difficulty.getCode(), index);
    }

    /**
     * 봇 닉네임 가져오기
     */
    public static String getBotNickname(BotDifficulty difficulty) {
        return difficulty.getNickname();
    }

    /**
     * UserId에서 난이도 추출
     */
    public static BotDifficulty extractDifficulty(String botUserId) {
        if (botUserId == null || !botUserId.startsWith("BOT_")) {
            return BotDifficulty.EASY; // 기본값
        }
        String[] parts = botUserId.split("_");
        if (parts.length >= 2) {
            try {
                return BotDifficulty.valueOf(parts[1]);
            } catch (IllegalArgumentException e) {
                return BotDifficulty.EASY;
            }
        }
        return BotDifficulty.EASY;
    }

    // 기존 호환성 유지
    public static final String DUEL_BOT_USER_ID = "BOT_EASY_1";
    public static final String DUEL_BOT_NICKNAME = "연습 봇(EASY)";

    private VersusBotConst() {
        // 상수 클래스는 인스턴스화 불가
    }
}
