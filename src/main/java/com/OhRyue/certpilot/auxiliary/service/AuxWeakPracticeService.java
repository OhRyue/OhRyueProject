package com.OhRyue.certpilot.aux.service;

import com.OhRyue.certpilot.progress.service.ProgressService;
import com.OhRyue.certpilot.progress.web.dto.ProgressOverviewDto;
import com.OhRyue.certpilot.quiz.service.QuizQuickService;
import com.OhRyue.certpilot.quiz.web.dto.QuickSessionDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Comparator;
import java.util.List;

/**
 * 약점 태그(EMA 낮음/오답 많음) 기반으로 Quick 세션을 시작한다.
 * - 태그 선택 규칙: EMA 오름차순(낮을수록 약점) → 오답수 내림차순
 */
@Service
@RequiredArgsConstructor
public class AuxWeakPracticeService {

    private final ProgressService progressService;
    private final QuizQuickService quickService;

    @Transactional(readOnly = true)
    public QuickSessionDto startWeakPractice(Long userId, int count) {
        ProgressOverviewDto ov = progressService.overview(userId);

        // 태그 스코어 정렬: EMA 오름차순(NULL은 최하위 취급), wrongCount 내림차순
        var sorted = ov.tagStats().stream()
                .sorted(Comparator
                        .comparing((ProgressOverviewDto.TagStat t) -> t.ema() == null ? -1.0 : t.ema()) // 낮은 EMA 먼저
                        .thenComparing(ProgressOverviewDto.TagStat::wrongCount, Comparator.reverseOrder()))
                .toList();

        // 상위 3개 태그로 세션 구성 (부족하면 있는 만큼)
        List<String> tags = sorted.stream().limit(3).map(ProgressOverviewDto.TagStat::tag).toList();
        int useCount = Math.max(3, Math.min(count, 10)); // 3~10 사이

        return quickService.createSession(tags, useCount);
    }
}
