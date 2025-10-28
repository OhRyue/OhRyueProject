package com.OhRyue.certpilot.progress.service;

import com.OhRyue.certpilot.progress.web.dto.ProgressOverviewDto;
import com.OhRyue.certpilot.wrongnote.domain.repo.WrongNoteRepository;
import com.OhRyue.certpilot.ability.domain.AbilityProfile;
import com.OhRyue.certpilot.ability.domain.repo.AbilityProfileRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.time.Instant;
import java.util.*;

@Service
@RequiredArgsConstructor
public class ProgressService {

    private final AbilityProfileRepository abilityRepo;
    private final WrongNoteRepository wrongRepo;

    /**
     * 유저의 태그별 능력/오답 현황 + 최근 오답 목록을 반환.
     */
    @Transactional(readOnly = true)
    public ProgressOverviewDto overview(Long userId) {
        // 태그별 능력
        var aps = abilityRepo.findByUserIdOrderByEmaCorrectAsc(userId);

        List<ProgressOverviewDto.TagStat> tagStats = new ArrayList<>();
        for (AbilityProfile ap : aps) {
            String tag = ap.getTag();
            Double ema = safeEma(ap);
            int wrongCount = wrongRepo.countByUserIdAndTag(userId, tag);

            tagStats.add(new ProgressOverviewDto.TagStat(tag, ema, wrongCount));
        }

        // 최근 오답 10개
        var recent = wrongRepo.findTopByUserIdOrderByLastWrongAtDesc(userId).stream()
                .map(wn -> new ProgressOverviewDto.RecentWrong(
                        wn.getQuestionId(),
                        wn.getTag(),
                        wn.getLastWrongAt()
                ))
                .toList();

        return new ProgressOverviewDto(userId, tagStats, recent, Instant.now());
    }

    /** 엔티티의 EMA/점수 필드 꺼내는 헬퍼 */
    private Double safeEma(AbilityProfile ap) {
        try {
            // 가장 흔한 케이스: getEma() 또는 getScore()
            var m = Arrays.stream(ap.getClass().getMethods())
                    .filter(mm -> mm.getName().equals("getEma") || mm.getName().equals("getScore"))
                    .findFirst().orElse(null);
            if (m != null) {
                Object v = m.invoke(ap);
                if (v instanceof Number n) return n.doubleValue();
            }
        } catch (Exception ignore) {}
        // 없으면 null
        return null;
    }
}
