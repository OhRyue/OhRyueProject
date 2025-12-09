package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.domain.TagMaster;
import com.OhRyue.certpilot.study.domain.enums.TagDomain;
import com.OhRyue.common.dto.TagViewDto;
import com.OhRyue.certpilot.study.repository.QuestionTagRepository;
import com.OhRyue.certpilot.study.repository.TagMasterRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collection;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TagQueryService {

  private final TagMasterRepository tagMasterRepository;

  /**
   * 태그 목록 조회
   * 
   * @param domain 도메인 필터 (null이면 전체)
   * @return 태그 목록 (orderNo ASC, code ASC)
   */
  public List<TagViewDto> getTags(String domain) {
    List<TagMaster> entities;
    if (domain == null || domain.isBlank()) {
      entities = tagMasterRepository.findAllOrdered();
    } else {
      try {
        TagDomain tagDomain = TagDomain.valueOf(domain.toUpperCase());
        entities = tagMasterRepository.findByDomainOrderByOrderNoAscCodeAsc(tagDomain);
      } catch (IllegalArgumentException e) {
        throw new IllegalArgumentException("Invalid domain: " + domain + ". Must be WRITTEN or PRACTICAL");
      }
    }

    return entities.stream()
        .map(this::toDto)
        .toList();
  }

  /**
   * 태그 코드 목록으로 태그 정보 조회
   * 
   * @param codes 태그 코드 목록
   * @return 태그 정보 Map (code -> TagViewDto)
   */
  public Map<String, TagViewDto> getTagsByCodes(Collection<String> codes) {
    if (codes == null || codes.isEmpty()) {
      return Map.of();
    }

    List<TagMaster> entities = tagMasterRepository.findByCodeIn(codes);
    return entities.stream()
        .collect(Collectors.toMap(
            TagMaster::getCode,
            this::toDto
        ));
  }

  /**
   * 문제 ID 목록으로 태그 정보 조회 (TagViewDto 포함)
   * 
   * @param questionIds 문제 ID 목록
   * @return 문제 ID -> 태그 목록 매핑 (orderNo, code 순으로 정렬)
   */
  public Map<Long, List<TagViewDto>> getTagsByQuestionIds(Collection<Long> questionIds, QuestionTagRepository questionTagRepository) {
    if (questionIds == null || questionIds.isEmpty()) {
      return Map.of();
    }

    // 1. QuestionTag에서 태그 코드 조회
    List<Object[]> tagResults = questionTagRepository.findTagsByQuestionIds(questionIds);
    Map<Long, List<String>> questionIdToTagCodes = new HashMap<>();
    for (Object[] row : tagResults) {
      Long questionId = (Long) row[0];
      String tagCode = (String) row[1];
      questionIdToTagCodes.computeIfAbsent(questionId, k -> new ArrayList<>()).add(tagCode);
    }

    // 2. 모든 태그 코드 수집
    Set<String> allTagCodes = questionIdToTagCodes.values().stream()
        .flatMap(List::stream)
        .collect(Collectors.toSet());

    if (allTagCodes.isEmpty()) {
      return questionIds.stream().collect(Collectors.toMap(id -> id, id -> List.of()));
    }

    // 3. TagMaster에서 태그 정보 조회
    Map<String, TagViewDto> tagMap = getTagsByCodes(allTagCodes);

    // 4. 문제별 태그 목록 매핑
    Map<Long, List<TagViewDto>> result = new HashMap<>();
    for (Long questionId : questionIds) {
      List<String> tagCodes = questionIdToTagCodes.getOrDefault(questionId, List.of());
      List<TagViewDto> tags = tagCodes.stream()
          .map(tagMap::get)
          .filter(Objects::nonNull)
          .sorted(Comparator.comparing((TagViewDto t) -> t.orderNo() != null ? t.orderNo() : Integer.MAX_VALUE)
              .thenComparing(TagViewDto::code))
          .toList();
      result.put(questionId, tags);
    }

    return result;
  }

  /**
   * TagMaster 엔티티를 TagViewDto로 변환
   */
  private TagViewDto toDto(TagMaster entity) {
    return new TagViewDto(
        entity.getCode(),
        entity.getLabelKo(),
        entity.getLabelEn(),
        entity.getDescription(),
        entity.getDomain().name(),
        entity.getOrderNo()
    );
  }
}

