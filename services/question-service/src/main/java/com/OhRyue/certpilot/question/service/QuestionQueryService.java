package com.OhRyue.certpilot.question.service;

import com.OhRyue.certpilot.question.domain.question.Question;
import com.OhRyue.certpilot.question.repo.QuestionRepository;
import com.OhRyue.certpilot.shared.dto.PageResponse;
import com.OhRyue.certpilot.shared.dto.QuestionDto;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import lombok.RequiredArgsConstructor;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.Collections;
import java.util.List;

@Service
@Transactional(readOnly = true)
@RequiredArgsConstructor
public class QuestionQueryService {
  private final QuestionRepository questionRepository;
  private final ObjectMapper om = new ObjectMapper();

  public PageResponse<QuestionDto> findAll(Pageable pageable) {
    Page<QuestionDto> page = questionRepository.findAll(pageable)
        .map(this::toDto);
    return PageResponse.of(page);
  }

  public List<QuestionDto> latestByTopics(List<Long> topicIds, int limit) {
    return questionRepository.findLatestByTopicIds(topicIds, limit)
        .stream().map(this::toDto).toList();
  }

  private QuestionDto toDto(Question q) {
    List<String> choices = parseJsonList(q.getChoicesJson());
    return QuestionDto.builder()
        .id(q.getId())
        .stem(q.getStem())
        .choices(choices)
        .difficulty(q.getDifficulty())
        .build();
  }

  private List<String> parseJsonList(String json) {
    try {
      return om.readValue(json, new TypeReference<List<String>>() {});
    } catch (Exception e) {
      return Collections.emptyList();
    }
  }
}
