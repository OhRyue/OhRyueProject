package com.OhRyue.certpilot.learn.service;

import com.OhRyue.certpilot.shared.dto.LearnSessionDto;
import com.OhRyue.certpilot.shared.dto.QuestionDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;

@Service
@RequiredArgsConstructor
public class LearnOrchestrationService {

  private final QuestionFacade questionFacade;

  public LearnSessionDto startSession(int page, int size) {
    List<QuestionDto> questions = questionFacade.list(page, size);
    return LearnSessionDto.ofNow(questions);
  }
}
