package com.OhRyue.certpilot.study.service;

import com.OhRyue.certpilot.study.dto.FlowDtos;
import com.OhRyue.certpilot.study.dto.PracticalDtos;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

class PracticalReviewSerializationTest {

  private final ObjectMapper objectMapper = new ObjectMapper();

  @Test
  void practicalReviewSubmitResponse_serializes_withLongIds() {
    // given: review submit payload with Long-only IDs
    PracticalDtos.PracticalSubmitItem item =
        new PracticalDtos.PracticalSubmitItem(1L, true, "base", "ai");
    PracticalDtos.PracticalReviewSubmitResp payload = new PracticalDtos.PracticalReviewSubmitResp(
        10,
        9,
        List.of(item),
        List.of(1L, 2L, 3L)
    );

    FlowDtos.StepEnvelope<PracticalDtos.PracticalReviewSubmitResp> envelope =
        new FlowDtos.StepEnvelope<>(
            99L,
            "REVIEW",
            "SHORT_REVIEW_SET",
            "IN_PROGRESS",
            null,
            Map.of("wrongQuestionIds", List.of(1L, 2L, 3L)),
            payload,
            100L
        );

    // expect: Jackson serialization succeeds without ClassCastException
    assertDoesNotThrow(() -> objectMapper.writeValueAsString(envelope));
  }
}

