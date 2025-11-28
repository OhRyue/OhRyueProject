package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.VersusDtos;
import com.OhRyue.certpilot.study.service.VersusQuestionService;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.security.test.context.support.WithMockUser;
import org.springframework.test.web.servlet.MockMvc;

import java.util.List;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@WebMvcTest(VersusQuestionController.class)
@WithMockUser
class VersusQuestionControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private VersusQuestionService versusQuestionService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    @DisplayName("문제 세트 생성 - 성공")
    void testGenerateVersusQuestions_Success() throws Exception {
        // Given
        VersusDtos.VersusQuestionRequest request = new VersusDtos.VersusQuestionRequest(
            "WRITTEN",
            "ALL",
            null,
            "NORMAL",
            10,
            List.of(
                new VersusDtos.QuestionTypeSpec("OX", 2),
                new VersusDtos.QuestionTypeSpec("MCQ", 8)
            )
        );

        VersusDtos.QuestionDto questionDto = new VersusDtos.QuestionDto(
            1L, "WRITTEN", "MCQ", "NORMAL", "문제 내용", "A", "해설", null
        );

        when(versusQuestionService.generateVersusQuestions(any()))
            .thenReturn(List.of(questionDto));

        // When & Then
        mockMvc.perform(post("/api/study/versus/questions")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$[0].id").value(1L))
            .andExpect(jsonPath("$[0].mode").value("WRITTEN"))
            .andExpect(jsonPath("$[0].type").value("MCQ"));
    }

    @Test
    @DisplayName("문제 정보 조회 - 성공")
    void testGetQuestion_Success() throws Exception {
        // Given
        VersusDtos.QuestionDto questionDto = new VersusDtos.QuestionDto(
            1L, "WRITTEN", "MCQ", "NORMAL", "문제 내용", "A", "해설", null
        );

        when(versusQuestionService.getQuestion(1L))
            .thenReturn(questionDto);

        // When & Then
        mockMvc.perform(get("/api/study/versus/questions/1"))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.id").value(1L))
            .andExpect(jsonPath("$.mode").value("WRITTEN"))
            .andExpect(jsonPath("$.answerKey").value("A"));
    }

    @Test
    @DisplayName("정답 검증 - MCQ 정답")
    void testValidateAnswer_McqCorrect() throws Exception {
        // Given
        VersusDtos.UserAnswerDto userAnswer = new VersusDtos.UserAnswerDto("A", "label");
        VersusDtos.AnswerValidationResult result = new VersusDtos.AnswerValidationResult(
            true, "A", "정답입니다."
        );

        when(versusQuestionService.validateAnswer(1L, userAnswer))
            .thenReturn(result);

        // When & Then
        mockMvc.perform(post("/api/study/versus/questions/1/validate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userAnswer)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.correct").value(true))
            .andExpect(jsonPath("$.correctAnswer").value("A"));
    }

    @Test
    @DisplayName("정답 검증 - MCQ 오답")
    void testValidateAnswer_McqWrong() throws Exception {
        // Given
        VersusDtos.UserAnswerDto userAnswer = new VersusDtos.UserAnswerDto("B", "label");
        VersusDtos.AnswerValidationResult result = new VersusDtos.AnswerValidationResult(
            false, "A", "정답은 A입니다."
        );

        when(versusQuestionService.validateAnswer(1L, userAnswer))
            .thenReturn(result);

        // When & Then
        mockMvc.perform(post("/api/study/versus/questions/1/validate")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(userAnswer)))
            .andExpect(status().isOk())
            .andExpect(jsonPath("$.correct").value(false))
            .andExpect(jsonPath("$.correctAnswer").value("A"));
    }
}






