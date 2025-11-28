package com.OhRyue.certpilot.progress.controller;

import com.OhRyue.certpilot.progress.dto.VersusDtos;
import com.OhRyue.certpilot.progress.service.VersusResultService;
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
import static org.mockito.Mockito.doNothing;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest(VersusResultController.class)
@WithMockUser
class VersusResultControllerTest {

    @Autowired
    private MockMvc mockMvc;

    @MockBean
    private VersusResultService versusResultService;

    @Autowired
    private ObjectMapper objectMapper;

    @Test
    @DisplayName("매치 결과 기록 - DUEL 모드")
    void testRecordVersusResult_Duel() throws Exception {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
            "DUEL",
            123L,
            "user1",
            List.of(
                new VersusDtos.ParticipantResult("user1", 8500, 1, 8, 10, 45000L),
                new VersusDtos.ParticipantResult("user2", 7200, 2, 7, 10, 50000L)
            ),
            10,
            120000L
        );

        doNothing().when(versusResultService).recordVersusResult(any());

        // When & Then
        mockMvc.perform(post("/api/progress/versus/result")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNoContent());
    }

    @Test
    @DisplayName("매치 결과 기록 - TOURNAMENT 모드")
    void testRecordVersusResult_Tournament() throws Exception {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
            "TOURNAMENT",
            124L,
            "user1",
            List.of(
                new VersusDtos.ParticipantResult("user1", 15000, 1, 9, 9, 60000L),
                new VersusDtos.ParticipantResult("user2", 12000, 2, 7, 9, 70000L)
            ),
            9,
            180000L
        );

        doNothing().when(versusResultService).recordVersusResult(any());

        // When & Then
        mockMvc.perform(post("/api/progress/versus/result")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNoContent());
    }

    @Test
    @DisplayName("매치 결과 기록 - GOLDENBELL 모드")
    void testRecordVersusResult_Goldenbell() throws Exception {
        // Given
        VersusDtos.VersusResultRequest request = new VersusDtos.VersusResultRequest(
            "GOLDENBELL",
            125L,
            "user1",
            List.of(
                new VersusDtos.ParticipantResult("user1", 20000, 1, 6, 6, 55000L)
            ),
            6,
            60000L
        );

        doNothing().when(versusResultService).recordVersusResult(any());

        // When & Then
        mockMvc.perform(post("/api/progress/versus/result")
                .contentType(MediaType.APPLICATION_JSON)
                .content(objectMapper.writeValueAsString(request)))
            .andExpect(status().isNoContent());
    }
}






