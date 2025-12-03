package com.OhRyue.certpilot.study.controller;

import com.OhRyue.certpilot.study.dto.InternalDtos;
import com.OhRyue.certpilot.study.service.StudyInternalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Study - Internal", description = "내부 서비스 전용 API")
@RestController
@RequestMapping("/api/study/internal")
@RequiredArgsConstructor
public class StudyInternalController {

    private final StudyInternalService internalService;

    @Operation(summary = "세션 상세 조회 (내부 API)", 
               description = "progress-service에서 호출하여 세션의 문제/답안/정답 정보를 조회")
    @GetMapping("/sessions/{sessionId}/detail")
    public ResponseEntity<InternalDtos.StudySessionDetailDto> getSessionDetail(
            @PathVariable Long sessionId,
            @RequestParam("userId") String userId
    ) {
        return ResponseEntity.ok(internalService.getSessionDetail(sessionId, userId));
    }
}



