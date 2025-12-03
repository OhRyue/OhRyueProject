package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.dto.InternalDtos;
import com.OhRyue.certpilot.versus.service.VersusInternalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

@Tag(name = "Versus - Internal", description = "내부 서비스 전용 API")
@RestController
@RequestMapping("/api/versus/internal")
@RequiredArgsConstructor
public class VersusInternalController {

    private final VersusInternalService internalService;

    @Operation(summary = "매치 상세 조회 (내부 API)", 
               description = "progress-service에서 호출하여 매치의 문제/답안/정답 정보를 조회")
    @GetMapping("/matches/{matchId}/detail")
    public ResponseEntity<InternalDtos.MatchDetailDto> getMatchDetail(
            @PathVariable Long matchId,
            @RequestParam("userId") String userId
    ) {
        return ResponseEntity.ok(internalService.getMatchDetail(matchId, userId));
    }
}



