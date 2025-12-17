package com.OhRyue.certpilot.versus.controller;

import com.OhRyue.certpilot.versus.dto.InternalDtos;
import com.OhRyue.certpilot.versus.repository.MatchParticipantRepository;
import com.OhRyue.certpilot.versus.service.PresenceService;
import com.OhRyue.certpilot.versus.service.VersusInternalService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.time.Duration;
import java.time.Instant;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Tag(name = "Versus - Internal", description = "내부 서비스 전용 API")
@RestController
@RequestMapping("/api/versus/internal")
@RequiredArgsConstructor
public class VersusInternalController {

    private final VersusInternalService internalService;
    private final PresenceService presenceService;
    private final MatchParticipantRepository participantRepository;
    
    @Value("${versus.heartbeat.timeout-seconds:30}")
    private int heartbeatTimeoutSeconds;

    @Operation(summary = "매치 상세 조회 (내부 API)", 
               description = "progress-service에서 호출하여 매치의 문제/답안/정답 정보를 조회")
    @GetMapping("/matches/{matchId}/detail")
    public ResponseEntity<InternalDtos.MatchDetailDto> getMatchDetail(
            @PathVariable Long matchId,
            @RequestParam("userId") String userId
    ) {
        return ResponseEntity.ok(internalService.getMatchDetail(matchId, userId));
    }

    @Operation(summary = "Heartbeat 상태 디버그 조회 (내부 API)",
               description = "특정 방의 참가자별 heartbeat 상태를 조회합니다. " +
                       "Redis의 lastSeen과 DB의 lastHeartbeatAt을 비교하여 동기화 상태를 확인할 수 있습니다.")
    @GetMapping("/rooms/{roomId}/heartbeats")
    public ResponseEntity<Map<String, Object>> getHeartbeatStatus(
            @PathVariable Long roomId
    ) {
        Instant now = Instant.now();
        long timeoutMs = heartbeatTimeoutSeconds * 1000L;
        
        // DB에서 참가자 조회
        List<com.OhRyue.certpilot.versus.domain.MatchParticipant> participants = 
                participantRepository.findByRoomId(roomId);
        
        // 참가자별 heartbeat 상태 수집
        List<Map<String, Object>> participantStatuses = participants.stream()
                .map(participant -> {
                    String userId = participant.getUserId();
                    Instant dbLastHeartbeat = participant.getLastHeartbeatAt();
                    Instant redisLastSeen = presenceService.getLastSeenAt(roomId, userId);
                    
                    long dbDiffMs = dbLastHeartbeat != null 
                            ? Duration.between(dbLastHeartbeat, now).toMillis() 
                            : -1;
                    long redisDiffMs = redisLastSeen != null 
                            ? Duration.between(redisLastSeen, now).toMillis() 
                            : -1;
                    
                    boolean dbTimeout = dbDiffMs >= timeoutMs;
                    boolean redisTimeout = redisDiffMs >= timeoutMs;
                    boolean isSynced = (dbLastHeartbeat == null && redisLastSeen == null) ||
                                      (dbLastHeartbeat != null && redisLastSeen != null && 
                                       Math.abs(Duration.between(dbLastHeartbeat, redisLastSeen).toMillis()) < 5000); // 5초 이내 차이는 동기화된 것으로 간주
                    
                    Map<String, Object> status = new HashMap<>();
                    status.put("userId", userId);
                    status.put("dbLastHeartbeatAt", dbLastHeartbeat != null ? dbLastHeartbeat.toString() : null);
                    status.put("redisLastSeenAt", redisLastSeen != null ? redisLastSeen.toString() : null);
                    status.put("now", now.toString());
                    status.put("dbDiffMs", dbDiffMs);
                    status.put("redisDiffMs", redisDiffMs);
                    status.put("timeoutMs", timeoutMs);
                    status.put("dbTimeout", dbTimeout);
                    status.put("redisTimeout", redisTimeout);
                    status.put("isSynced", isSynced);
                    
                    return status;
                })
                .collect(Collectors.toList());
        
        Map<String, Object> result = new HashMap<>();
        result.put("roomId", roomId);
        result.put("now", now.toString());
        result.put("timeoutSeconds", heartbeatTimeoutSeconds);
        result.put("timeoutMs", timeoutMs);
        result.put("participants", participantStatuses);
        
        return ResponseEntity.ok(result);
    }
}



