package com.OhRyue.certpilot.community.dto;

import com.OhRyue.certpilot.community.domain.ReactionTargetType;
import jakarta.validation.constraints.NotNull;

public class ReactionDtos {

    public record ToggleRequest(
            @NotNull ReactionTargetType targetType,
            @NotNull Long targetId
    ) {}

    public record ToggleResponse(
            ReactionTargetType targetType,
            Long targetId,
            boolean liked,
            long likeCount
    ) {}
}
