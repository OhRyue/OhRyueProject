package com.OhRyue.certpilot.auxiliary.web.dto;

import java.util.List;

public record AuxSoloStartDto(
        String sessionId,
        List<Item> items
) {
    public record Item(
            Long id,
            String stem,
            List<String> choices
    ) {}
}
