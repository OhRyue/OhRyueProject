package com.OhRyue.certpilot.community.client;

import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;

import java.util.Map;

@FeignClient(
    name = "progress-service",
    contextId = "notificationClient",
    url = "${PROGRESS_SERVICE_URL:http://progress-service:8083}/api/progress/notifications"
)
public interface NotificationClient {

    @PostMapping("/create")
    void create(@RequestBody NotificationCreateRequest request);

    record NotificationCreateRequest(
        String userId,
        String type,
        String title,
        String message,
        Map<String, Object> payload
    ) {}
}












