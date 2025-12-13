package com.OhRyue.certpilot.progress.repository;

import com.OhRyue.certpilot.progress.domain.UserNotification;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;

public interface UserNotificationRepository extends JpaRepository<UserNotification, Long> {

    Page<UserNotification> findByUserIdOrderByCreatedAtDesc(String userId, Pageable pageable);

    Page<UserNotification> findByUserIdAndIsReadFalseOrderByCreatedAtDesc(String userId, Pageable pageable);

    @Modifying
    @Query("UPDATE UserNotification n SET n.isRead = true, n.readAt = :readAt WHERE n.userId = :userId AND n.isRead = false")
    int markAllAsReadByUserId(@Param("userId") String userId, @Param("readAt") Instant readAt);

    List<UserNotification> findByUserIdAndIsReadFalse(String userId);
}












