package com.OhRyue.certpilot.account.repo;

import com.OhRyue.certpilot.account.domain.NotificationJob;
import com.OhRyue.certpilot.account.domain.enums.NotificationChannel;
import com.OhRyue.certpilot.account.domain.enums.NotificationStatus;
import com.OhRyue.certpilot.account.domain.enums.NotificationType;
import org.springframework.data.jpa.repository.JpaRepository;

import java.time.Instant;
import java.util.List;

public interface NotificationJobRepository extends JpaRepository<NotificationJob, Long> {

  boolean existsByUserIdAndChannelAndTypeAndScheduledAt(String userId,
                                                        NotificationChannel channel,
                                                        NotificationType type,
                                                        Instant scheduledAt);

  List<NotificationJob> findTop100ByStatusOrderByScheduledAtAsc(NotificationStatus status);
}

