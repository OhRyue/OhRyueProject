package com.OhRyue.certpilot.study.repository;

import com.OhRyue.certpilot.study.domain.Topic;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TopicRepository extends JpaRepository<Topic, Long> {
}
