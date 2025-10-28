package com.OhRyue.certpilot.question.repo;

import com.OhRyue.certpilot.question.domain.topic.Topic;
import org.springframework.data.jpa.repository.JpaRepository;

public interface TopicRepository extends JpaRepository<Topic, Long> {}
