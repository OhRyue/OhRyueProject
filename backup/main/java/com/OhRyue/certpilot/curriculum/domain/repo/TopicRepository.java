package com.OhRyue.certpilot.curriculum.domain.repo;

import com.OhRyue.certpilot.curriculum.domain.Topic;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface TopicRepository extends JpaRepository<Topic, Long> {
  Optional<Topic> findByIdAndCertId(Long id, Long certId);
  List<Topic> findByParentId(Long parentId);
  List<Topic> findByCertIdAndLevel(Long certId, Integer level);
}
