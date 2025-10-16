package com.OhRyue.certpilot.curriculum.domain.repo;

import com.OhRyue.certpilot.curriculum.domain.Topic;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;

public interface TopicRepository extends JpaRepository<Topic, Long> {
  List<Topic> findByParentIdOrderByOrdAscIdAsc(Long parentId);
  List<Topic> findByCertIdAndLevelOrderByOrdAsc(Long certId, Integer level);
}
