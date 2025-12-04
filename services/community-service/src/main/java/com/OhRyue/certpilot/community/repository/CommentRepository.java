package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.Comment;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.util.List;
import java.util.Optional;

public interface CommentRepository extends JpaRepository<Comment, Long> {

  Page<Comment> findByPostIdAndDeletedAtIsNullOrderByCreatedAtAsc(Long postId, Pageable pageable);

  Optional<Comment> findByIdAndDeletedAtIsNull(Long id);

  long countByPostIdAndDeletedAtIsNull(Long postId);

  List<Comment> findByAuthorIdAndDeletedAtIsNull(String authorId, Pageable pageable);

  @Query("SELECT DISTINCT c.authorId FROM Comment c WHERE c.postId = :postId AND c.deletedAt IS NULL")
  List<String> findDistinctUserIdsByPostId(@Param("postId") Long postId);
}


