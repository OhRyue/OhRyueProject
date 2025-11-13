package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.Post;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;

import java.time.Instant;
import java.util.List;
import java.util.Optional;

public interface PostRepository extends JpaRepository<Post, Long> {

  Optional<Post> findByIdAndDeletedAtIsNull(Long id);

  @Query("""
      select p from Post p
      where p.deletedAt is null
        and (:categoryId is null or p.categoryId = :categoryId)
        and (:authorId is null or p.authorId = :authorId)
        and (:anonymousOnly = false or p.anonymous = true)
        and (:since is null or p.createdAt >= :since)
        and (
              :keyword is null
              or lower(p.title) like lower(concat('%', :keyword, '%'))
              or lower(p.content) like lower(concat('%', :keyword, '%'))
        )
      """)
  Page<Post> search(@Param("categoryId") Byte categoryId,
                    @Param("keyword") String keyword,
                    @Param("authorId") String authorId,
                    @Param("anonymousOnly") boolean anonymousOnly,
                    @Param("since") Instant since,
                    Pageable pageable);

  @Query("""
      select p from Post p
      where p.deletedAt is null
        and (:categoryId is null or p.categoryId = :categoryId)
        and (:authorId is null or p.authorId = :authorId)
        and (:anonymousOnly = false or p.anonymous = true)
        and (:since is null or p.createdAt >= :since)
        and p.authorId not in :blockedAuthors
        and (
              :keyword is null
              or lower(p.title) like lower(concat('%', :keyword, '%'))
              or lower(p.content) like lower(concat('%', :keyword, '%'))
        )
      """)
  Page<Post> searchExcluding(@Param("categoryId") Byte categoryId,
                             @Param("keyword") String keyword,
                             @Param("authorId") String authorId,
                             @Param("anonymousOnly") boolean anonymousOnly,
                             @Param("since") Instant since,
                             @Param("blockedAuthors") List<String> blockedAuthors,
                             Pageable pageable);

  List<Post> findByAuthorIdAndDeletedAtIsNull(String authorId, Pageable pageable);

  @Query("""
      select p from Post p
      where p.deletedAt is null
        and p.createdAt >= :since
      order by (p.likeCount * 2 + p.commentCount * 3 + p.viewCount) desc, p.createdAt desc
      """)
  List<Post> findTopHotSince(@Param("since") Instant since, Pageable pageable);

  @Query("""
      select p from Post p
      where p.deletedAt is null
        and p.createdAt >= :since
        and p.authorId not in :blockedAuthors
      order by (p.likeCount * 2 + p.commentCount * 3 + p.viewCount) desc, p.createdAt desc
      """)
  List<Post> findTopHotSinceExcluding(@Param("since") Instant since,
                                      @Param("blockedAuthors") List<String> blockedAuthors,
                                      Pageable pageable);
}


