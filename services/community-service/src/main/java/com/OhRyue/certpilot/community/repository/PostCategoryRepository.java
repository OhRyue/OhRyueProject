package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.PostCategory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.List;
import java.util.Optional;

public interface PostCategoryRepository extends JpaRepository<PostCategory, Byte> {

  Optional<PostCategory> findByCode(String code);

  /**
   * 전체(ALL) 카테고리를 제외하고, sort_order 기준으로 정렬해서 조회
   */
  List<PostCategory> findByCodeNotOrderBySortOrderAsc(String code);
}
