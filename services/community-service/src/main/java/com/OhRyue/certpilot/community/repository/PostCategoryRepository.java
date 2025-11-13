package com.OhRyue.certpilot.community.repository;

import com.OhRyue.certpilot.community.domain.PostCategory;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface PostCategoryRepository extends JpaRepository<PostCategory, Byte> {

  Optional<PostCategory> findByCode(String code);
}


