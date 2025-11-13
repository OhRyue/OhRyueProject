package com.OhRyue.certpilot.community.service;

import com.OhRyue.certpilot.community.domain.PostCategory;
import com.OhRyue.certpilot.community.exception.ResourceNotFoundException;
import com.OhRyue.certpilot.community.repository.PostCategoryRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.annotation.Cacheable;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class CategoryService {

  private final PostCategoryRepository categoryRepository;

  @Cacheable(cacheNames = "community:category:all")
  public List<PostCategory> findAll() {
    return categoryRepository.findAll();
  }

  public PostCategory getByCode(String code) {
    return categoryRepository.findByCode(code)
        .orElseThrow(() -> new ResourceNotFoundException("카테고리를 찾을 수 없습니다: " + code));
  }

  public PostCategory getById(Byte id) {
    return categoryRepository.findById(id)
        .orElseThrow(() -> new ResourceNotFoundException("카테고리를 찾을 수 없습니다: " + id));
  }
}


