package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.Topic;
import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import com.OhRyue.certpilot.cert.repository.TopicRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;
import java.util.NoSuchElementException;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class TopicQueryService {

    private final TopicRepository topicRepository;

    public Topic getById(Long id) {
        return topicRepository.findById(id)
                .orElseThrow(() -> new NoSuchElementException("Topic not found: " + id));
    }

    public List<Topic> list(Long certId, ExamMode mode, Long parentId) {
        // certId + mode + parentId 조합 필터링
        List<Topic> base;

        if (certId != null && mode != null) {
            base = topicRepository.findByCertIdAndExamMode(certId, mode);
        } else if (certId != null) {
            base = topicRepository.findByCertId(certId);
        } else if (mode != null) {
            base = topicRepository.findByExamMode(mode);
        } else {
            base = topicRepository.findAll();
        }

        if (parentId != null) {
            return base.stream()
                    .filter(t -> parentId.equals(t.getParentId()))
                    .toList();
        }

        return base;
    }

    public List<Topic> search(String code, String title) {
        return topicRepository.findAll().stream()
                .filter(t -> code == null || (t.getCode() != null && t.getCode().contains(code)))
                .filter(t -> title == null || (t.getTitle() != null && t.getTitle().contains(title)))
                .toList();
    }
}
