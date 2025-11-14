package com.OhRyue.certpilot.cert.service;

import com.OhRyue.certpilot.cert.domain.Concept;
import com.OhRyue.certpilot.cert.repository.ConceptRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.NoSuchElementException;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class ConceptQueryService {

    private final ConceptRepository conceptRepository;

    public Concept getByTopicId(Long topicId) {
        return conceptRepository.findByTopicId(topicId)
                .orElseThrow(() -> new NoSuchElementException("Concept not found for topicId: " + topicId));
    }
}
