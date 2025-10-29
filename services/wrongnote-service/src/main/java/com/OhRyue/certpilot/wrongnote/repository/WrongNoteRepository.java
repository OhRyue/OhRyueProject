package com.OhRyue.certpilot.wrongnote.repository;

import com.OhRyue.certpilot.wrongnote.entity.WrongNote;
import com.OhRyue.certpilot.wrongnote.entity.WrongNoteId;
import org.springframework.data.jpa.repository.JpaRepository;

public interface WrongNoteRepository extends JpaRepository<WrongNote, WrongNoteId> {
}

