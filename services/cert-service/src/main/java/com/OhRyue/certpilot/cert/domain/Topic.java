package com.OhRyue.certpilot.cert.domain;

import com.OhRyue.certpilot.cert.domain.enums.ExamMode;
import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(
        name = "topic",
        indexes = {
                @Index(name = "ix_topic_parent", columnList = "parent_id"),
                @Index(name = "ix_topic_cert", columnList = "cert_id"),
                @Index(name = "ix_topic_mode", columnList = "exam_mode")
        }
)
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Topic {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "cert_id", nullable = false)
    private Long certId;

    @Column(name = "parent_id")
    private Long parentId;

    @Column(name = "code", length = 50, nullable = false)
    private String code;

    @Column(name = "title", length = 255, nullable = false)
    private String title;

    @Column(name = "emoji", length = 16)
    private String emoji;

    @Enumerated(EnumType.STRING)
    @Column(name = "exam_mode", length = 20, nullable = false)
    private ExamMode examMode;

    @Column(name = "order_no", nullable = false)
    private Integer orderNo;
}
