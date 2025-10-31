package com.OhRyue.certpilot.study.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "concept")
@Getter @Setter
@NoArgsConstructor @AllArgsConstructor @Builder
public class Concept {

  @Id @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @Column(nullable = false)
  private Long topicId;                 // 가장 작은 단위 토픽(예: 1.1.1)의 ID

  @Lob
  @Column(columnDefinition = "TEXT")
  private String content;               // 레거시 단일 본문(유지)

  /**
   * 리치 컨텐츠(섹션/블록) JSON
   * 형식:
   * {
   *   "sections":[
   *     {
   *       "orderNo":1, "subCode":"1.1.1.1", "title":"플랫폼 기능 분석", "importance":2,
   *       "blocks":[
   *         {"type":"heading","text":"(1) 플랫폼의 개념"},
   *         {"type":"list","items":["플랫폼은 애플리케이션을 구동시키는데 필요한 소프트웨어 환경이다.","..."]},
   *         {"type":"heading","text":"(2) 플랫폼 성능 특성 분석"},
   *         {"type":"paragraph","text":"플랫폼 성능 분석을 통해 ..."},
   *         {"type":"table","caption":"플랫폼 성능 특성 측정 항목",
   *           "headers":["항목","설명"],
   *           "rows":[["경과시간","작업 시작~종료까지의 총 소요시간"],["사용률","자원 사용 비율"]]
   *         },
   *         {"type":"image","url":"https://.../img.png","alt":"설명","caption":"캡션"}
   *       ]
   *     },
   *     { "orderNo":2, "subCode":"1.1.1.2", "title":"운영체제 분석", "importance":1, "blocks":[ ... ] }
   *   ]
   * }
   */
  @Column(columnDefinition = "json")
  private String blocksJson;            // 섹션/블록 JSON (NULL 가능)
}
