package com.OhRyue.certpilot.study.dto;

import io.swagger.v3.oas.annotations.media.Schema;

/**
 * 태그 관련 공통 DTO
 * 
 * <p>태그 관련 공통 규칙:</p>
 * <ul>
 *   <li>항상 code 기반으로 로직/검색/필터를 처리한다.</li>
 *   <li>UI에는 labelKo를 기본으로 사용하고, 필요 시 description을 툴팁/설명에 쓴다.</li>
 *   <li>정렬이 필요한 태그 목록은 orderNo를 기준으로 정렬한다.</li>
 *   <li>domain은 필기/실기를 구분하거나, 필터(WRITTEN/PRACTICAL)용으로 사용할 수 있다.</li>
 * </ul>
 */
public class TagDtos {

  @Schema(description = "태그 정보 DTO (프론트엔드 표시용)")
  public record TagViewDto(
      @Schema(description = "태그 코드 (예: DB_SQL_TX)", example = "DB_SQL_TX")
      String code,
      
      @Schema(description = "한글 라벨 (화면 표시용)", example = "SQL·트랜잭션·관계대수")
      String labelKo,
      
      @Schema(description = "영문 라벨", example = "SQL / Transaction / Relational Algebra")
      String labelEn,
      
      @Schema(description = "태그 설명 (툴팁/세부 설명용)", example = "DDL/DML/DCL, VIEW, ...")
      String description,
      
      @Schema(description = "도메인 (WRITTEN | PRACTICAL)", example = "WRITTEN")
      String domain,
      
      @Schema(description = "정렬 순서", example = "9")
      Integer orderNo
  ) {}
}





