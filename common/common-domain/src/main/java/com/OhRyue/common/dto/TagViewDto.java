package com.OhRyue.common.dto;

/**
 * 태그 정보 DTO (공통)
 * 
 * <p>태그 관련 공통 규칙:</p>
 * <ul>
 *   <li>항상 code 기반으로 로직/검색/필터를 처리한다.</li>
 *   <li>UI에는 labelKo를 기본으로 사용하고, 필요 시 description을 툴팁/설명에 쓴다.</li>
 *   <li>정렬이 필요한 태그 목록은 orderNo를 기준으로 정렬한다.</li>
 *   <li>domain은 필기/실기를 구분하거나, 필터(WRITTEN/PRACTICAL)용으로 사용할 수 있다.</li>
 * </ul>
 * 
 * @param code 태그 코드 (예: "DB_SQL_TX")
 * @param labelKo 한글 라벨 (화면 표시용, 예: "SQL·트랜잭션·관계대수")
 * @param labelEn 영문 라벨 (예: "SQL / Transaction / Relational Algebra")
 * @param description 태그 설명 (툴팁/세부 설명용, 예: "DDL/DML/DCL, VIEW, ...")
 * @param domain 도메인 ("WRITTEN" | "PRACTICAL")
 * @param orderNo 정렬 순서 (null 가능)
 */
public record TagViewDto(
    String code,
    String labelKo,
    String labelEn,
    String description,
    String domain,
    Integer orderNo
) {}

