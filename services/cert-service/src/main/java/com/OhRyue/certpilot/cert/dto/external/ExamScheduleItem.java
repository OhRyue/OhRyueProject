package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class ExamScheduleItem {

  @JsonProperty("implYy")
  private String implYy;

  @JsonProperty("implSeq")
  private String implSeq;

  @JsonProperty("qualgbCd")
  private String qualgbCd;

  @JsonProperty("qualgbNm")
  private String qualgbNm;

  @JsonProperty("jmCd")
  private String jmCd;

  @JsonProperty("jmNm")
  private String jmNm;

  @JsonProperty("description")
  private String description;

  @JsonProperty("docRegStartDt")
  private String docRegStartDt;

  @JsonProperty("docRegEndDt")
  private String docRegEndDt;

  @JsonProperty("docExamStartDt")
  private String docExamStartDt;

  @JsonProperty("docExamEndDt")
  private String docExamEndDt;

  @JsonProperty("docPassDt")
  private String docPassDt;

  @JsonProperty("pracRegStartDt")
  private String pracRegStartDt;

  @JsonProperty("pracRegEndDt")
  private String pracRegEndDt;

  @JsonProperty("pracExamStartDt")
  private String pracExamStartDt;

  @JsonProperty("pracExamEndDt")
  private String pracExamEndDt;

  @JsonProperty("pracPassDt")
  private String pracPassDt;
}

