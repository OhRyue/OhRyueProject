package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class CurriculumItem {

  @JsonProperty("atchFileNm")
  private String atchFileNm;

  @JsonProperty("grade")
  private String grade;

  @JsonProperty("gradeYy")
  private String gradeYy;

  @JsonProperty("univCd")
  private String univCd;

  @JsonProperty("univNm")
  private String univNm;
}


