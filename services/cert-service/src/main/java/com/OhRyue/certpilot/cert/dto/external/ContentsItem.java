package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class ContentsItem {

  @JsonProperty("ctsId")
  private String ctsId;

  @JsonProperty("ctsNm")
  private String ctsNm;

  @JsonProperty("deptNm")
  private String deptNm;

  @JsonProperty("pubYnCcd")
  private String pubYnCcd;

  @JsonProperty("title")
  private String title;
}


