package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class QualificationItem {

  @JsonProperty("jmCd")
  private String jmCd;

  @JsonProperty("seriesCd")
  private String seriesCd;

  @JsonProperty("jmNm")
  private String jmNm;

  @JsonProperty("engJmNm")
  private String engJmNm;

  @JsonProperty("seriesNm")
  private String seriesNm;

  @JsonProperty("implNm")
  private String implNm;

  @JsonProperty("instiNm")
  private String instiNm;

  @JsonProperty("summary")
  private String summary;

  @JsonProperty("job")
  private String job;

  @JsonProperty("trend")
  private String trend;

  @JsonProperty("career")
  private String career;

  @JsonProperty("hist")
  private String hist;
}

