package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class OpenQuestionItem {

  @JsonProperty("artlSeq")
  private Long artlSeq;

  @JsonProperty("title")
  private String title;

  @JsonProperty("seriesCd")
  private String seriesCd;

  @JsonProperty("seriesNm")
  private String seriesNm;

  @JsonProperty("qualgbCd")
  private String qualgbCd;

  @JsonProperty("qualgbNm")
  private String qualgbNm;

  @JsonProperty("jmCd")
  private String jmCd;

  @JsonProperty("jmNm")
  private String jmNm;

  @JsonProperty("regDttm")
  private String regDttm;

  @JsonProperty("modDttm")
  private String modDttm;

  @JsonProperty("atchFileNm")
  private String attachmentNames;

  @JsonProperty("atchFileUrl")
  private String attachmentUrls;
}

