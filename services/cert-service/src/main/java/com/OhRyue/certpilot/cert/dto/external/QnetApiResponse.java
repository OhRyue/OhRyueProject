package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class QnetApiResponse {

  @JsonProperty("response")
  private Response response;

  @Getter
  @Setter
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Response {
    private Header header;
    private Body body;
  }

  @Getter
  @Setter
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Header {
    private String resultCode;
    private String resultMsg;
  }

  @Getter
  @Setter
  @JsonIgnoreProperties(ignoreUnknown = true)
  public static class Body {
    private JsonNode items;
    private Integer numOfRows;
    private Integer pageNo;
    private Integer totalCount;
  }
}

