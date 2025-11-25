package com.OhRyue.certpilot.cert.dto.external;

import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
@JsonIgnoreProperties(ignoreUnknown = true)
public class QnetApiResponse {

  @JsonProperty("response")
  private Response response;
  // 일부 data.go.kr API는 response 래퍼 없이 header/body 를 바로 반환한다.
  @JsonProperty("header")
  private Header rootHeader;
  @JsonProperty("body")
  private Body rootBody;

  @JsonIgnore
  public Response resolveResponse() {
    if (response != null) {
      return response;
    }
    if (rootHeader != null || rootBody != null) {
      Response fallback = new Response();
      fallback.setHeader(rootHeader);
      fallback.setBody(rootBody);
      return fallback;
    }
    return null;
  }

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

