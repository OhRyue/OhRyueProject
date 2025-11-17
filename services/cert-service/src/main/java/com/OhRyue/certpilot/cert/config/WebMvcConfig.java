package com.OhRyue.certpilot.cert.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.boot.autoconfigure.http.HttpMessageConverters;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

import java.util.List;

@Configuration
public class WebMvcConfig {

    /**
     * JSON 전용 MessageConverter 등록
     * - XML 컨버터는 아예 등록하지 않습니다.
     */
    @Bean
    public MappingJackson2HttpMessageConverter jsonMessageConverter(ObjectMapper objectMapper) {
        MappingJackson2HttpMessageConverter converter =
                new MappingJackson2HttpMessageConverter(objectMapper);

        converter.setSupportedMediaTypes(List.of(
                MediaType.APPLICATION_JSON,
                new MediaType("application", "*+json")
        ));

        return converter;
    }

    /**
     * Spring Boot 기본 HttpMessageConverters를 덮어쓰고
     * 우리가 만든 JSON 컨버터만 사용하도록 설정합니다.
     */
    @Bean
    public HttpMessageConverters customConverters(MappingJackson2HttpMessageConverter jsonMessageConverter) {
        // 이 Bean을 정의하면 Boot가 기본으로 넣어주던 XML 컨버터들도 포함한
        // 전체 목록 대신, 여기서 넘긴 컨버터들만 사용합니다.
        return new HttpMessageConverters(jsonMessageConverter);
    }
}
