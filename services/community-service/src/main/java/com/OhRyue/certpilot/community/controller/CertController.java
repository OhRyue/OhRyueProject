package com.OhRyue.certpilot.cert.controller;

import org.springframework.web.bind.annotation.*;
import java.util.Map;

@RestController @RequestMapping("/cert")
public class CertController {
  @GetMapping("/ping")
  public Map<String,Object> ping(){
    return Map.of("service","cert","ok",true);
  }
}