package com.OhRyue.certpilot.report.controller;

import org.springframework.web.bind.annotation.*;import java.util.Map;

@RestController @RequestMapping("/report")
public class ReportController {
  @GetMapping("/ping")
  public Map<String,Object> ping(){
    return Map.of("service","report","ok",true);
  }
}