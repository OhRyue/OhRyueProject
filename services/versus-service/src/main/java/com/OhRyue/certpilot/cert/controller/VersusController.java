package com.OhRyue.certpilot.versus.controller;

import org.springframework.web.bind.annotation.*;
import java.util.Map;

@RestController @RequestMapping("/versus")
public class VersusController {
  @GetMapping("/ping")
  public Map<String,Object> ping(){
    return Map.of("service","versus","ok",true);
  }
}