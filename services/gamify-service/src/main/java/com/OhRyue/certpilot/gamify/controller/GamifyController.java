package com.OhRyue.certpilot.gamify.controller;

import org.springframework.web.bind.annotation.*;
import java.util.Map;

@RestController @RequestMapping("/gamify")
public class GamifyController {
  @GetMapping("/ping") public Map<String,Object> ping(){
    return Map.of("service","gamify","ok",true);
  }
}