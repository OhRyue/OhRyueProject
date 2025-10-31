package com.OhRyue.certpilot.account.controller;


import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;


import java.util.Map;


@RestController
@RequestMapping("/account")
public class AccountController {
  @GetMapping("/ping")
  public Map<String,Object> ping(){ return Map.of("service","account","ok",true); }
}