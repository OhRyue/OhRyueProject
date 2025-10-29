package com.OhRyue.certpilot.calendar.dto;

public class CertNameDto {
  private Long id;
  private String name;
  private String field;

  public CertNameDto() {}

  public CertNameDto(Long id, String name, String field) {
    this.id = id;
    this.name = name;
    this.field = field;
  }

  public Long getId() { return id; }
  public void setId(Long id) { this.id = id; }
  
  public String getName() { return name; }
  public void setName(String name) { this.name = name; }
  
  public String getField() { return field; }
  public void setField(String field) { this.field = field; }
}

