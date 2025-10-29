package com.OhRyue.certpilot.calendar.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class FullCalendarEventDto {
  private String id;
  private String title;
  private String start;
}

