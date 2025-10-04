package pack.calendar.dto;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class FullCalendarEventDto {
  private String id;     // 고유 ID
  private String title;  // 캘린더에 표시될 제목
  private String start;  // ISO-8601 날짜("YYYY-MM-DD") 또는 datetime
  // 필요 시 end, allDay, url 등 추가 가능
}
