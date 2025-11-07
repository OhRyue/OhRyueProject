package com.OhRyue.certpilot.account.domain;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "user_settings")
@Getter @Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class UserSettings {

  @Id
  @Column(name = "user_id", length = 100)
  private String userId;

  // JSON 컬럼은 RDB에는 TEXT로 매핑 가능, JPA에선 String으로 들고 다니다가 서비스에서 파싱
  @Column(name = "ui_prefs_json", columnDefinition = "JSON")
  private String uiPrefsJson;

  @Column(name = "notif_prefs_json", columnDefinition = "JSON")
  private String notifPrefsJson;
}
