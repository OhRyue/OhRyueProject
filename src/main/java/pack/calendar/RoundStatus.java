package pack.calendar;

public enum RoundStatus {
  BEFORE_REG,      // 접수 시작 전
  REGISTER_OPEN,   // 접수 중
  BEFORE_EXAM,     // 접수 마감~시험 전
  EXAM_TODAY,      // 시험 당일
  DONE             // 시험 종료
}
