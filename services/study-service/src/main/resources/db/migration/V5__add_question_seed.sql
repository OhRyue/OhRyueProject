INSERT INTO question (
    cert_id, topic_id, mode, type, difficulty,
    stem, payload_json, answer_key, solution_text, source, image_url
)
VALUES
-- 10번 토픽: SOLID 원칙 / 객체지향 설계 원칙 OX 추가

(1, 10, 'WRITTEN', 'OX', 'EASY',
 '인터페이스 분리 원칙(ISP)은 하나의 큰 인터페이스를 여러 개의 작은 인터페이스로 나누어, 클라이언트가 자신이 사용하지 않는 메서드에 의존하지 않도록 하는 원칙이다. (O/X)',
 NULL,
 'O',
 'ISP는 클라이언트별로 필요한 메서드만 가진 작은 인터페이스를 설계함으로써 불필요한 의존을 줄이는 원칙입니다.',
 NULL, NULL),

(1, 10, 'WRITTEN', 'OX', 'NORMAL',
 '의존성 역전 원칙(DIP)은 고수준 모듈이 저수준 모듈의 구체 구현에 직접 의존하도록 설계하는 것을 의미한다. (O/X)',
 NULL,
 'X',
 'DIP는 고수준 모듈과 저수준 모듈 모두 추상화에 의존하도록 하여, 구체 구현 변경에 덜 민감하게 만드는 원칙입니다.',
 NULL, NULL),

(1, 10, 'WRITTEN', 'OX', 'NORMAL',
 '리스코프 치환 원칙(LSP)은 자식 클래스가 부모 클래스의 계약을 깨뜨리지 않고 완전히 대체 가능해야 한다는 원칙이다. (O/X)',
 NULL,
 'O',
 'LSP는 상속 구조에서 자식 타입을 부모 타입 대신 사용해도 기존 동작과 계약이 유지되어야 한다는 원칙입니다.',
 NULL, NULL);

INSERT INTO question (
    cert_id, topic_id, mode, type, difficulty,
    stem, payload_json, answer_key, solution_text, source, image_url
)
VALUES
(1, 9, 'WRITTEN', 'MCQ', 'EASY',
 '현행 시스템 분석 단계에서 수집해야 할 정보로 가장 거리가 먼 것은 무엇인가?',
 NULL,
 'D',
 '현행 분석에서는 기능, 인터페이스, 성능, 보안 등 시스템 특성을 수집하며, 조직 내 인사 평가 기준은 직접적인 분석 대상이 아닙니다.',
 NULL, NULL),

(1, 9, 'WRITTEN', 'MCQ', 'NORMAL',
 '외부 시스템 인터페이스 분석 시 가장 먼저 정리해야 할 항목으로 적절한 것은 무엇인가?',
 NULL,
 'B',
 '연계 대상 시스템, 인터페이스 데이터 구조, 전송 주기와 프로토콜 등을 우선 정리해야 이후 상세 설계가 수월해집니다.',
 NULL, NULL);


INSERT INTO question (
    cert_id, topic_id, mode, type, difficulty,
    stem, payload_json, answer_key, solution_text, source, image_url
)
VALUES
(1, 11, 'WRITTEN', 'MCQ', 'EASY',
 'RESTful API 설계 시 HTTP 메서드와 가장 올바르게 매핑된 것은 무엇인가?',
 NULL,
 'A',
 '조회는 GET, 생성은 POST, 전체 수정은 PUT, 부분 수정은 PATCH, 삭제는 DELETE에 매핑하는 것이 일반적인 REST 관례입니다.',
 NULL, NULL),

(1, 11, 'WRITTEN', 'MCQ', 'NORMAL',
 '대량 트래픽 환경에서 외부 API를 연동할 때, 장애 확산을 막기 위한 대표적인 패턴 조합으로 가장 적절한 것은?',
 NULL,
 'C',
 '타임아웃, 재시도(백오프), 서킷브레이커를 함께 설계하면 지연과 실패를 제어하며 장애 확산을 효과적으로 막을 수 있습니다.',
 NULL, NULL);
