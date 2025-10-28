INSERT INTO topic (name, level, parent_id, ord) VALUES
('소프트웨어설계', 1, NULL, 1),
('요구사항확인', 2, 1, 1);

INSERT INTO question (stem, choices_json, answer_idx, difficulty)
VALUES
('요구사항 분석의 주 산출물은 무엇인가?', '["요구사항정의서","테스트케이스","소스코드","DB스키마"]', 0, 2),
('UML에서 유스케이스를 표현하는 다이어그램은?', '["클래스","시퀀스","유스케이스","패키지"]', 2, 1);

INSERT INTO question_topic (question_id, topic_id) VALUES (1,2),(2,2);

INSERT INTO question_tag (question_id, tag) VALUES (1,'req'),(2,'uml');
