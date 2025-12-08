-- =========================================
-- Topic ID 상수 (4과목 프로그래밍 언어 활용 – 4.x.x)
-- =========================================
SET @tp_41101 := 14101; -- 4.1.1 프로그래밍 언어 기본 개념/변수
SET @tp_41102 := 14102; -- 4.1.2 연산자와 형 변환
SET @tp_41103 := 14103; -- 4.1.3 표현식·문장·블록 구조
SET @tp_42101 := 14201; -- 4.2.1 조건문/분기
SET @tp_42102 := 14202; -- 4.2.2 반복문
SET @tp_42103 := 14203; -- 4.2.3 문자열 처리(C 중심)
SET @tp_43101 := 14301; -- 4.3.1 함수/프로시저와 매개변수
SET @tp_43102 := 14302; -- 4.3.2 예외 처리·표준 라이브러리

/* ========================================================
 * 4.1.1 프로그래밍 언어 기본 개념/변수  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'EASY',
       '변수는 프로그램 실행 중에 값을 저장하기 위한 메모리 공간을 의미하며, 선언 시 타입과 이름을 함께 지정하는 것이 일반적이다.',
       'O',
       '변수 선언 시 데이터 타입과 식별자(이름)를 함께 지정해, 어떤 종류의 값을 저장할지 컴파일러에 알려 줍니다.',
       'seed:4.1.1:var-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '변수는 프로그램 실행 중에 값을 저장하기 위한 메모리 공간을 의미하며, 선언 시 타입과 이름을 함께 지정하는 것이 일반적이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'EASY',
       '대부분의 프로그래밍 언어에서 식별자(변수 이름)는 숫자로 시작할 수 없고, 공백 문자를 포함할 수 없다.',
       'O',
       'C, Java 등 대부분의 언어에서 식별자는 문자 또는 밑줄로 시작해야 하며, 공백을 포함할 수 없습니다.',
       'seed:4.1.1:identifier-rule:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '대부분의 프로그래밍 언어에서 식별자(변수 이름)는 숫자로 시작할 수 없고, 공백 문자를 포함할 수 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'NORMAL',
       '정적 타이핑 언어에서는 변수에 최초로 대입되는 값의 타입에 의해 자동으로 타입이 결정되고, 이후에 다른 타입의 값을 자유롭게 대입할 수 있다.',
       'X',
       '정적 타이핑 언어(C/Java 등)는 컴파일 시점에 타입이 고정되며, 선언된 타입과 다른 값을 대입하면 오류가 발생합니다.',
       'seed:4.1.1:static-typing:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정적 타이핑 언어에서는 변수에 최초로 대입되는 값의 타입에 의해 자동으로 타입이 결정되고, 이후에 다른 타입의 값을 자유롭게 대입할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'NORMAL',
       '전역 변수는 프로그램 어디에서나 접근 가능하므로, 항상 지역 변수보다 우선적으로 사용하는 것이 바람직하다.',
       'X',
       '전역 변수는 편리하지만 결합도가 높아져 유지보수가 어려워질 수 있어, 필요한 범위 내에서만 지역 변수를 사용하는 것이 권장됩니다.',
       'seed:4.1.1:global-var:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '전역 변수는 프로그램 어디에서나 접근 가능하므로, 항상 지역 변수보다 우선적으로 사용하는 것이 바람직하다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'NORMAL',
       '상수(constant)는 프로그램 실행 중에 값을 변경할 수 없는 데이터로, 매직 넘버를 대체하여 코드 가독성을 높이는 데 활용할 수 있다.',
       'O',
       '의미 있는 이름을 가진 상수를 사용하면 값의 의미를 명확히 표현하고, 변경 시에도 한 곳만 수정하면 되어 유지보수성이 향상됩니다.',
       'seed:4.1.1:constant:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '상수(constant)는 프로그램 실행 중에 값을 변경할 수 없는 데이터로, 매직 넘버를 대체하여 코드 가독성을 높이는 데 활용할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41101, 'WRITTEN', 'OX', 'EASY',
       '같은 블록 내에서 동일한 이름의 변수를 두 번 이상 선언하면 대부분의 정적 타이핑 언어에서는 컴파일 오류가 발생한다.',
       'O',
       '동일 스코프에서 같은 이름의 변수를 재선언하는 것은 이름 충돌로 간주되어 컴파일 오류가 발생합니다.',
       'seed:4.1.1:redeclaration:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '같은 블록 내에서 동일한 이름의 변수를 두 번 이상 선언하면 대부분의 정적 타이핑 언어에서는 컴파일 오류가 발생한다.%'
);

/* ========================================================
 * 4.1.2 연산자와 형 변환  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'EASY',
       '대부분의 언어에서 대입 연산자(=)는 오른쪽 피연산자의 값을 왼쪽 피연산자(변수)에 저장한다.',
       'O',
       'a = b; 는 b의 값을 a에 저장한다는 의미이며, ==는 동등 비교 연산자로 쓰입니다.',
       'seed:4.1.2:assign-vs-eq:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '대부분의 언어에서 대입 연산자(=)는 오른쪽 피연산자의 값을 왼쪽 피연산자(변수)에 저장한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'NORMAL',
       '정수 나눗셈 연산에서 피연산자가 모두 정수형이면, 결과는 소수점 이하까지 정확한 실수 값으로 반환된다.',
       'X',
       'C/Java 등에서는 두 피연산자가 정수형이면 소수점 이하가 버려진 정수 결과가 나옵니다.',
       'seed:4.1.2:int-division:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '정수 나눗셈 연산에서 피연산자가 모두 정수형이면, 결과는 소수점 이하까지 정확한 실수 값으로 반환된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'NORMAL',
       '논리 연산자 AND, OR는 단축 평가(short-circuit)를 지원하는 경우, 왼쪽 피연산자의 결과만으로 전체 결과가 결정되면 오른쪽 피연산자를 평가하지 않을 수 있다.',
       'O',
       '예를 들어, 조건식에서 앞부분이 거짓인 AND 연산은 뒷부분을 평가하지 않고 바로 거짓으로 결정할 수 있습니다.',
       'seed:4.1.2:short-circuit:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '논리 연산자 AND, OR는 단축 평가(short-circuit)를 지원하는 경우, 왼쪽 피연산자의 결과만으로 전체 결과가 결정되면 오른쪽 피연산자를 평가하지 않을 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'NORMAL',
       '암시적 형 변환(자동 형 변환)은 항상 정보 손실 없이 안전하게 수행되므로, 개발자가 별도로 신경 쓸 필요가 없다.',
       'X',
       '정수 → 실수 승격은 비교적 안전하지만, 큰 실수를 작은 정수형으로 변환하는 경우처럼 암시적 변환 중에도 정보 손실이 발생할 수 있습니다.',
       'seed:4.1.2:implicit-cast:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '암시적 형 변환(자동 형 변환)은 항상 정보 손실 없이 안전하게 수행되므로, 개발자가 별도로 신경 쓸 필요가 없다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'NORMAL',
       '명시적 형 변환(cast)을 사용하면, 타입만 강제로 바꿀 뿐 값의 범위나 오버플로 같은 문제는 고려하지 않아도 된다.',
       'X',
       '캐스팅은 단순히 비트 해석 방식만 바꾸기도 하므로, 범위를 벗어나면 의도치 않은 값이 될 수 있습니다.',
       'seed:4.1.2:explicit-cast:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '명시적 형 변환(cast)을 사용하면, 타입만 강제로 바꿀 뿐 값의 범위나 오버플로 같은 문제는 고려하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41102, 'WRITTEN', 'OX', 'EASY',
       '관계 연산자(<, >, == 등)의 결과는 보통 논리값(true/false)으로 표현되며, 조건문에서 분기 판단에 사용된다.',
       'O',
       '비교 결과는 조건식으로 사용되어 if, while 등의 분기·반복 구조를 제어합니다.',
       'seed:4.1.2:relational:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '관계 연산자(<, >, == 등)의 결과는 보통 논리값(true/false)으로 표현되며, 조건문에서 분기 판단에 사용된다.%'
);

/* ========================================================
 * 4.1.3 표현식·문장·블록 구조  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'EASY',
       '표현식(Expression)은 값으로 평가될 수 있는 코드 조각을 의미하며, 하나의 문장(statement) 안에 여러 표현식이 포함될 수 있다.',
       'O',
       'a + b, func(x) 와 같은 것이 표현식이며, 세미콜론으로 끝나는 한 줄이 문장인 경우가 많습니다.',
       'seed:4.1.3:expression:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '표현식(Expression)은 값으로 평가될 수 있는 코드 조각을 의미하며, 하나의 문장(statement) 안에 여러 표현식이 포함될 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'EASY',
       'C/Java 계열 언어에서 중괄호({})로 둘러싸인 영역은 하나의 블록으로 간주되며, 독립적인 지역 변수 범위를 제공한다.',
       'O',
       '블록마다 별도의 스코프가 형성되어, 블록 내부에서 선언된 변수는 블록 밖에서 보이지 않습니다.',
       'seed:4.1.3:block-scope:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'C/Java 계열 언어에서 중괄호({})로 둘러싸인 영역은 하나의 블록으로 간주되며, 독립적인 지역 변수 범위를 제공한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'NORMAL',
       '세미콜론(;)은 대부분의 언어에서 문장을 구분하는 역할을 하므로, if, for와 같은 제어문 뒤에는 항상 세미콜론을 붙여야 한다.',
       'X',
       'if (조건) 문장; 에서 if 블록 자체 뒤에는 세미콜론을 붙이지 않으며, 잘못 붙이면 빈 문장이 되어 논리 오류가 발생할 수 있습니다.',
       'seed:4.1.3:semicolon:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '세미콜론(;)은 대부분의 언어에서 문장을 구분하는 역할을 하므로, if, for와 같은 제어문 뒤에는 항상 세미콜론을 붙여야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'NORMAL',
       '한 줄에 여러 문장을 연속해서 쓸 수 있지만, 가독성과 유지보수성을 위해 일반적으로 문장당 한 줄 사용하는 것이 권장된다.',
       'O',
       '여러 문장을 한 줄에 모으면 오류를 찾기 어렵고 코드 가독성이 떨어집니다.',
       'seed:4.1.3:one-stmt-per-line:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '한 줄에 여러 문장을 연속해서 쓸 수 있지만, 가독성과 유지보수성을 위해 일반적으로 문장당 한 줄 사용하는 것이 권장된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'NORMAL',
       '주석(Comment)은 컴파일러가 실행 코드로 변환하지 않는 설명용 텍스트로, 프로그램의 동작 결과에는 영향을 주지 않는다.',
       'O',
       '주석은 코드 이해를 돕기 위한 설명이며, 컴파일 과정에서 제거됩니다.',
       'seed:4.1.3:comment:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '주석(Comment)은 컴파일러가 실행 코드로 변환하지 않는 설명용 텍스트로, 프로그램의 동작 결과에는 영향을 주지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_41103, 'WRITTEN', 'OX', 'EASY',
       '읽기 쉬운 코드 스타일을 위해 일관된 들여쓰기와 블록 정렬을 유지하는 것은 협업 개발에서 매우 중요하다.',
       'O',
       '들여쓰기 규칙이 통일되어 있으면, 제어 구조와 블록 범위를 빠르게 파악할 수 있습니다.',
       'seed:4.1.3:indent-style:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_41103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '읽기 쉬운 코드 스타일을 위해 일관된 들여쓰기와 블록 정렬을 유지하는 것은 협업 개발에서 매우 중요하다.%'
);

/* ========================================================
 * 4.2.1 조건문/분기  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'EASY',
       'if 문은 조건식이 참(true)인 경우에만 특정 문장 블록을 실행하기 위한 분기 구조이다.',
       'O',
       'if (조건) { … } 형태로 조건이 참일 때만 블록이 실행됩니다.',
       'seed:4.2.1:if-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'if 문은 조건식이 참(true)인 경우에만 특정 문장 블록을 실행하기 위한 분기 구조이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'EASY',
       'if-else 문에서 if 조건이 거짓(false)이면, else 블록이 실행된다.',
       'O',
       'if 조건이 참이면 if 블록, 거짓이면 else 블록이 실행되는 기본 분기 구조입니다.',
       'seed:4.2.1:if-else:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'if-else 문에서 if 조건이 거짓(false)이면, else 블록이 실행된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'NORMAL',
       '중첩 if 문에서는 else 블록이 항상 가장 바깥쪽 if 문과 짝을 이루므로, 들여쓰기만 잘 맞추면 어떤 if와 연결되는지 쉽게 알 수 있다.',
       'X',
       '언어 규칙상 else는 가장 가까운 if와 짝을 이룹니다. 가독성을 위해 중괄호와 들여쓰기를 명확히 하는 것이 중요합니다.',
       'seed:4.2.1:dangling-else:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '중첩 if 문에서는 else 블록이 항상 가장 바깥쪽 if 문과 짝을 이루므로, 들여쓰기만 잘 맞추면 어떤 if와 연결되는지 쉽게 알 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'NORMAL',
       'switch 문에서는 break 문을 생략하면 다음 case로 실행이 이어지는 fall-through 현상이 발생할 수 있다.',
       'O',
       'break가 없으면 조건과 상관없이 다음 case 문장까지 실행되므로, 의도하지 않은 결과가 나올 수 있습니다.',
       'seed:4.2.1:switch-break:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'switch 문에서는 break 문을 생략하면 다음 case로 실행이 이어지는 fall-through 현상이 발생할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'NORMAL',
       'switch 문은 모든 언어에서 문자열, 실수 타입까지 동일하게 지원하므로, 타입 제약을 고려하지 않아도 된다.',
       'X',
       'C언어의 switch는 정수형/열거형만 허용하는 등, 언어마다 switch에서 허용하는 타입이 다릅니다.',
       'seed:4.2.1:switch-type:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'switch 문은 모든 언어에서 문자열, 실수 타입까지 동일하게 지원하므로, 타입 제약을 고려하지 않아도 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42101, 'WRITTEN', 'OX', 'EASY',
       '다중 분기 구조에서는 if-else if-else 체인이나 switch 문 등을 사용해 조건에 따라 서로 다른 처리를 수행할 수 있다.',
       'O',
       '여러 경우의 수를 분기 처리할 때 if-else if 또는 switch를 적절히 선택해 사용할 수 있습니다.',
       'seed:4.2.1:multi-branch:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '다중 분기 구조에서는 if-else if-else 체인이나 switch 문 등을 사용해 조건에 따라 서로 다른 처리를 수행할 수 있다.%'
);

/* ========================================================
 * 4.2.2 반복문  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'EASY',
       'for 문은 초기식, 조건식, 증감식을 이용해 정해진 횟수만큼 반복을 수행하는 데 자주 사용된다.',
       'O',
       'for (초기식; 조건식; 증감식) 형태로, 반복 횟수가 명확한 경우에 적합합니다.',
       'seed:4.2.2:for-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'for 문은 초기식, 조건식, 증감식을 이용해 정해진 횟수만큼 반복을 수행하는 데 자주 사용된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'EASY',
       'while 문은 조건식이 참인 동안 반복을 수행하는 구조이므로, 조건이 처음부터 거짓이면 한 번도 실행되지 않을 수 있다.',
       'O',
       'while은 사전 검사 반복문으로, 최초 조건이 거짓이면 반복 본문이 한 번도 실행되지 않습니다.',
       'seed:4.2.2:while:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'while 문은 조건식이 참인 동안 반복을 수행하는 구조이므로, 조건이 처음부터 거짓이면 한 번도 실행되지 않을 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'NORMAL',
       'do-while 문은 반복을 시작하기 전에 조건을 검사하므로, 조건이 거짓이면 반복 본문이 한 번도 실행되지 않는다.',
       'X',
       'do { … } while(조건); 구조는 최소 한 번은 본문을 실행한 뒤 조건을 검사하는 사후 검사 반복문입니다.',
       'seed:4.2.2:do-while:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'do-while 문은 반복을 시작하기 전에 조건을 검사하므로, 조건이 거짓이면 반복 본문이 한 번도 실행되지 않는다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'NORMAL',
       'break 문은 반복문 전체를 즉시 종료하고, continue 문은 현재 반복을 중단하고 다음 반복으로 넘어가는 역할을 한다.',
       'O',
       'break는 반복 자체를 끝내고, continue는 조건 검사 단계로 건너뛰어 다음 회차를 진행하게 합니다.',
       'seed:4.2.2:break-continue:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'break 문은 반복문 전체를 즉시 종료하고, continue 문은 현재 반복을 중단하고 다음 반복으로 넘어가는 역할을 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'NORMAL',
       '반복문에서 조건을 잘못 설정하거나 증감식을 누락하면 무한 루프가 발생할 수 있으므로, 종료 조건을 항상 명확히 설계해야 한다.',
       'O',
       '무한 루프는 CPU와 자원을 계속 점유하므로, 종료 조건과 증감식을 신중히 작성해야 합니다.',
       'seed:4.2.2:infinite-loop:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '반복문에서 조건을 잘못 설정하거나 증감식을 누락하면 무한 루프가 발생할 수 있으므로, 종료 조건을 항상 명확히 설계해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42102, 'WRITTEN', 'OX', 'EASY',
       '향상된 for 문(또는 for-each 문)은 컬렉션이나 배열의 모든 요소를 순차적으로 탐색할 때 유용하게 사용할 수 있다.',
       'O',
       '인덱스를 직접 다루지 않고도 배열·컬렉션 요소를 순회할 수 있어 가독성이 좋습니다.',
       'seed:4.2.2:foreach:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '향상된 for 문(또는 for-each 문)은 컬렉션이나 배열의 모든 요소를 순차적으로 탐색할 때 유용하게 사용할 수 있다.%'
);

/* ========================================================
 * 4.2.3 문자열 처리 (C 중심)  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'EASY',
       'C 언어에서 문자열은 마지막에 널 문자(\\0)가 포함된 char 배열로 표현된다.',
       'O',
       '문자열의 끝을 나타내는 널 문자가 있어야 strlen, strcpy 같은 함수가 문자열 경계를 인식할 수 있습니다.',
       'seed:4.2.3:string-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'C 언어에서 문자열은 마지막에 널 문자(\\0)가 포함된 char 배열로 표현된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'NORMAL',
       'C 언어의 strlen 함수는 널 문자(\\0)를 포함한 전체 버퍼 크기를 반환한다.',
       'X',
       'strlen은 널 문자를 제외한 실제 문자열 길이(문자 개수)만 반환합니다.',
       'seed:4.2.3:strlen:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'C 언어의 strlen 함수는 널 문자(\\0)를 포함한 전체 버퍼 크기를 반환한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'NORMAL',
       'strcpy 함수는 목적지 버퍼의 크기를 자동으로 확인하므로, 항상 버퍼 오버플로우 없이 안전하게 문자열을 복사할 수 있다.',
       'X',
       'strcpy는 버퍼 크기를 검사하지 않기 때문에, 목적지 크기를 넘는 문자열을 복사하면 버퍼 오버플로우가 발생할 수 있습니다.',
       'seed:4.2.3:strcpy:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'strcpy 함수는 목적지 버퍼의 크기를 자동으로 확인하므로, 항상 버퍼 오버플로우 없이 안전하게 문자열을 복사할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'NORMAL',
       'strcmp 함수는 두 문자열이 같으면 0을 반환하고, 사전식 순서에 따라 양수 또는 음수를 반환할 수 있다.',
       'O',
       'strcmp(s1, s2)는 s1과 s2를 비교해 같으면 0, 크면 양수, 작으면 음수를 반환합니다.',
       'seed:4.2.3:strcmp:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'strcmp 함수는 두 문자열이 같으면 0을 반환하고, 사전식 순서에 따라 양수 또는 음수를 반환할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'NORMAL',
       '문자열 상수(예: "ABC")는 읽기 전용 영역에 저장될 수 있으므로, 이를 직접 수정하는 코드는 정의되지 않은 동작을 일으킬 수 있다.',
       'O',
       '문자열 리터럴은 변경 불가능한 메모리에 위치할 수 있어, char *p = "ABC"; *p = ''X''; 와 같은 코드는 위험합니다.',
       'seed:4.2.3:string-literal:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '문자열 상수(예: "ABC")는 읽기 전용 영역에 저장될 수 있으므로, 이를 직접 수정하는 코드는 정의되지 않은 동작을 일으킬 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_42103, 'WRITTEN', 'OX', 'EASY',
       'C 문자열을 처리할 때는 항상 목적지 버퍼의 크기와 널 문자 공간을 고려해 안전한 함수 사용과 길이 검사를 수행해야 한다.',
       'O',
       '버퍼 크기를 넘는 복사는 대표적인 보안 취약점이므로, 길이 제한 함수(strncpy, strncat 등)와 적절한 검사를 병행해야 합니다.',
       'seed:4.2.3:string-safe:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_42103 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE 'C 문자열을 처리할 때는 항상 목적지 버퍼의 크기와 널 문자 공간을 고려해 안전한 함수 사용과 길이 검사를 수행해야 한다.%'
);

/* ========================================================
 * 4.3.1 함수/프로시저와 매개변수  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'EASY',
       '함수(Function)는 일정한 작업을 수행하는 코드 블록으로, 입력(매개변수)을 받아 결과 값을 반환할 수 있다.',
       'O',
       '함수는 코드를 재사용 가능하게 만들고, 복잡한 로직을 의미 있는 단위로 분리하는 데 사용됩니다.',
       'seed:4.3.1:function-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '함수(Function)는 일정한 작업을 수행하는 코드 블록으로, 입력(매개변수)을 받아 결과 값을 반환할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'NORMAL',
       '값에 의한 호출(call by value)에서는 함수가 매개변수로 전달된 인자의 원본 값 자체를 직접 변경할 수 있다.',
       'X',
       '값에 의한 호출은 인자의 복사본을 전달하므로, 함수 내부 변경은 원본 변수에 영향을 주지 않습니다.',
       'seed:4.3.1:call-by-value:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '값에 의한 호출(call by value)에서는 함수가 매개변수로 전달된 인자의 원본 값 자체를 직접 변경할 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'NORMAL',
       '참조에 의한 호출(call by reference)은 포인터나 참조 타입 등을 이용해, 함수가 호출자 변수의 값을 직접 변경할 수 있게 한다.',
       'O',
       '포인터/참조를 매개변수로 사용하면, 함수 내부에서 원본 데이터를 수정할 수 있습니다.',
       'seed:4.3.1:call-by-ref:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '참조에 의한 호출(call by reference)은 포인터나 참조 타입 등을 이용해, 함수가 호출자 변수의 값을 직접 변경할 수 있게 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'NORMAL',
       '재귀 함수는 자기 자신을 직접 또는 간접적으로 호출하는 함수로, 항상 반복문보다 메모리 사용량이 적다.',
       'X',
       '재귀 호출은 호출 스택을 사용하므로, 깊은 재귀에서는 반복문보다 오히려 메모리를 더 많이 사용할 수 있습니다.',
       'seed:4.3.1:recursion:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '재귀 함수는 자기 자신을 직접 또는 간접적으로 호출하는 함수로, 항상 반복문보다 메모리 사용량이 적다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'NORMAL',
       '함수의 인터페이스(매개변수 타입, 반환 타입)는 모듈 간 의존성을 줄이고 재사용성을 높이기 위해 안정적으로 설계해야 한다.',
       'O',
       '인터페이스가 자주 바뀌면 호출하는 모든 코드가 함께 수정되어야 하므로, 변경에 강한 설계가 중요합니다.',
       'seed:4.3.1:interface:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '함수의 인터페이스(매개변수 타입, 반환 타입)는 모듈 간 의존성을 줄이고 재사용성을 높이기 위해 안정적으로 설계해야 한다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43101, 'WRITTEN', 'OX', 'EASY',
       '함수 분리를 통해 한 함수가 하나의 책임에만 집중하도록 만드는 것은 가독성과 테스트 용이성을 높이는 데 도움이 된다.',
       'O',
       '단일 책임 원칙에 따라 함수를 작게 나누면, 이해하기 쉽고 테스트하기 쉬운 코드가 됩니다.',
       'seed:4.3.1:single-responsibility:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43101 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '함수 분리를 통해 한 함수가 하나의 책임에만 집중하도록 만드는 것은 가독성과 테스트 용이성을 높이는 데 도움이 된다.%'
);

/* ========================================================
 * 4.3.2 예외 처리·표준 라이브러리  (MICRO용 OX 6문항)
 * ======================================================== */

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'EASY',
       '예외 처리는 프로그램 실행 중 발생하는 오류 상황을 적절히 감지하고 복구하기 위한 메커니즘이다.',
       'O',
       '예외 처리를 통해 비정상 종료를 방지하고, 사용자에게 의미 있는 오류 메시지와 복구 절차를 제공할 수 있습니다.',
       'seed:4.3.2:exception-basic:1'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '예외 처리는 프로그램 실행 중 발생하는 오류 상황을 적절히 감지하고 복구하기 위한 메커니즘이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'NORMAL',
       '예외를 전혀 사용하지 않고 모든 오류를 반환값으로만 처리하면, 호출하는 쪽에서 오류 확인을 빼먹을 위험이 줄어든다.',
       'X',
       '반환값 기반 오류 처리는 호출자가 체크를 누락하기 쉽습니다. 언어와 상황에 맞는 예외 처리 방식을 적절히 사용하는 것이 좋습니다.',
       'seed:4.3.2:return-vs-exception:2'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '예외를 전혀 사용하지 않고 모든 오류를 반환값으로만 처리하면, 호출하는 쪽에서 오류 확인을 빼먹을 위험이 줄어든다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'NORMAL',
       '표준 라이브러리를 활용하면, 검증된 기능을 재사용할 수 있어 개발 생산성과 품질을 모두 높일 수 있다.',
       'O',
       '직접 구현하는 것보다 표준 라이브러리의 검증된 코드를 재사용하는 것이 일반적으로 더 안전하고 효율적입니다.',
       'seed:4.3.2:std-lib:3'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '표준 라이브러리를 활용하면, 검증된 기능을 재사용할 수 있어 개발 생산성과 품질을 모두 높일 수 있다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'NORMAL',
       '파일 입출력, 컬렉션, 수학 함수 등은 대부분 언어에서 표준 라이브러리로 제공되므로, 필요할 때마다 직접 구현하는 것이 일반적인 방식이다.',
       'X',
       '표준 라이브러리가 제공하는 기능은 직접 구현하기보다 그대로 사용하는 것이 유지보수성과 신뢰성 측면에서 유리합니다.',
       'seed:4.3.2:reinvent-wheel:4'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '파일 입출력, 컬렉션, 수학 함수 등은 대부분 언어에서 표준 라이브러리로 제공되므로, 필요할 때마다 직접 구현하는 것이 일반적인 방식이다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'NORMAL',
       '예외를 무조건 상위로 전파시키는 것보다는, 적절한 위치에서 로그를 남기고 의미 있는 메시지로 변환해주는 것이 유지보수에 도움이 된다.',
       'O',
       '적절한 계층에서 예외를 잡아 도메인에 맞는 메시지와 로그를 남기면, 문제 분석과 사용자 대응이 쉬워집니다.',
       'seed:4.3.2:handle-exception:5'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '예외를 무조건 상위로 전파시키는 것보다는, 적절한 위치에서 로그를 남기고 의미 있는 메시지로 변환해주는 것이 유지보수에 도움이 된다.%'
);

INSERT INTO question (cert_id, topic_id, mode, type, difficulty,
                      stem, answer_key, solution_text, source)
SELECT 1, @tp_43102, 'WRITTEN', 'OX', 'EASY',
       '표준 라이브러리 사용 시 공식 문서나 API 레퍼런스를 참고하면, 함수의 용도와 제약 사항을 정확히 이해할 수 있다.',
       'O',
       '오동작을 방지하려면, 사용하는 API의 입력 범위·예외 상황·반환값 의미를 문서를 통해 정확히 파악하는 것이 중요합니다.',
       'seed:4.3.2:api-doc:6'
WHERE NOT EXISTS (
  SELECT 1 FROM question
   WHERE topic_id=@tp_43102 AND mode='WRITTEN' AND type='OX'
     AND stem LIKE '표준 라이브러리 사용 시 공식 문서나 API 레퍼런스를 참고하면, 함수의 용도와 제약 사항을 정확히 이해할 수 있다.%'
);
