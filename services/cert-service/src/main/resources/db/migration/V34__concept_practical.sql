SET NAMES utf8mb4;
SET FOREIGN_KEY_CHECKS = 0;

USE certpilot_cert;

-- =========================================
-- 실기 P.5.x 개념 (백업/복구 / 장애 분석)
--  - topic_id = 31501 (P.5.1), 31502 (P.5.2)
-- =========================================

SET @tp_backup   := 31501; -- P.5.1 백업 및 복구 전략 수립
SET @tp_incident := 31502; -- P.5.2 장애 분석 및 개선안 도출

/* ========================================================
 * P.5.1 백업 및 복구 전략 수립
 * topic_id = 31501
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_backup,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.5.1.1 백업의 기본 개념과 유형
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.5.1.1',
        'title', '백업 기본 개념과 대표 유형 정리',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','백업은 왜 중요한가?'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '백업은 장애나 실수로 인한 데이터 손실에 대비해 데이터를 다른 위치에 복제해 두는 작업입니다. 실기 시험에서는 단순히 백업 종류를 외우는 수준을 넘어서, 주어진 시스템에 가장 적절한 백업 및 복구 전략을 설계하는 능력을 요구합니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','대표적인 백업 유형 비교',
            'headers', JSON_ARRAY('백업 유형','내용','장점','단점'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '전체 백업 (Full Backup)',
                '데이터베이스 전체를 통째로 백업',
                '복구 절차가 단순하고 직관적이다.',
                '백업 시간과 저장 공간이 많이 필요하다.'
              ),
              JSON_ARRAY(
                '증분 백업 (Incremental Backup)',
                '마지막 백업 이후 변경된 데이터만 백업',
                '백업 시간이 짧고 저장 공간을 절약할 수 있다.',
                '복구 시 전체 백업과 여러 개의 증분 백업을 순서대로 적용해야 하므로 복잡하다.'
              ),
              JSON_ARRAY(
                '차등 백업 (Differential Backup)',
                '마지막 전체 백업 이후 변경된 데이터를 누적해서 백업',
                '복구 시 전체 백업 + 최신 차등 백업만 적용하면 된다.',
                '시간이 지날수록 백업 용량이 점점 커질 수 있다.'
              ),
              JSON_ARRAY(
                '로그 백업 (Log Backup)',
                '트랜잭션 로그를 주기적으로 백업',
                '장애 발생 시 특정 시점까지 복구(Point-in-Time Recovery)가 가능하다.',
                '로그 관리 전략이 없으면 로그 파일이 너무 커질 수 있다.'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','RPO / RTO 개념 함께 묶어서 이해하기'),
          JSON_OBJECT(
            'type','table',
            'caption','RPO / RTO 요약',
            'headers', JSON_ARRAY('용어','뜻','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'RPO',
                'Recovery Point Objective',
                '얼마 전 시점까지의 데이터가 보존되어야 하는가?',
                'RPO = 10분이면, 장애 시 최대 10분치 데이터 손실까지 허용한다는 의미이다.'
              ),
              JSON_ARRAY(
                'RTO',
                'Recovery Time Objective',
                '시스템이 어느 정도 시간 안에 복구되어야 하는가?',
                'RTO = 1시간이면, 장애 발생 후 1시간 이내에 서비스를 복구해야 한다는 의미이다.'
              )
            )
          ),

          JSON_OBJECT(
            'type','paragraph',
            'text',
            '백업 주기와 방식은 RPO·RTO 요구사항에 따라 결정됩니다. 예를 들어, RPO 5분을 요구하는 금융 서비스라면 로그 백업 주기를 매우 짧게 가져가야 합니다.'
          )
        )
      ),

      /* -------------------------------------
         P.5.1.2 백업·복구 전략 수립 절차
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.5.1.2',
        'title', '요구사항 기반 백업·복구 전략 설계 절차',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','전략 수립 4단계'),
          JSON_OBJECT(
            'type','table',
            'caption','백업·복구 전략 수립 단계',
            'headers', JSON_ARRAY('단계','주요 질문','산출물 예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '1단계: 중요도 분석',
                '어떤 시스템과 데이터가 가장 중요한가?',
                '시스템 분류표, 중요도 등급, 서비스 목록'
              ),
              JSON_ARRAY(
                '2단계: RPO·RTO 정의',
                '각 서비스별로 어느 정도 데이터 손실과 다운타임을 허용할 수 있는가?',
                '서비스별 RPO·RTO 매트릭스'
              ),
              JSON_ARRAY(
                '3단계: 백업 방식·주기 결정',
                '어떤 백업 유형을 어떤 주기로 수행할 것인가?',
                '백업 정책 문서, 스케줄(일·주·월 단위)'
              ),
              JSON_ARRAY(
                '4단계: 복구 시나리오·테스트 설계',
                '장애 유형별로 어떻게 복구할 것인가, 실제로 잘 동작하는가?',
                '복구 절차서, 모의 복구 테스트 결과 보고서'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','실기에서 자주 출제되는 서술형 포인트'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '“주어진 시스템에서 적절한 백업 주기와 방법을 서술하시오.”와 같이, 요구사항을 보고 백업 전략을 고르는 문제',
              '“RPO 30분, RTO 2시간을 만족하기 위한 백업 및 복구 방안을 제시하시오.”와 같은 서술형 문제',
              '“전체 백업만 수행할 때의 문제점과 이를 보완할 수 있는 전략을 쓰시오.”와 같이 장단점을 비교하는 문제'
            )
          )
        )
      ),

      /* -------------------------------------
         P.5.1.3 복구 절차와 모의 복구 테스트
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.5.1.3',
        'title', '복구 절차 수립과 정기적인 모의 복구',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','복구 절차서에는 무엇이 들어가야 할까'),
          JSON_OBJECT(
            'type','table',
            'caption','복구 절차서 기본 구성',
            'headers', JSON_ARRAY('항목','내용'),
            'rows', JSON_ARRAY(
              JSON_ARRAY('장애 유형 정의','디스크 장애, 데이터 삭제, 데이터 손상, 데이터센터 장애 등'),
              JSON_ARRAY('복구 대상 범위','어떤 시스템·DB·서비스까지 포함되는지 명시'),
              JSON_ARRAY('복구 순서','서비스 중단 공지 → 백업본 확인 → 복구 수행 → 검증 → 서비스 재개'),
              JSON_ARRAY('역할·책임','누가 복구를 수행하고, 누가 승인하며, 누구에게 보고하는지'),
              JSON_ARRAY('검증 방법','복구 후 어떤 테스트를 수행해 정상 동작을 확인할지')
            )
          ),

          JSON_OBJECT('type','heading','text','모의 복구 테스트의 중요성'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '백업이 정상적으로 수행되고 있는지, 실제로 복구에 사용할 수 있는지 주기적으로 검증해야 한다.',
              '복구 시간이 RTO 안에 들어오는지, 실제 측정값을 기록해 두어야 한다.',
              '장애 상황에서 담당자가 바뀌더라도, 절차서만 보고 복구를 수행할 수 있을 정도로 문서를 구체적으로 작성해야 한다.'
            )
          ),

          JSON_OBJECT(
            'type','paragraph',
            'text',
            '실기에서는 “백업은 했지만 복구 테스트를 하지 않아 실제 장애 시 복구에 실패했다”는 사례를 제시하고, 무엇이 문제였는지와 개선 방안을 쓰게 하는 형태로도 출제될 수 있습니다.'
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

/* ========================================================
 * P.5.2 장애 분석 및 개선안 도출
 * topic_id = 31502
 * ======================================================== */
INSERT INTO concept (topic_id, sections_json)
VALUES (
  @tp_incident,
  JSON_OBJECT(
    'sections', JSON_ARRAY(

      /* -------------------------------------
         P.5.2.1 장애 라이프사이클과 분류
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 1,
        'subCode', 'P.5.2.1',
        'title', '장애 라이프사이클과 유형 분류',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','장애(Incident)의 개념'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '장애는 서비스 품질이 저하되거나, 서비스가 완전히 중단되어 사용자에게 영향을 주는 사건을 말합니다. 실기에서는 장애 발생부터 복구, 재발 방지까지의 전체 흐름을 얼마나 체계적으로 관리하는지가 중요한 평가 포인트입니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','장애 라이프사이클 요약',
            'headers', JSON_ARRAY('단계','설명','예시 활동'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '탐지 (Detection)',
                '모니터링·알림·사용자 신고 등을 통해 장애 발생 사실을 인지',
                '모니터링 시스템에서 오류율 급증 알림, 고객센터 문의 폭주'
              ),
              JSON_ARRAY(
                '대응 (Response)',
                '장애를 제한하고, 추가 확산을 막기 위한 초기 대응 수행',
                '트래픽 우회, 문제 있는 서비스 임시 중단, 긴급 공지'
              ),
              JSON_ARRAY(
                '복구 (Recovery)',
                '서비스를 정상 상태로 되돌리는 작업 수행',
                '롤백, 재배포, 서버 재기동, 데이터 복구 등'
              ),
              JSON_ARRAY(
                '분석 (Analysis)',
                '장애 원인과 영향 범위를 분석하고, 개선 방안을 도출',
                '로그 분석, 타임라인 정리, 근본 원인 규명'
              ),
              JSON_ARRAY(
                '개선 (Improvement)',
                '재발 방지를 위한 기술적·프로세스 개선을 적용',
                '코드 수정, 모니터링 보강, 프로세스 변경, 교육'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','장애 유형을 나누어 보는 기준'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '원인 기준: 인적 오류(배포 실수, 설정 오타), 시스템 오류(버그, 리소스 부족), 외부 요인(통신사 장애, 전원 문제 등)',
              '영향 범위 기준: 단일 서비스 장애, 특정 기능 장애, 전체 서비스 장애, 특정 지역/채널 제한적 장애',
              '심각도 기준: 경미, 주의, 심각, 치명 등으로 구분하여 대응 우선순위를 정한다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.5.2.2 장애 분석(포스트모템) 리포트 구성
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 2,
        'subCode', 'P.5.2.2',
        'title', '장애 분석 리포트(포스트모템) 작성 요소',
        'importance', 5,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','포스트모템(Postmortem) 리포트란?'),
          JSON_OBJECT(
            'type','paragraph',
            'text',
            '장애가 발생한 후, 장애의 경과와 원인, 영향, 대응 과정을 정리하고, 재발 방지 방안을 문서화한 보고서를 포스트모템 리포트라고 합니다. 실기에서는 이 리포트의 구성 요소를 쓰게 하거나, 예시 리포트를 보고 빠진 항목을 찾게 하는 문제가 자주 등장합니다.'
          ),

          JSON_OBJECT(
            'type','table',
            'caption','장애 분석 리포트 기본 구성',
            'headers', JSON_ARRAY('항목','내용 예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '장애 개요',
                '발생 일시, 복구 일시, 장애 ID, 담당 조직, 장애 요약'
              ),
              JSON_ARRAY(
                '영향 범위',
                '영향 받은 시스템·서비스, 사용자 수, 거래 건수, 매출 영향 등'
              ),
              JSON_ARRAY(
                '타임라인',
                '장애 탐지부터 복구 완료까지의 주요 이벤트를 시간 순으로 정리'
              ),
              JSON_ARRAY(
                '원인 분석',
                '직접 원인과 근본 원인을 구분해 서술 (예: 설정값 오류 vs 검증 프로세스 부재)'
              ),
              JSON_ARRAY(
                '대응 및 복구 내용',
                '장애 기간 동안 수행한 조치, 우회 방법, 최종 복구 방식'
              ),
              JSON_ARRAY(
                '재발 방지 대책',
                '코드·구성 변경, 모니터링·알림 강화, 운영 프로세스 개선, 교육 계획'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','근본 원인(Root Cause)을 찾는 관점'),
          JSON_OBJECT(
            'type','list',
            'items', JSON_ARRAY(
              '단순히 “개발자가 실수했다”에서 끝내지 않고, 왜 그런 실수가 시스템적으로 방지되지 못했는지를 분석해야 한다.',
              '사람, 프로세스, 기술(시스템) 관점으로 나누어 원인을 찾으면 재발 방지 대책을 더 잘 설계할 수 있다.',
              '5Why 기법처럼 “왜?”를 여러 번 반복해 진짜 근본 원인에 도달하는 방식이 자주 사용된다.'
            )
          )
        )
      ),

      /* -------------------------------------
         P.5.2.3 재발 방지 대책과 지표 관리
         ------------------------------------- */
      JSON_OBJECT(
        'orderNo', 3,
        'subCode', 'P.5.2.3',
        'title', '개선안 도출과 장애 관련 지표 관리',
        'importance', 4,
        'blocks', JSON_ARRAY(

          JSON_OBJECT('type','heading','text','재발 방지 대책의 유형'),
          JSON_OBJECT(
            'type','table',
            'caption','재발 방지 대책 분류',
            'headers', JSON_ARRAY('유형','내용','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                '기술적 대책',
                '시스템 구조나 코드를 수정하여 같은 상황이 다시 발생해도 장애로 이어지지 않도록 함',
                '입력값 검증 추가, 장애 전파를 막는 서킷브레이커 도입, 자동 롤백 스크립트 작성'
              ),
              JSON_ARRAY(
                '프로세스 개선',
                '업무 절차와 승인·검증 과정을 보완',
                '배포 전 리뷰·테스트 절차 강화, 변경 관리(Change Management) 프로세스 도입'
              ),
              JSON_ARRAY(
                '모니터링·알림 강화',
                '이상 징후를 더 빨리 탐지할 수 있도록 지표와 알림을 개선',
                '오류율·응답 시간·자원 사용률에 대한 임계치 설정, 슬랙·문자 알림 연동'
              ),
              JSON_ARRAY(
                '교육·훈련',
                '유사한 실수가 반복되지 않도록 담당자 역량을 강화',
                '정기적인 장애 대응 훈련, 운영 가이드 공유, 신규 인력 온보딩 교육'
              )
            )
          ),

          JSON_OBJECT('type','heading','text','장애 관련 지표(서비스 안정성 지표) 예시'),
          JSON_OBJECT(
            'type','table',
            'caption','장애·가용성 관련 주요 지표',
            'headers', JSON_ARRAY('지표','설명','예시'),
            'rows', JSON_ARRAY(
              JSON_ARRAY(
                'MTBF (Mean Time Between Failures)',
                '평균 고장 간격. 장애 사이의 평균 시간',
                '값이 클수록 장애가 드물게 발생한다는 의미이다.'
              ),
              JSON_ARRAY(
                'MTTR (Mean Time To Repair)',
                '평균 복구 시간. 장애 발생부터 복구까지 평균 소요 시간',
                '값이 작을수록 장애 대응이 빠르다는 의미이다.'
              ),
              JSON_ARRAY(
                '가용성(Availability)',
                '시스템이 정상적으로 서비스를 제공한 시간의 비율',
                '연간 가용성 99.9% 등으로 목표를 설정하고 운영한다.'
              )
            )
          ),

          JSON_OBJECT(
            'type','paragraph',
            'text',
            '실기에서는 “장애가 자주 발생하고 복구 시간이 길어지는 상황에서 개선해야 할 지표와 조치를 쓰시오.”처럼, 지표 이름과 의미, 개선 방향을 함께 서술하는 문제가 나올 수 있습니다.'
          )
        )
      )

    )
  )
)
ON DUPLICATE KEY UPDATE
  sections_json = VALUES(sections_json);

SET FOREIGN_KEY_CHECKS = 1;
