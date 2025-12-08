-- 정보처리기사 종목 정보 수동 입력 (시연용)
-- jmCd: 1320 (정보처리기사)
-- 시연을 위해 큐넷 API 대신 수동으로 입력

INSERT INTO qualification (
  jm_cd,
  series_cd,
  jm_nm,
  eng_jm_nm,
  series_nm,
  impl_nm,
  insti_nm,
  summary,
  job,
  trend,
  career,
  hist
) VALUES (
  '1320',                    -- jm_cd (종목코드)
  '03',                      -- series_cd (03: 기사)
  '정보처리기사',             -- jm_nm (자격명)
  'Information Processing Engineer',  -- eng_jm_nm (영문명)
  '기사',                    -- series_nm (자격등급)
  '한국산업인력공단',        -- impl_nm (시행기관)
  '고용노동부',              -- insti_nm (관련부처)
  '소프트웨어 개발 및 정보시스템 구축과 운영에 필요한 전문 지식과 실무 능력을 갖춘 전문 인력 양성을 목적으로 하는 국가기술자격이다. 컴퓨터 시스템의 하드웨어와 소프트웨어를 활용하여 정보처리 업무를 수행할 수 있는 능력을 평가한다.',  -- summary (개요)
  '정보처리 업무에 필요한 시스템 분석·설계·개발·운영 업무를 수행한다. 구체적으로 소프트웨어 개발, 데이터베이스 설계 및 구축, 프로그래밍, 정보시스템 운영 관리 등의 직무를 수행한다.',  -- job (수행직무)
  '빅데이터, 클라우드 컴퓨팅, AI 등 신기술 도입에 따라 정보처리 기술자의 역할이 확대되고 있다. 특히 데이터 분석, 시스템 통합, 보안 강화 등에 대한 전문성이 중요해지고 있다. 실무 중심의 문제 해결 능력과 최신 기술 트렌드를 이해하는 것이 필수적이다.',  -- trend (출제경향)
  '주로 정보통신업, 제조업, 금융업, 공공기관, 교육기관 등 다양한 분야로 진출할 수 있다. 특히 SI(시스템통합) 업체, IT 서비스 기업, 소프트웨어 개발 회사, 기업의 정보시스템 부서, 정부기관 및 공공기관의 정보화 사업 부서 등으로의 진출이 활발하다. 최근에는 클라우드 서비스, 빅데이터, AI 분야에서도 전문 인력으로 활동하고 있다.',  -- career (진로 및 전망)
  '1974년 10월 16일 대통령령 제7283호에 의해 제정되었고, 이후 1997년 12월 31일 대통령령 제8799호, 1983년 12월 20일 대통령령 제11281호, 1998년 5월 9일 대통령령 제15794호 등을 거쳐 현재에 이르고 있다. 2010년 12월 13일 고용노동부령 제11호로 현재의 자격 체계로 정비되었다.'  -- hist (변천과정)
)
ON DUPLICATE KEY UPDATE
  series_cd = VALUES(series_cd),
  jm_nm = VALUES(jm_nm),
  eng_jm_nm = VALUES(eng_jm_nm),
  series_nm = VALUES(series_nm),
  impl_nm = VALUES(impl_nm),
  insti_nm = VALUES(insti_nm),
  summary = VALUES(summary),
  job = VALUES(job),
  trend = VALUES(trend),
  career = VALUES(career),
  hist = VALUES(hist);


