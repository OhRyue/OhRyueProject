DELETE FROM report_tag_skill
WHERE (user_id = 'ohryue' AND tag = '인터페이스요구' AND exam_mode = 'WRITTEN')
   OR (user_id = 'ohryue' AND tag = 'OOP'           AND exam_mode = 'WRITTEN')
   OR (user_id = 'user2'  AND tag = '공통모듈'      AND exam_mode = 'WRITTEN');
