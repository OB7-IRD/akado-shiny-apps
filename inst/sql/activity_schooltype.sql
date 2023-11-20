--- Recovers information about the school type of the activity
SELECT 
    a.topiaid::text AS activity_id,
    s.code::text AS schooltype_code
FROM 
    ps_logbook.activity a 
    LEFT JOIN ps_common.schooltype s ON a.schooltype = s.topiaid
WHERE 
    a.topiaid IN (?select_item) 
