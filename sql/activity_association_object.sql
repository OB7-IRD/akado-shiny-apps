--- Recovers information about activities related to associations of object types
SELECT 
    a.topiaid::text AS activity_id
FROM 
    ps_logbook.activity a 
	INNER JOIN ps_logbook.activity_observedsystem ao ON ao.activity = a.topiaid 
	INNER JOIN ps_common.observedsystem o ON ao.observedsystem = o.topiaid
    INNER JOIN ps_common.schooltype s ON o.schooltype = s.topiaid
WHERE 
    a.topiaid IN (?select_item) 
    AND s.code = '1'
