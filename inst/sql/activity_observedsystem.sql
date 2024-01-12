--- Recovers information about activities related to associations of object types
SELECT 
    o.topiaid::text AS observedsystem_id,
    a.topiaid::text AS activity_id,
    s.code::text AS schooltype_code
FROM 
    ps_logbook.activity a 
	LEFT JOIN ps_logbook.activity_observedsystem ao ON ao.activity = a.topiaid 
	LEFT JOIN ps_common.observedsystem o ON ao.observedsystem = o.topiaid
    LEFT JOIN ps_common.schooltype s ON o.schooltype = s.topiaid
WHERE 
    a.topiaid IN (?select_item) 
