--- Recovers information about the weight of each capture of the activity
SELECT 
    a.topiaid::text AS activity_id, 
    c.weight::numeric AS catch_weight
FROM 
    ps_logbook.activity a
    LEFT JOIN ps_logbook.catch c ON c.activity = a.topiaid 
WHERE 
    a.topiaid IN (?select_item) 
