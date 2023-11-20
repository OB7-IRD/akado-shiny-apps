--- Recovers information about the weigth catch of the activity
SELECT 
    a.topiaid::text AS activity_id, 
    a.totalweight::numeric AS activity_weight
FROM 
    ps_logbook.activity a
WHERE 
    a.topiaid IN (?select_item) 
