--- Recovers information about the sea surface temperature of the activity
SELECT 
    a.topiaid::text AS activity_id, 
    a.seasurfacetemperature::numeric AS activity_seasurfacetemperature
FROM 
    ps_logbook.activity a
WHERE 
    a.topiaid IN (?select_item) 
