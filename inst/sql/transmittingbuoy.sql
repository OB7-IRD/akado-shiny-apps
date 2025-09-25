--- Recovers information about the transmittingbuoy of the activity
SELECT 
    tb.topiaid::text AS transmittingbuoy_id,
    tbo.code::text  AS transmittingbuoyoperation_code,
    a.topiaid::text AS activity_id,
    f.topiaid::text AS floatingobject_id,
    tb.code::text AS transmittingbuoy_code
FROM 
    ps_logbook.transmittingbuoy tb 
    LEFT JOIN ps_common.transmittingbuoyoperation tbo ON tb.transmittingbuoyoperation = tbo.topiaid 
    INNER JOIN ps_logbook.floatingobject f ON tb.floatingobject = f.topiaid 
    INNER JOIN ps_logbook.activity a ON f.activity = a.topiaid 
WHERE 
    a.topiaid IN (?select_item) 
