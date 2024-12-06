--- Recovers information about the floatingobject of the activity
SELECT 
    fo.topiaid::text AS floatingobject_id,
    oo.code::text  AS objectoperation_code,
    a.topiaid::text AS activity_id
FROM 
    ps_logbook.floatingobject fo
    INNER JOIN ps_common.objectoperation oo ON fo.objectoperation = oo.topiaid 
    INNER JOIN ps_logbook.activity a ON fo.activity = a.topiaid
WHERE 
    a.topiaid IN (?select_item) 
