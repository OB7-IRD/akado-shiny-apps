--- retrieve trip IDs for selected items
SELECT 
    t.topiaid::text AS trip_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    t.topiaid IN (?select_item) 
