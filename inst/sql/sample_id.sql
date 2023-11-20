--- Recovers information about sample IDs and sample identification
SELECT 
    s.topiaid::text AS sample_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    s.number::integer AS sample_number
FROM 
    ps_logbook.sample s 
    INNER JOIN ps_common.trip t ON s.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    t.topiaid IN (?select_item) 
