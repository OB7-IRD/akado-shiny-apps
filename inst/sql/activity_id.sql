--- Recovers information about activities IDs and activity identification
SELECT 
    a.topiaid::text AS activity_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    r."date"::date AS activity_date,
    a.number::integer AS activity_number
FROM 
    ps_logbook.activity a 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    t.topiaid IN (?select_item) 
