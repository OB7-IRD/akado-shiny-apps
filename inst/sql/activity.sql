--- Recovers information about activities IDs and activity 
SELECT 
    a.topiaid::text AS activity_id,
    a.number::integer AS activity_number,
    r."date"::date AS activity_date,
    a.time::time AS activity_time,
    v.code::text AS vessel_code,
    t.topiaid::text AS trip_id,
    st_asText(a.the_geom)::text AS activity_position,
    ST_SRID(a.the_geom)::numeric AS activity_crs
FROM 
    ps_logbook.activity a 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    a.topiaid IN (?select_item) 
