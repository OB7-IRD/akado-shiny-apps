--- Recovers information about the route
SELECT 
    r.topiaid::text AS route_id,
    r.trip::text AS trip_id,
    r.fishingtime::numeric AS route_fishingtime,
    r.timeatsea::numeric AS route_seatime,
    r."date"::date AS activity_date,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate
FROM 
    ps_logbook.route r 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    t.topiaid IN (?select_item)
