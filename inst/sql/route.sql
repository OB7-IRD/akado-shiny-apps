--- Recovers information about the route
SELECT 
    r.topiaid::text AS route_id,
    r.trip::text AS trip_id,
    r.fishingtime::numeric AS route_fishingtime,
    r.timeatsea::numeric AS route_seatime,
    r."date"::date AS activity_date
FROM 
    ps_logbook.route r 
WHERE 
    r.trip IN (?select_item)
