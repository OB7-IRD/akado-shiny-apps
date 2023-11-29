--- Recovers information about the species caught, the ocean declared in the trip
SELECT 
    r.topiaid::text AS route_id,
    r.trip::text AS trip_id,
    r.fishingtime::numeric AS route_fishingtime 
FROM 
    ps_logbook.route r 
WHERE 
    r.trip IN (?select_item)
