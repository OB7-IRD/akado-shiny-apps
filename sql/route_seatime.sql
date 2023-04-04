--- Recovers information about the sea time declared in the route
SELECT 
    r.trip::text AS trip_id,
    r.timeatsea::numeric AS route_sea_time 
FROM 
    ps_logbook.route r 
WHERE 
    r.trip IN (?select_item)
