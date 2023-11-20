--- Returns the trip ID if associated with a route and an activity for the trip
SELECT 
    r.trip::text AS trip_id 
FROM 
    ps_logbook.activity a
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid
WHERE 
    r.trip IN (?select_item)
