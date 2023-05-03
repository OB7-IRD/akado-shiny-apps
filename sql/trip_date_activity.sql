--- Recovers information about the start and end date of the trip and the dates of the activities
SELECT 
    t.topiaid::text AS trip_id, 
    t.startdate::date AS trip_startdate, 
    t.enddate::date AS trip_enddate,  
    r."date"::date AS activity_date
FROM 
    ps_logbook.route r
	INNER JOIN ps_common.trip t ON r.trip = t.topiaid
WHERE 
    t.topiaid IN (?select_item)
