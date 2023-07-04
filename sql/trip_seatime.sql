--- Recovers information about the sea time declared in the trip
SELECT 
    t.topiaid::text AS trip_id,
    t.timeatsea::numeric AS trip_seatime 
FROM 
    ps_common.trip t
WHERE 
    t.topiaid IN (?select_item)
