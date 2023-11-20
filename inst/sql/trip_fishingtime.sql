--- Recovers information about the species caught, the ocean declared in the trip
SELECT 
    t.topiaid::text AS trip_id,
    t.fishingtime::numeric AS trip_fishingtime 
FROM 
    ps_common.trip t
WHERE 
    t.topiaid IN (?select_item)
