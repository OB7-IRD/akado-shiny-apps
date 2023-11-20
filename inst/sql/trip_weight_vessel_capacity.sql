--- Recovers information about the total landed weight for canneries and the local market in the trip and vessel capacity link to trip
SELECT t.topiaid::text AS trip_id,
    t.landingtotalweight::numeric AS trip_landingtotalweight, 
    t.localmarkettotalweight::numeric AS trip_localmarkettotalweight,
    v.capacity::numeric AS vessel_capacity
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid 
WHERE 
    t.topiaid IN (?select_item)
