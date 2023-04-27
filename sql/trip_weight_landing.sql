--- Recovers information about the weight at each landing of the trip for canneries
SELECT 
    t.topiaid::text AS trip_id, 
    l.weight::numeric AS landing_weight
FROM 
    ps_landing.landing l 
    INNER JOIN ps_common.trip t ON l.trip = t.topiaid
WHERE 
    t.topiaid IN (?select_item)
