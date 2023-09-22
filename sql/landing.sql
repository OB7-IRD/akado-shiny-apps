--- Recovers information about landing
SELECT
    l.trip::text AS trip_id,
    l.weight::numeric AS landing_weight,
    wc.code::text AS weightcategory_code
FROM 
    ps_landing.landing l
    LEFT JOIN ps_common.weightcategory wc ON l.weightcategory = wc.topiaid
WHERE 
    l.trip IN (?select_item)
