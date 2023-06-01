--- Recovers information about of weights caught for certain species, relative to the calculation of RF1
SELECT 
    r.trip::text AS trip_id, 
    c.weight::numeric AS catch_weight
FROM 
    ps_logbook.catch c
    INNER JOIN ps_logbook.activity a ON c.activity = a.topiaid
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.speciesfate s ON c.speciesfate = s.topiaid 
    INNER JOIN ps_common.vesselactivity v ON a.vesselactivity = v.topiaid 
WHERE 
    r.trip IN (?select_item_1)
    AND s.code = '6' 
    AND c.species IN (?select_item_2)
    AND v.code NOT IN ('25', '27', '29')
