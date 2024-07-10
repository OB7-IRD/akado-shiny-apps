--- Recovers information about landing
SELECT
    l.topiaid::text AS landing_id,
    l.trip::text AS trip_id,
    l.weight::numeric AS landing_weight,
    s.faocode::text AS species_fao_code
FROM 
    ps_landing.landing l
    LEFT JOIN common.species s ON l.species = s.topiaid 
WHERE 
    l.trip IN (?select_item)
