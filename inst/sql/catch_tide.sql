--- Recovers information about the weight of each capture of the activity
SELECT 
    c.topiaid::text AS catch_id,
    a.topiaid::text AS activity_id, 
    t.topiaid::text AS trip_id,
    c.weight::numeric AS catch_weight,
    sf.code::text AS speciesfate_code,
    s.faocode::text AS species_fao_code, 
    v.code::text AS vesselactivity_code
FROM 
    ps_common.trip t
    LEFT JOIN ps_logbook.route r ON r.trip = t.topiaid
    LEFT JOIN ps_logbook.activity a ON a.route = r.topiaid 
    LEFT JOIN ps_logbook.catch c ON c.activity = a.topiaid 
    LEFT JOIN ps_common.speciesfate sf ON c.speciesfate = sf.topiaid 
    LEFT JOIN ps_common.vesselactivity v ON a.vesselactivity = v.topiaid
    LEFT JOIN common.species s ON c.species = s.topiaid
WHERE 
    t.topiaid IN (?select_item) 
