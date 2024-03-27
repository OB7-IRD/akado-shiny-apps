--- Recovers information about the weight of each capture of the activity
SELECT 
    c.topiaid::text AS catch_id,
    c.weight::numeric AS catch_weight,
    a.topiaid::text AS activity_id,
    sf.code::text AS speciesfate_code,
    s.faocode::text AS species_fao_code
FROM 
    ps_logbook.activity a
    LEFT JOIN ps_logbook.catch c ON c.activity = a.topiaid 
    LEFT JOIN ps_common.speciesfate sf ON c.speciesfate = sf.topiaid 
    LEFT JOIN common.species s ON c.species = s.topiaid
WHERE 
    a.topiaid IN (?select_item) 
