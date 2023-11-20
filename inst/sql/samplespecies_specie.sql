--- Recovers information about species, measurement type and size class of the sample
SELECT
    se.topiaid::text AS samplespecies_id, 
    s.faocode::text AS specie_name
FROM 
    ps_logbook.samplespecies se 
    LEFT JOIN common.species s ON se.species = s.topiaid
WHERE 
    se.topiaid IN (?select_item)
