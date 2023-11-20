--- Recovers information about sample species
SELECT
    se.topiaid::text AS samplespecies_id,
    se.subsamplenumber::numeric AS samplespecies_subsamplenumber,
    se.sample::text AS sample_id,
    s.faocode::text AS specie_name,
    smt.code::text AS sizemeasuretype_code
FROM 
    ps_logbook.samplespecies se
    LEFT JOIN common.species s ON se.species = s.topiaid
    LEFT JOIN common.sizemeasuretype smt ON se.sizemeasuretype = smt.topiaid
WHERE 
    se.topiaid IN (?select_item)
