--- Recovers information about species, measurement type and size class of the sample
SELECT
    ssm.topiaid::text AS samplespeciesmeasure_id, 
    ssm.sizeclass::numeric AS samplespeciesmeasure_sizeclass,
    ssm.count::numeric AS samplespeciesmeasure_count,
    ssm.samplespecies::text AS samplespecies_id,
    s.faocode::text AS specie_code, 
    smt.code::text AS sizemeasuretype_code
 FROM 
    ps_logbook.samplespeciesmeasure ssm 
    LEFT JOIN ps_logbook.samplespecies se ON ssm.samplespecies = se.topiaid
    LEFT JOIN common.SizeMeasureType smt ON se.sizemeasuretype = smt.topiaid
    LEFT JOIN common.species s ON se.species = s.topiaid
WHERE 
    ssm.topiaid IN (?select_item)
