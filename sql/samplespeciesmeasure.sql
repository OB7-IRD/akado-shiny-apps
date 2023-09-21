--- Recovers information about species, measurement type and size class of the sample
SELECT
    ssm.topiaid::text AS samplespeciesmeasure_id, 
    ssm.sizeclass::numeric AS samplespeciesmeasure_sizeclass,
    ssm.count::numeric AS samplespeciesmeasure_count,
    ssm.samplespecies::text AS samplespecies_id
 FROM 
    ps_logbook.samplespeciesmeasure ssm 
WHERE 
    ssm.topiaid IN (?select_item)
