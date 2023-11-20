--- Recovers information about the number of individuals measured in each sample species
SELECT 
    sa.topiaid::text AS sample_id,
    ssm.count::numeric AS samplespeciesmeasure_count 
FROM 
    ps_logbook.samplespeciesmeasure ssm
    INNER JOIN ps_logbook.samplespecies se ON ssm.samplespecies = se.topiaid
    INNER JOIN ps_logbook.sample sa ON se.sample = sa.topiaid
WHERE 
    sa.topiaid IN (?select_item)
