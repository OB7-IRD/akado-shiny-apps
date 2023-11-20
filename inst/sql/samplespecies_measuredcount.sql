--- Recovers information about the number of individuals measured in each sample
SELECT 
    sa.topiaid::text AS sample_id,
    se.measuredcount::numeric AS samplespecies_measuredcount
FROM 
    ps_logbook.sample sa
    LEFT JOIN ps_logbook.samplespecies se ON se.sample = sa.topiaid
WHERE 
    sa.topiaid IN (?select_item)
