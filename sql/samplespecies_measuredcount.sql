--- Recovers information about the number of individuals measured in each sample
SELECT 
    sa.topiaid::text AS sample_id,
    se.measuredcount::numeric AS samplespecies_measuredcount
FROM 
    ps_logbook.samplespecies se 
    INNER JOIN ps_logbook.sample sa ON se.sample = sa.topiaid
WHERE 
    sa.topiaid IN (?select_item)
