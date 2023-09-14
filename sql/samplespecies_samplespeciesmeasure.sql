--- Recovers information about the link between samplespecies and samplespeciesmeasure
SELECT
    se.topiaid::text AS samplespecies_id, 
    ssm.topiaid::text AS samplespeciesmeasure_id
FROM 
    ps_logbook.samplespecies se 
    LEFT JOIN ps_logbook.samplespeciesmeasure ssm ON ssm.samplespecies = se.topiaid
WHERE 
    se.topiaid IN (?select_item)
