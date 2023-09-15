--- Recovers information about the link between sample and samplespecies
SELECT
    sa.topiaid::text AS sample_id,
    se.topiaid::text AS samplespecies_id    
FROM 
    ps_logbook.sample sa
    LEFT JOIN ps_logbook.samplespecies se ON se.sample = sa.topiaid
WHERE 
    sa.topiaid IN (?select_item)
