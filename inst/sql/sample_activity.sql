--- Recovers information about the link between sample and samplespecies
SELECT
    sa.topiaid::text AS sample_id,
    a.topiaid::text AS activity_id    
FROM 
    ps_logbook.sample sa
    LEFT JOIN ps_logbook.sampleactivity saa ON saa.sample = sa.topiaid
    LEFT JOIN ps_logbook.activity a ON saa.activity = a.topiaid
WHERE 
    sa.topiaid IN (?select_item)
