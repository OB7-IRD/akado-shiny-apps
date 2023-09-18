--- Recovers information about super sample
SELECT
    sa.topiaid::text AS sample_id,
    sa.supersample::boolean AS sample_supersample
FROM 
    ps_logbook.sample sa
WHERE 
    sa.topiaid IN (?select_item)
