--- Recovers information about subsample numbers
SELECT
    se.topiaid::text AS samplespecies_id,
    se.subsamplenumber::numeric AS samplespecies_subsamplenumber,
    se.sample::text AS sample_id
FROM 
    ps_logbook.samplespecies se
WHERE 
    se.topiaid IN (?select_item)
