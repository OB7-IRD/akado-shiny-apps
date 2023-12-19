--- Recovers information about sampleactivity
SELECT
    saa.sample::text AS sample_id,
    saa.weightedweight::numeric AS sampleactivity_weightedweight,
    saa.activity::text AS activity_id
FROM 
    ps_logbook.sampleactivity saa
WHERE 
    saa.sample IN (?select_item)
