--- Recovers information about sampleactivity
SELECT
    saa.sample::text AS sample_id,
    saa.weightedweight::numeric AS sampleactivity_weightedweight    
FROM 
    ps_logbook.sampleactivity saa
WHERE 
    saa.sample IN (?select_item)
