--- Recovers information about sample
SELECT
    sa.topiaid::text AS sample_id,
    sa.supersample::boolean AS sample_supersample,
    sa.well::text AS sample_well,
    sa.trip::text AS sample_trip
FROM 
    ps_logbook.sample sa
WHERE 
    sa.topiaid IN (?select_item)
