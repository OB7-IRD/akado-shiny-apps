--- Recovers information about sample
SELECT
    sa.topiaid::text AS sample_id,
    sa.supersample::boolean AS sample_supersample,
    sa.well::text AS sample_well,
    sa.trip::text AS trip_id,
    sa.smallsweight::numeric AS sample_smallsweight, 
    sa.bigsweight::numeric AS sample_bigsweight,
    sa.totalweight::numeric AS sample_totalweight,
    st.code::text AS sampletype_code
FROM 
    ps_logbook.sample sa
    LEFT JOIN ps_common.sampletype st ON sa.sampletype = st.topiaid
WHERE 
    sa.topiaid IN (?select_item)
