--- Recovers information about species, measurement type and size class of the sample
SELECT
    ssm.topiaid::text AS samplespeciesmeasure_id, 
    ssm.sizeclass::numeric AS samplespeciesmeasure_sizeclass,
    ssm.count::numeric AS samplespeciesmeasure_count,
    ssm.samplespecies::text AS samplespecies_id,
    s.faocode::text AS species_fao_code, 
    smt.code::text AS sizemeasuretype_code,
    sa.topiaid::text AS sample_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    sa.number::integer AS sample_number,
    se.subsamplenumber::numeric AS samplespecies_subsamplenumber
 FROM 
    ps_logbook.samplespeciesmeasure ssm 
    INNER JOIN ps_logbook.samplespecies se ON ssm.samplespecies = se.topiaid
    INNER JOIN ps_logbook.sample sa ON se.sample = sa.topiaid
    INNER JOIN ps_common.trip t ON sa.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN common.species s ON se.species = s.topiaid
    LEFT JOIN common.sizemeasuretype smt ON se.sizemeasuretype = smt.topiaid
WHERE 
    t.topiaid IN (?select_item) 
