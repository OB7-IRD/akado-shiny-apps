--- Recovers information about sample species
SELECT
    se.topiaid::text AS samplespecies_id,
    se.subsamplenumber::numeric AS samplespecies_subsamplenumber,
    se.sample::text AS sample_id,
    s.faocode::text AS specie_name,
    smt.code::text AS sizemeasuretype_code,
    se.measuredcount::numeric AS samplespecies_measuredcount,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    sa.number::integer AS sample_number
FROM 
    ps_logbook.samplespecies se
    INNER JOIN ps_logbook.sample sa ON se.sample = sa.topiaid
    INNER JOIN ps_common.trip t ON sa.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN common.species s ON se.species = s.topiaid
    LEFT JOIN common.sizemeasuretype smt ON se.sizemeasuretype = smt.topiaid
WHERE 
    t.topiaid IN (?select_item) 

