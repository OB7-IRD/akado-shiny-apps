--- Recovers information about samplespeciesmeasure IDs and samplespeciesmeasure identification
SELECT 
    sm.topiaid::text AS samplespeciesmeasure_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    sa.number::integer AS sample_number,
    s.faocode::text AS specie_name,
    smt.code::text AS sizemeasuretype_code
FROM 
    ps_logbook.samplespeciesmeasure sm 
    INNER JOIN ps_logbook.samplespecies se ON sm.samplespecies = se.topiaid
    INNER JOIN ps_logbook.sample sa ON se.sample = sa.topiaid
    INNER JOIN ps_common.trip t ON sa.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN common.species s ON se.species = s.topiaid
    LEFT JOIN common.sizemeasuretype smt ON se.sizemeasuretype = smt.topiaid
WHERE 
    t.topiaid IN (?select_item) 
