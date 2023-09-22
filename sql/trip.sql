--- Recovers information about trip
SELECT
    t.topiaid::text AS trip_id,
    vt.code::text AS vesseltype_code,
    vt.label1::text AS vesseltype_label1
FROM 
    ps_common.trip t
    LEFT JOIN common.vessel v ON t.vessel = v.topiaid 
    LEFT JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid 
WHERE 
    t.topiaid IN (?select_item)
