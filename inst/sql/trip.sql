--- Recovers information about trip
SELECT
    t.topiaid::text AS trip_id,
    vt.code::text AS vessel_type_code, 
    vt.label1::text AS vesseltype_name,
    t.fishingtime::numeric AS trip_fishingtime,
    t.timeatsea::numeric AS trip_seatime  
FROM 
    ps_common.trip t
    LEFT JOIN common.vessel v ON t.vessel = v.topiaid 
    LEFT JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid 
WHERE 
    t.topiaid IN (?select_item)
