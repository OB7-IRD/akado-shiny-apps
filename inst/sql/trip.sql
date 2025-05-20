--- Recovers information about trip
SELECT 
    t.topiaid::text AS trip_id,
    vt.code::text AS vesseltype_code, 
    vt.label1::text AS vesseltype_label,
    t.fishingtime::numeric AS trip_fishingtime,
    t.timeatsea::numeric AS trip_seatime,
    t.landingtotalweight::numeric AS trip_landingtotalweight, 
    t.localmarkettotalweight::numeric AS trip_localmarkettotalweight,
    v.capacity::numeric AS vessel_capacity,
    v.code::text AS vessel_code,
    t.startdate::date AS trip_startdate, 
    t.enddate::date AS trip_enddate,
    hd.topiaid::text AS harbour_id_departure,
    hd.label1::text AS harbour_label_departure,
    st_asText(hd.the_geom)::text AS harbour_position_departure,
    hl.topiaid::text AS harbour_id_landing,
    hl.label1::text AS harbour_label_landing,
    st_asText(hl.the_geom)::text AS harbour_position_landing,
    w.label1::text AS wellcontentstatus_landing_label
FROM 
    ps_common.trip t
    LEFT JOIN common.vessel v ON t.vessel = v.topiaid 
    LEFT JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid 
    LEFT JOIN common.harbour hd ON t.departureharbour = hd.topiaid
    LEFT JOIN common.harbour hl ON t.landingharbour = hl.topiaid
    LEFT JOIN ps_logbook.wellcontentstatus w ON t.landingwellcontentstatus = w.topiaid 
WHERE 
	t.topiaid IN (?select_item_1)
