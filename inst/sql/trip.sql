--- Recovers information about trip
SELECT 
	trip.trip_id::text AS trip_id, 
	trip_previous.topiaid::text AS trip_previous_id,
	h.topiaid::text AS harbour_id_landing_trip_previous,
	h.label1::text AS harbour_label_landing_trip_previous,
    trip.vesseltype_code::text AS vesseltype_code, 
    trip.vesseltype_label::text AS vesseltype_label,
    trip.trip_fishingtime::numeric AS trip_fishingtime,
    trip.trip_seatime::numeric AS trip_seatime,
    trip.trip_landingtotalweight::numeric AS trip_landingtotalweight, 
    trip.trip_localmarkettotalweight::numeric AS trip_localmarkettotalweight,
    trip.vessel_capacity::numeric AS vessel_capacity,
    trip.trip_startdate::date AS trip_startdate, 
    trip.trip_enddate::date AS trip_enddate,
    trip.harbour_id_departure::text AS harbour_id_departure,
    trip.harbour_label_departure::text AS harbour_label_departure,
    trip.harbour_position_departure::text AS harbour_position_departure,
    trip.harbour_position_landing::text AS harbour_position_landing
FROM (
	SELECT 
		t.topiaid::text AS trip_id,
		t.vessel::text AS vessel_id, 
        vt.code::text AS vesseltype_code, 
        vt.label1::text AS vesseltype_label,
        t.fishingtime::numeric AS trip_fishingtime,
        t.timeatsea::numeric AS trip_seatime,
        t.landingtotalweight::numeric AS trip_landingtotalweight, 
        t.localmarkettotalweight::numeric AS trip_localmarkettotalweight,
        v.capacity::numeric AS vessel_capacity,
        t.startdate::date AS trip_startdate, 
        t.enddate::date AS trip_enddate,
        hd.topiaid::text AS harbour_id_departure,
        hd.label1::text AS harbour_label_departure,
        st_asText(hd.the_geom)::text AS harbour_position_departure,
        st_asText(hl.the_geom)::text AS harbour_position_landing,
		max(trip_previous_tmp.enddate) AS trip_previous_enddate
	FROM 
		ps_common.trip t
        LEFT JOIN common.vessel v ON t.vessel = v.topiaid 
        LEFT JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid 
        LEFT JOIN common.harbour hd ON t.departureharbour = hd.topiaid
        LEFT JOIN common.harbour hl ON t.landingharbour = hl.topiaid
		LEFT JOIN  ps_common.trip trip_previous_tmp ON t.vessel = trip_previous_tmp.vessel AND trip_previous_tmp.topiaid <> t.topiaid AND trip_previous_tmp.enddate <= t.startdate AND coalesce(trip_previous_tmp.logbookprogram,'NULL') IN (?select_item_1)
	WHERE 
		t.topiaid IN (?select_item_2)
	GROUP BY t.topiaid, vt.topiaid, v.capacity, hd.topiaid, hl.topiaid
	) AS trip
	-- trip selection and corresponding info for the previous trip (same date and same ship)
	LEFT JOIN ps_common.trip trip_previous ON trip.vessel_id = trip_previous.vessel AND trip.trip_previous_enddate = trip_previous.enddate 
	LEFT JOIN common.harbour h ON trip_previous.landingharbour = h.topiaid
WHERE 
	coalesce(trip_previous.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous.topiaid IS NULL 
