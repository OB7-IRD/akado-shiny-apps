--- Recovers information about trip
SELECT 
	trip.trip_id::text AS trip_id, 
	h.topiaid::text AS harbour_id_landing_trip_previous,
	h.label1::text AS harbour_name_landing_trip_previous,
    trip.vessel_type_code::text AS vessel_type_code, 
    trip.vesseltype_name::text AS vesseltype_name,
    trip.trip_fishingtime::numeric AS trip_fishingtime,
    trip.trip_seatime::numeric AS trip_seatime,
    trip.trip_landingtotalweight::numeric AS trip_landingtotalweight, 
    trip.trip_localmarkettotalweight::numeric AS trip_localmarkettotalweight,
    trip.vessel_capacity::numeric AS vessel_capacity,
    trip.trip_startdate::date AS trip_startdate, 
    trip.trip_enddate::date AS trip_enddate,
    trip.harbour_id::text AS harbour_id_departure,
	trip.harbour_name::text AS harbour_name_departure
FROM (
	SELECT 
		t.topiaid::text AS trip_id,
		t.vessel::text AS vessel_id, 
        vt.code::text AS vessel_type_code, 
        vt.label1::text AS vesseltype_name,
        t.fishingtime::numeric AS trip_fishingtime,
        t.timeatsea::numeric AS trip_seatime,
        t.landingtotalweight::numeric AS trip_landingtotalweight, 
        t.localmarkettotalweight::numeric AS trip_localmarkettotalweight,
        v.capacity::numeric AS vessel_capacity,
        t.startdate::date AS trip_startdate, 
        t.enddate::date AS trip_enddate,
        h.topiaid::text AS harbour_id,
	    h.label1::text AS harbour_name,
		max(trip_previous_tmp.enddate) AS trip_previous_enddate
	FROM 
		ps_common.trip t
        LEFT JOIN common.vessel v ON t.vessel = v.topiaid 
        LEFT JOIN common.vesseltype vt ON v.vesseltype = vt.topiaid 
        LEFT JOIN common.harbour h ON t.departureharbour = h.topiaid
		LEFT JOIN  ps_common.trip trip_previous_tmp ON t.vessel = trip_previous_tmp.vessel AND trip_previous_tmp.enddate <= t.startdate AND coalesce(trip_previous_tmp.logbookprogram,'NULL') IN (?select_item_1)
	WHERE 
		t.topiaid IN (?select_item_2)
	GROUP BY t.topiaid, vt.code, vt.label1, v.capacity, h.topiaid, h.label1
	) AS trip
	-- trip selection and corresponding info for the previous trip (same date and same ship)
	LEFT JOIN ps_common.trip trip_previous ON trip.vessel_id = trip_previous.vessel AND trip.trip_previous_enddate = trip_previous.enddate 
	LEFT JOIN common.harbour h ON trip_previous.landingharbour = h.topiaid
WHERE 
	coalesce(trip_previous.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous.topiaid IS NULL 
