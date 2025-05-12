--- Recovers information about previous trip
SELECT 
	trip.trip_id::text AS trip_id, 
	trip_previous.topiaid::text AS trip_previous_id,
	h.topiaid::text AS harbour_id_landing_trip_previous,
	h.label1::text AS harbour_label_landing_trip_previous, 
	trip.vessel_code::text AS vessel_code,
	trip.trip_enddate::date AS trip_enddate,
	trip.harbour_id_departure::text AS harbour_id_departure,
	trip.harbour_label_departure::text AS harbour_label_departure,
	trip.harbour_id_landing::text AS harbour_id_landing,
	trip.harbour_label_landing::text AS harbour_label_landing
FROM (
	SELECT 
		t.topiaid::text AS trip_id,
		t.vessel::text AS vessel_id, 
		t.enddate::date AS trip_enddate,
		v.code::text AS vessel_code,
		hd.topiaid::text AS harbour_id_departure,
		hd.label1::text AS harbour_label_departure,
		hl.topiaid::text AS harbour_id_landing,
		hl.label1::text AS harbour_label_landing,
		max(trip_previous_tmp.enddate) AS trip_previous_enddate
	FROM 
		ps_common.trip t
		LEFT JOIN common.vessel v ON t.vessel = v.topiaid
		LEFT JOIN common.harbour hd ON t.departureharbour = hd.topiaid
		LEFT JOIN common.harbour hl ON t.landingharbour = hl.topiaid
		LEFT JOIN  ps_common.trip trip_previous_tmp ON t.vessel = trip_previous_tmp.vessel AND trip_previous_tmp.topiaid <> t.topiaid AND trip_previous_tmp.enddate <= t.startdate AND coalesce(trip_previous_tmp.logbookprogram,'NULL') IN (?select_item_1)
	WHERE 
		t.topiaid IN (?select_item_2) OR t.topiaid IN (
			-- Selection of trips corresponding to the most recent trip for vessels with trips not in the database being extracted
			SELECT t.topiaid
			FROM ps_common.trip t
			INNER JOIN common.vessel v ON t.vessel = v.topiaid
			INNER JOIN (
				-- Selection of first trip dates for selected vessels
				SELECT max(t.enddate), v.topiaid
				FROM ps_common.trip t
				INNER JOIN common.vessel v ON t.vessel = v.topiaid
				WHERE coalesce(t.logbookprogram,'NULL') IN (?select_item_1) AND t.vessel IN (
					SELECT DISTINCT v.topiaid
					FROM (
					-- Selection of vessels whose trip is not in the database being extracted
					SELECT unnest(array[ ?select_item_3 ]) AS vessel_id, 
									unnest(array[ ?select_item_2 ]) AS trip_id) trip_vessel_id 
						INNER JOIN common.vessel v ON trip_vessel_id.vessel_id= v.topiaid  
						LEFT JOIN ps_common.trip t ON trip_vessel_id.trip_id = t.topiaid 
				WHERE t.topiaid IS NULL)
			GROUP BY v.topiaid) AS vessel_first_enddatetrip ON vessel_first_enddatetrip.topiaid = v.topiaid AND vessel_first_enddatetrip.max = t.enddate and coalesce(t.logbookprogram,'NULL') IN (?select_item_1)
		)
	GROUP BY t.topiaid, v.topiaid, hd.topiaid, hl.topiaid
	) AS trip
	-- trip selection and corresponding info for the previous trip (same date and same ship)
	LEFT JOIN ps_common.trip trip_previous ON trip.vessel_id = trip_previous.vessel AND trip.trip_previous_enddate = trip_previous.enddate 
	LEFT JOIN common.harbour h ON trip_previous.landingharbour = h.topiaid
WHERE 
	coalesce(trip_previous.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous.topiaid IS NULL 
