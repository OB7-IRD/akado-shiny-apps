--- Reconstructs complete full trips from selected trips
SELECT DISTINCT
	trip_full_trip.topiaid::text AS trip_id, 
	trip_end_full_trip.trip_current_end_full_trip_id::text AS trip_end_full_trip_id,
	trip_full_trip.vessel::text AS vessel_id,
	c.code::text AS country_fleetcountry,
	trip_full_trip.enddate::date AS trip_enddate
FROM 
 	(
	-- Search for information on the end trip of the current full_trip and the end trip of the previous full trip
	SELECT 
		trip_ennddate_end_full_trip.topiaid AS trip_id, 
		trip_previous_end_full_trip.topiaid AS trip_previous_end_full_trip_id,
		trip_previous_end_full_trip.enddate AS trip_previous_end_full_trip_enddate,
		trip_current_end_full_trip.topiaid AS trip_current_end_full_trip_id,
		trip_current_end_full_trip.enddate AS trip_current_end_full_trip_enddate,
		trip_ennddate_end_full_trip.vessel AS vessel
	FROM (
		-- Search for the end date of the current full_trip and the end date of the previous full trip
		SELECT 
			trip.topiaid, 
			trip.vessel, 
			max(trip_previous_end_full_trip_tmp.enddate) AS trip_previous_enddate,
			min(trip_current_end_full_trip_tmp.enddate) AS trip_current_enddate
		FROM 
			ps_common.trip trip
			LEFT JOIN  ps_common.trip trip_previous_end_full_trip_tmp ON trip.vessel = trip_previous_end_full_trip_tmp.vessel AND trip_previous_end_full_trip_tmp.enddate <= trip.startdate AND coalesce(trip_previous_end_full_trip_tmp.logbookprogram,'NULL') IN (?select_item_1) AND trip_previous_end_full_trip_tmp.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1') AND trip_previous_end_full_trip_tmp.topiaid <> trip.topiaid
			LEFT JOIN  ps_common.trip trip_current_end_full_trip_tmp ON trip.vessel = trip_current_end_full_trip_tmp.vessel AND trip_current_end_full_trip_tmp.enddate >= trip.enddate AND trip_current_end_full_trip_tmp.enddate >= trip.startdate AND coalesce(trip_current_end_full_trip_tmp.logbookprogram,'NULL') IN (?select_item_1) AND trip_current_end_full_trip_tmp.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
		WHERE 
			trip.topiaid IN (?select_item_2) OR trip.topiaid IN (
				-- Selection of trips corresponding to the most recent and oldest trip for vessels with trips not in the database being extracted
				SELECT t.topiaid
				FROM ps_common.trip t
				INNER JOIN common.vessel v ON t.vessel = v.topiaid
				INNER JOIN (
					-- Selection of last and first trip dates for selected vessels
					SELECT min(t.enddate), max(t.enddate), v.topiaid
					FROM ps_common.trip t
					INNER JOIN common.vessel v ON t.vessel = v.topiaid
					WHERE coalesce(t.logbookprogram,'NULL') IN (?select_item_1) AND t.vessel IN (
						SELECT DISTINCT v.topiaid
						FROM (
						-- Selection of vessels whose trip is not in the database being extracted
						SELECT unnest(array[ ?select_item_3 ]) AS vessel_id, 
										unnest(array[ ?select_item_2 ]) AS trip_id) trip_vessel_id 
							INNER JOIN common.vessel v ON trip_vessel_id.vessel_id = v.topiaid  
							LEFT JOIN ps_common.trip t ON trip_vessel_id.trip_id = t.topiaid 
							WHERE t.topiaid IS NULL)
				GROUP BY v.topiaid) AS vessel_first_enddatetrip_last_enddatetrip ON vessel_first_enddatetrip_last_enddatetrip.topiaid = v.topiaid AND (vessel_first_enddatetrip_last_enddatetrip.min = t.enddate OR vessel_first_enddatetrip_last_enddatetrip.max = t.enddate) and coalesce(t.logbookprogram,'NULL') IN (?select_item_1)
			)
		GROUP BY trip.topiaid
		) AS trip_ennddate_end_full_trip
		LEFT JOIN ps_common.trip trip_previous_end_full_trip ON trip_ennddate_end_full_trip.vessel = trip_previous_end_full_trip.vessel AND trip_ennddate_end_full_trip.trip_previous_enddate = trip_previous_end_full_trip.enddate AND trip_previous_end_full_trip.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
		LEFT JOIN ps_common.trip trip_current_end_full_trip ON trip_ennddate_end_full_trip.vessel = trip_current_end_full_trip.vessel AND trip_ennddate_end_full_trip.trip_current_enddate = trip_current_end_full_trip.enddate  AND trip_current_end_full_trip.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
	WHERE 
		(coalesce(trip_previous_end_full_trip.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous_end_full_trip.topiaid IS NULL) 
	 	AND
		(coalesce(trip_current_end_full_trip.logbookprogram,'NULL') IN (?select_item_1) OR trip_current_end_full_trip.topiaid IS NULL) 
	) AS trip_end_full_trip
	LEFT JOIN ps_common.trip trip_full_trip ON trip_full_trip.vessel = trip_end_full_trip.vessel AND (trip_end_full_trip.trip_current_end_full_trip_enddate IS NULL OR trip_full_trip.enddate <= trip_end_full_trip.trip_current_end_full_trip_enddate) AND (trip_end_full_trip.trip_previous_end_full_trip_enddate IS NULL OR trip_full_trip.enddate > trip_end_full_trip.trip_previous_end_full_trip_enddate) AND coalesce(trip_full_trip.logbookprogram,'NULL') IN (?select_item_1)
	LEFT JOIN common.vessel v on trip_full_trip.vessel = v.topiaid
	LEFT JOIN common.country c ON v.fleetcountry = c.topiaid 
