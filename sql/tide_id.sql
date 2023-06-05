--- Reconstructs complete tides from selected trips
SELECT 
	trip_tide.topiaid::text AS trip_id, 
	trip_end_tide.trip_current_end_tide_id::text AS trip_end_tide_id, 
	trip_end_tide.trip_previous_end_tide_id::text AS trip_previous_end_tide_id
FROM 
 	(
	-- Search for information on the end trip of the current tide and the end trip of the previous tide
	SELECT 
		trip_ennddate_end_tide.topiaid AS trip_id, 
		trip_previous_end_tide.topiaid AS trip_previous_end_tide_id,
		trip_previous_end_tide.enddate AS trip_previous_end_tide_enddate,
		trip_current_end_tide.topiaid AS trip_current_end_tide_id,
		trip_current_end_tide.enddate AS trip_current_end_tide_enddate,
		trip_ennddate_end_tide.vessel AS vessel
	FROM (
		-- Search for the end date of the current tide and the end date of the previous tide
		SELECT 
			trip.topiaid, 
			trip.vessel, 
			max(trip_previous_end_tide_tmp.enddate) AS trip_previous_enddate,
			min(trip_current_end_tide_tmp.enddate) AS trip_current_enddate
		FROM 
			ps_common.trip trip
			LEFT JOIN  ps_common.trip trip_previous_end_tide_tmp ON trip.vessel = trip_previous_end_tide_tmp.vessel AND trip_previous_end_tide_tmp.enddate < trip.startdate AND coalesce(trip_previous_end_tide_tmp.logbookprogram,'NULL') IN (?select_item_1) AND trip_previous_end_tide_tmp.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
			LEFT JOIN  ps_common.trip trip_current_end_tide_tmp ON trip.vessel = trip_current_end_tide_tmp.vessel AND trip_current_end_tide_tmp.enddate >= trip.startdate AND coalesce(trip_current_end_tide_tmp.logbookprogram,'NULL') IN (?select_item_1) AND trip_current_end_tide_tmp.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
		WHERE 
			trip.topiaid IN (?select_item_2) 
		GROUP BY trip.topiaid
		) AS trip_ennddate_end_tide
		LEFT JOIN ps_common.trip trip_previous_end_tide ON trip_ennddate_end_tide.vessel = trip_previous_end_tide.vessel AND trip_ennddate_end_tide.trip_previous_enddate = trip_previous_end_tide.enddate AND trip_previous_end_tide.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
		LEFT JOIN ps_common.trip trip_current_end_tide ON trip_ennddate_end_tide.vessel = trip_current_end_tide.vessel AND trip_ennddate_end_tide.trip_current_enddate = trip_current_end_tide.enddate  AND trip_current_end_tide.landingwellcontentstatus = (SELECT w.topiaid FROM ps_logbook.wellcontentstatus w WHERE w.code = '1')
	WHERE 
		(coalesce(trip_previous_end_tide.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous_end_tide.topiaid IS NULL) 
	 	AND
		(coalesce(trip_current_end_tide.logbookprogram,'NULL') IN (?select_item_1) OR trip_current_end_tide.topiaid IS NULL) 
	) AS trip_end_tide
	LEFT JOIN ps_common.trip trip_tide ON trip_tide.vessel = trip_end_tide.vessel AND (trip_end_tide.trip_current_end_tide_enddate IS NULL OR trip_tide.enddate <= trip_end_tide.trip_current_end_tide_enddate) AND (trip_end_tide.trip_previous_end_tide_enddate IS NULL OR trip_tide.enddate > trip_end_tide.trip_previous_end_tide_enddate) AND coalesce(trip_tide.logbookprogram,'NULL') IN (?select_item_1)
