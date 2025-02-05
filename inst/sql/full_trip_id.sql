--- Reconstructs complete full trips from selected trips
SELECT 
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
			trip.topiaid IN (?select_item_2) 
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
