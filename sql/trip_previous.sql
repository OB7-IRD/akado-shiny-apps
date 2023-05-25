--- Recovers information about the previous trip ID 
SELECT 
	trip_previous_enddate.topiaid::text AS trip_id, 
	trip_previous.topiaid::text AS trip_previous_id
FROM (
	SELECT 
		trip.topiaid, 
		trip.vessel, 
		max(trip_previous_tmp.enddate) AS trip_previous_enddate
	FROM 
		ps_common.trip trip
		LEFT JOIN  ps_common.trip trip_previous_tmp ON trip.vessel = trip_previous_tmp.vessel AND trip_previous_tmp.enddate < trip.startdate AND coalesce(trip_previous_tmp.logbookprogram,'NULL') IN (?select_item_1)
	WHERE 
		trip.topiaid IN (?select_item_2)
	GROUP BY trip.topiaid
	) AS trip_previous_enddate
	-- trip selection and corresponding info for the previous trip (same date and same ship)
	LEFT JOIN ps_common.trip trip_previous ON trip_previous_enddate.vessel = trip_previous.vessel AND trip_previous_enddate.trip_previous_enddate = trip_previous.enddate 
WHERE 
	coalesce(trip_previous.logbookprogram,'NULL') IN (?select_item_1) OR trip_previous.topiaid IS NULL 
