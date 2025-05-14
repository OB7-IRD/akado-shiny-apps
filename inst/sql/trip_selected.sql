--- retrieve trip IDs for selected items
SELECT 
    t.topiaid::text AS trip_id,
    v.code::text AS vessel_code,
    v.topiaid::text AS vessel_id,
    t.enddate::date AS trip_enddate,
    t.startdate::date AS trip_startdate,
    w.label1::text AS wellcontentstatus_landing_label
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN ps_logbook.wellcontentstatus w ON t.landingwellcontentstatus = w.topiaid 
WHERE 
    coalesce(t.logbookprogram,'NULL') IN (?select_item_1) AND
    -- Selected trip with the vessel code and the end date of the trip
    ((((?select_item_2) = 'NA' AND (?select_item_3) IN ('')) OR (v.code = NULLIF( ?select_item_2 , '')::text AND t.enddate = NULLIF( ?select_item_3 , '')::date)) AND
    -- Selected trip with a date range
    (((?select_item_4) IN ('') AND (?select_item_5) IN ('')) OR (t.startdate >= NULLIF( ?select_item_4 , '')::date AND t.enddate <= NULLIF( ?select_item_5 , '')::date)) 
    )
