--- retrieve trip IDs for selected items
SELECT 
    t.topiaid::text AS trip_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    w.label1::text AS wellcontentstatus_landing_label
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN ps_logbook.wellcontentstatus w ON t.landingwellcontentstatus = w.topiaid 
WHERE 
    coalesce(t.logbookprogram,'NULL') IN (?select_item_1) AND
    (t.startdate >= ?select_item_2 AND t.enddate <= ?select_item_3)
