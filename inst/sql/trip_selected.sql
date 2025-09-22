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
    t.topiaid IN (?select_item_1)
