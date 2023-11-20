--- Recovers information about link between trip and wellactivity
SELECT
    t.topiaid::text AS trip_id,
    wa.topiaid::text AS wellactivity_id
FROM 
    ps_common.trip t
    LEFT JOIN ps_logbook.well w ON w.trip = t.topiaid 
    LEFT JOIN ps_logbook.wellactivity wa ON wa.well = w.topiaid 
WHERE 
    t.topiaid IN (?select_item)
