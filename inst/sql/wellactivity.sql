--- Recovers information about wellactivity
SELECT 
    wa.topiaid::text AS wellactivity_id, 
	wa.well::text AS well_id
FROM 
    ps_common.trip t
    LEFT JOIN ps_logbook.well w ON w.trip = t.topiaid 
    LEFT JOIN ps_logbook.wellactivity wa ON wa.well = w.topiaid 
WHERE 
    t.topiaid IN (?select_item)
