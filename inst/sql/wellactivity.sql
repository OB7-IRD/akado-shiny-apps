--- Recovers information about wellactivity
SELECT 
    wa.topiaid::text AS wellactivity_id, 
	wa.well::text AS well_id
FROM 
    ps_logbook.wellactivity wa 
WHERE 
    wa.topiaid IN (?select_item)
