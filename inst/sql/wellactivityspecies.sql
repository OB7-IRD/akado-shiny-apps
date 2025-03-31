--- Recovers information about wellactivityspecies
SELECT 
    was.topiaid::text AS wellactivityspecies_id, 
	was.wellactivity::text AS wellactivity_id, 
	wc.code::text AS weightcategory_code, 
	s.faocode::text AS species_fao_code, 
	was.weight::numeric AS wellactivityspecies_weight,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    w.well::text AS well_label
FROM 
    ps_logbook.wellactivityspecies was 
    INNER JOIN ps_logbook.wellactivity wa ON was.wellactivity = wa.topiaid
    INNER JOIN ps_logbook.well w ON wa.well = w.topiaid
    INNER JOIN ps_common.trip t ON w.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN ps_common.weightcategory wc ON was.weightcategory = wc.topiaid 
    LEFT JOIN common.species s ON was.species = s.topiaid 
WHERE 
    t.topiaid IN (?select_item)
