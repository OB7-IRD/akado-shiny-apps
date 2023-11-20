--- Recovers information about wellactivityspecies
SELECT 
    was.topiaid::text AS wellactivityspecies_id, 
	was.wellactivity::text AS wellactivity_id, 
	wc.code::text AS weightcategory_code, 
	s.faocode::text AS specie_name, 
	was.weight::numeric AS wellactivityspecies_weight
FROM 
    ps_logbook.wellactivityspecies was 
    LEFT JOIN ps_common.weightcategory wc ON was.weightcategory = wc.topiaid 
    LEFT JOIN common.species s ON was.species = s.topiaid 
WHERE 
    was.wellactivity IN (?select_item)
