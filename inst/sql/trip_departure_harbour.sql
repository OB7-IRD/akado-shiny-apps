--- Recovers information about the port of departure identifier
SELECT 
	t.topiaid::text AS trip_id,
	h.topiaid::text AS harbour_id,
	h.label1::text AS harbour_name
FROM 
	ps_common.trip t
	INNER JOIN common.harbour h ON t.departureharbour = h.topiaid
WHERE 
	t.topiaid IN (?select_item)
