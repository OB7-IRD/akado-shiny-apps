--- Recovers information about well
SELECT
    w.topiaid::text AS well_id,
    w.trip::text AS trip_id,
    w.well::text AS well_well
FROM 
    ps_logbook.well w 
WHERE 
    w.trip IN (?select_item)
