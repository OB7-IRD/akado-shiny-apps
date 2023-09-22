--- Recovers information about well
SELECT
    w.trip::text AS trip_id,
    w.well::text AS well_well
FROM 
    ps_logbook.well w 
WHERE 
    w.trip IN (?select_item)
