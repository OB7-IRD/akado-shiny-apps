--- retrieve trip IDs for selected items
SELECT 
    t.topiaid::text AS trip_id
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
    coalesce(t.logbookprogram,'NULL') IN (?select_item_1) AND
    (v.code IN (?select_item_2) AND t.enddate IN (?select_item_3))
    (t.startdate >= ?select_item_2 AND t.enddate <= ?select_item_3)
