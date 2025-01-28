--- Recovers information about activitie (not necessarily linked to trips but to a VMS comparison range)
SELECT 
    a.topiaid::text AS activity_id,
    v.code::text AS vessel_code,
    r."date"::date AS activity_date
FROM 
    ps_logbook.activity a 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
WHERE 
   r."date" >= ?select_item_1 AND 
   r."date" <= ?select_item_2 AND
   (v.code::text IN (?select_item_3) OR CONCAT(?select_item_3) IN (''))
