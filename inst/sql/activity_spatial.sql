--- Recovers information about the location of the activity, whether in a harbour, in a ocean
SELECT 
    a.topiaid::text AS activity_id, 
    tmp_harbour.topiaid::text AS harbour_id
FROM 
    ps_logbook.activity a
     LEFT JOIN (SELECT ST_Buffer(h.the_geom,0.2) AS harbour_area, h.topiaid FROM common.harbour h WHERE h.locode IS NOT NULL) AS tmp_harbour ON ST_Intersects(tmp_harbour.harbour_area, a.the_geom) = TRUE
WHERE 
    a.topiaid IN (?select_item) 
