--- Recovers information about the location of the activity, whether in a harbour
SELECT 
    a.topiaid::text AS activity_id, 
    tmp_harbour.topiaid::text AS harbour_id
FROM 
    ps_logbook.activity a
     INNER JOIN (SELECT ST_Buffer(h.the_geom,0.2) AS harbour_area, h.topiaid FROM common.harbour h) AS tmp_harbour ON ST_Intersects(tmp_harbour.harbour_area, a.the_geom) = TRUE
WHERE 
    a.topiaid IN (?select_item) 
