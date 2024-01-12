--- Recovers information about the location of the activity, whether in a harbour, in a ocean
SELECT 
    a.topiaid::text AS activity_id, 
    tmp_harbour.topiaid::text AS harbour_id,
    tmp_geo.ocean::text AS zfao_ocean
FROM 
    ps_logbook.activity a
     LEFT JOIN (SELECT ST_Buffer(h.the_geom,0.2) AS harbour_area, h.topiaid FROM common.harbour h WHERE h.locode IS NOT NULL) AS tmp_harbour ON ST_Intersects(tmp_harbour.harbour_area, a.the_geom) = TRUE
    LEFT JOIN (SELECT za.ocean, ST_Union(za.geom) AS geom FROM custom.zfao za WHERE f_level = 'MAJOR' GROUP BY za.ocean) AS tmp_geo ON ST_Intersects(tmp_geo.geom, a.the_geom) = TRUE 

WHERE 
    a.topiaid IN (?select_item) 
