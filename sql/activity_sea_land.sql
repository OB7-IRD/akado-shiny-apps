--- Recovers information about the location of the activity, whether it is at sea or on land
SELECT 
    a.topiaid::text AS activity_id, 
    o.label1::text AS ocean_name, 
    tmp_geo.ocean::text AS ocean_name_pos, 
    st_asText(a.the_geom)::text AS activity_position,
    ST_SRID(a.the_geom)::numeric AS activity_crs
FROM 
    ps_logbook.activity a
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid 
    LEFT JOIN common.ocean o ON t.ocean = o.topiaid 
    LEFT JOIN (SELECT za.ocean, ST_Union(za.geom) AS geom FROM public.zfao_areas za GROUP BY za.ocean) AS tmp_geo ON ST_Intersects(tmp_geo.geom, a.the_geom) = TRUE 
 WHERE 
    a.topiaid IN (?select_item) 
