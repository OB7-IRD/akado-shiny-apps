--- Recovers information about activities IDs and activity 
SELECT 
    a.topiaid::text AS activity_id,
    a.number::integer AS activity_number,
    r."date"::date AS activity_date,
    a.time::time AS activity_time,
    v.code::text AS vessel_code,
    t.topiaid::text AS trip_id,
    st_asText(a.the_geom)::text AS activity_position,
    ST_SRID(a.the_geom)::numeric AS activity_crs,
    s.code::text AS schooltype_code,
    sss.code::text AS successstatus_code,
    a.totalweight::numeric AS activity_weight,
    va.code::text AS vesselactivity_code,
    o.label1::text AS ocean_name, 
    tmp_geo.ocean::text AS zfao_ocean,
    tmp_harbour.topiaid::text AS harbour_id,
    a.seasurfacetemperature::numeric AS activity_seasurfacetemperature
FROM 
    ps_logbook.activity a 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN ps_logbook.setsuccessstatus sss ON a.setsuccessstatus = sss.topiaid 
    LEFT JOIN ps_common.schooltype s ON a.schooltype = s.topiaid
    LEFT JOIN ps_common.vesselactivity va ON a.vesselactivity = va.topiaid 
    LEFT JOIN common.ocean o ON t.ocean = o.topiaid 
    LEFT JOIN (SELECT za.ocean, ST_Union(za.geom) AS geom FROM custom.zfao za WHERE f_level = 'MAJOR' GROUP BY za.ocean) AS tmp_geo ON ST_Intersects(tmp_geo.geom, a.the_geom) = TRUE 
    LEFT JOIN (SELECT ST_Buffer(h.the_geom,0.2) AS harbour_area, h.topiaid FROM common.harbour h WHERE h.locode IS NOT NULL) AS tmp_harbour ON ST_Intersects(tmp_harbour.harbour_area, a.the_geom) = TRUE
WHERE 
    a.topiaid IN (?select_item) 
