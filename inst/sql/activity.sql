--- Recovers information about activitie
SELECT 
    a.topiaid::text AS activity_id,
    v.code::text AS vessel_code,
    t.enddate::date AS trip_enddate,
    r."date"::date AS activity_date,
    a.number::integer AS activity_number,
    a.time::time AS activity_time,
    t.topiaid::text AS trip_id,
    st_asText(a.the_geom)::text AS activity_position,
    ST_SRID(a.the_geom)::numeric AS activity_crs,
    a.totalweight::numeric AS activity_weight,
    a.seasurfacetemperature::numeric AS activity_seasurfacetemperature,
    st.code::text AS schooltype_code,
    sss.code::text AS successstatus_code,
    va.code::text AS vesselactivity_code,
    o.label1::text AS ocean_label,
    r.topiaid::text AS route_id
FROM 
    ps_logbook.activity a 
    INNER JOIN ps_logbook.route r ON a.route = r.topiaid 
    INNER JOIN ps_common.trip t ON r.trip = t.topiaid
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    LEFT JOIN ps_common.schooltype st ON a.schooltype = st.topiaid
    LEFT JOIN ps_logbook.setsuccessstatus sss ON a.setsuccessstatus = sss.topiaid 
    LEFT JOIN ps_common.vesselactivity va ON a.vesselactivity = va.topiaid 
    LEFT JOIN common.ocean o ON t.ocean = o.topiaid 
WHERE 
    t.topiaid IN (?select_item) 
