--- retrieve parameter trip for selected data base
SELECT 
    t.topiaid::text AS trip_id,
    v.code::text AS vessel_code,
    v.label1::text AS vessel_label,
    v.status::numeric AS vessel_status,
    v.topiaid::text AS vessel_id,
    t.enddate::date AS trip_enddate,
    t.startdate::date AS trip_startdate,
    o.topiaid::text AS ocean_id,
    o.code::text AS ocean_code,
    o.label1::text AS ocean_label,
    cfla.topiaid ::text AS flagcountry_id,
    cfla.code ::text AS flagcountry_code,
    cfla.label1 ::text AS flagcountry_label,
    cfle.topiaid ::text AS fleetcountry_id,
    cfle.code ::text AS fleetcountry_code,
    cfle.label1 ::text AS fleetcountry_label,
    p.topiaid::text AS logbookprogram_id,
    p.code::text AS logbookprogram_code,
    p.label1::text AS logbookprogram_label
FROM 
    ps_common.trip t
    INNER JOIN common.vessel v ON t.vessel = v.topiaid
    INNER JOIN common.ocean o ON t.ocean = o.topiaid 
    INNER JOIN common.country cfla ON v.flagcountry = cfla.topiaid 
    INNER JOIN common.country cfle ON v.fleetcountry = cfle.topiaid 
    INNER JOIN ps_common."program" p ON t.logbookprogram = p.topiaid 
WHERE 
    coalesce(t.logbookprogram,'NULL') IN (?select_item_1) 
