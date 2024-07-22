--- Recovers information about the VMS of the trip
SELECT 
    n."date"::date AS vms_date, 
    n."time"::time AS vms_time,
    st_asText(n.the_geom)::text AS vms_position,
    ST_SRID(n.the_geom)::numeric AS vms_crs, 
    tb."NUMBAT"::text AS vessel_code 
FROM 
    public.clean_nafpositionmessage n 
    INNER JOIN public.turbobat tb ON n.internalreferencenumber = tb."CFR_CODE" 
    INNER JOIN (SELECT unnest(array[ ?select_item_1 ])::integer AS vessel_code, 
                       unnest(array[ ?select_item_2 ])::date AS activity_date) tmp ON tmp.vessel_code = tb."NUMBAT" AND tmp.activity_date = n."date"::date
