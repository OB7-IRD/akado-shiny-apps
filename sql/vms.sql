--- Recovers information about the VMS of the trip
SELECT 
    n."date"::date AS vms_date, 
    n."time"::time AS vms_time,
    st_asText(n.the_geom)::text AS vms_position,
    ST_SRID(n.the_geom)::numeric AS vms_crs, 
    tb."NUMBAT"::text AS vessel_code 
FROM 
    public.nafpositionmessage n 
    INNER JOIN public.turbobat tb ON n.internalreferencenumber = tb."CFR_CODE" 
WHERE 
    CONCAT(tb."NUMBAT", '_',  n."date"::date ) in (?select_item)
