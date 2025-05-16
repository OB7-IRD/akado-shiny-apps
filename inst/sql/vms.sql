--- Recovers information about the VMS of the trip
SELECT 
    CONCAT(tb."NUMBAT", '_', n."date"::date) AS vms_id,
    n.id::text AS clean_nafpositionmessage_id, 
    n."date"::date AS vms_date, 
    n."time"::text AS vms_time,
    st_asText(n.the_geom)::text AS vms_position,
    ST_SRID(n.the_geom)::numeric AS vms_crs, 
    n.internalreferencenumber::text AS vms_codevessel,
    tb."NUMBAT"::text AS vessel_code,
    tb."TYPE"::text AS vessel_type,
    tb."STATUT"::text AS vessel_statut
FROM 
    public.clean_nafpositionmessage n 
    INNER JOIN public.turbobat tb ON n.internalreferencenumber = tb."CFR_CODE" 
WHERE 
   n."date" >= ?select_item_1 AND 
   n."date" <= ?select_item_2 AND
   (tb."NUMBAT" = NULLIF( ?select_item_3 , '')::integer OR (?select_item_3) IN (''))
