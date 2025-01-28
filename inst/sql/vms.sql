--- Recovers information about the VMS of the trip
SELECT 
    n.id::text AS vms_id, 
    n."date"::date AS vms_date, 
    n."time"::time AS vms_time,
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
   (tb."NUMBAT" IN (SELECT NULLIF(val, '')::integer FROM UNNEST(ARRAY[ ?select_item_3 ]) AS val) OR CONCAT( ?select_item_3 ) IN ('')) 
