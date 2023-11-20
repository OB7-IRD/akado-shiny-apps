--- Recovers information about the school type, success status and catch weight of the activity and the vessel activity
SELECT  
    a.topiaid::text AS activity_id,
    st.code::text AS schooltype_code,
    sss.code::text AS successstatus_code,
    a.totalweight::numeric AS activity_weight ,
    v.code::text AS vesselactivity_code
FROM 
    ps_logbook.activity a 
    LEFT JOIN ps_common.schooltype st ON a.schooltype = st.topiaid
    LEFT JOIN ps_logbook.setsuccessstatus sss ON a.setsuccessstatus = sss.topiaid 
    LEFT JOIN ps_common.vesselactivity v ON a.vesselactivity = v.topiaid 
WHERE 
    a.topiaid IN (?select_item) 
