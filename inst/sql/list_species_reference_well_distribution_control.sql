--- Recovers information about list of species used as reference for well distribution control
SELECT 
    s.faocode::text AS species_fao_code 
FROM common.specieslist_species sls 
INNER JOIN common.species s ON sls.species = s.topiaid 
WHERE sls.specieslist IN (?select_item)
