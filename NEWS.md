# AkadoR 1.0.1- 2025.02.06

## Added
* Add Readme
* Ability connect multiple Observe databases

## Changed
* Patch position & anapo : point out bounds crs

# AkadoR 1.0.0- 2025.01.28

For check (Added 32 functions):  
* Added a `check_trip_activity_inspector` function to check inconsistencies between the trip and the presence of associated activity  
* Added a `check_fishing_time_inspector` function to check inconsistencies between the sum of the fishing times indicated for the route and the one indicated for the trip  
* Added a `check_sea_time_inspector` function to check inconsistencies between the sum of the sea times indicated for the route and the one indicated for the trip  
* Added a `check_landing_consistent_inspector` function to check inconsistencies between the total landed weight for canneries and local market in the trip and vessel capacity link to trip  
* Added a `check_landing_total_weight_inspector` function to check inconsistencies between the total weight landed during the trip for the canneries and the sum of the weights of each landing for the canneries linked to the trip  
* Added a `check_temporal_limit_inspector` function to check inconsistencies between trip start and end date and the dates of activity  
* Added a `check_harbour_inspector` function to check inconsistencies between the harbour of landing of the previous trip and the harbour of departure of the current trip  
* Added a `check_raising_factor_inspector` function to check inconsistencies between RF1 and threshold values  
* Added a `check_fishing_context_inspector` function to check inconsistencies between the school type and the association  
* Added a `check_operationt_inspector` function to check inconsistencies between fishing success status, vessel activity, type of school or weight caught  
* Added a `check_position_inspector` function to check inconsistencies between the ocean declared for the trip and the position of the activity  
* Added a `check_weight_inspector` function to check inconsistencies between the sum of the weight indicated for catches and the one indicated for the activity  
* Added a `check_length_class_inspector` function to check inconsistencies between size class of the samples depending on the species and measurement type and the valid threshold  
* Added a `check_measure_inspector` function to check inconsistencies between the total number of individuals measured per sample and the sum of individuals per sample, species and size class  
* Added a `check_temperature_inspector` function to check inconsistencies between activity sea surface temperature and valid threshold  
* Added a `check_weighting_sample_inspector` function to check inconsistencies between the sample weighting and catch weight  
* Added a `check_time_route_inspector` function to check inconsistencies between the fishing times or sea times indicated for the route and activities carried out  
* Added a `check_eez_inspector` function to check inconsistencies between the fishing area declared and calculated for the activity  
* Added a `check_species_inspector` function to check inconsistencies between species sampled and species authorized  
* Added a `check_sample_without_measure_inspector` function to check inconsistencies between the sample and the measurement in terms of presence  
* Added a `check_sample_without_species_inspector` function to check inconsistencies between the sample and the species sampled in terms of presence  
* Added a `check_super_sample_number_consistent_inspector` function to check inconsistencies between the sample and the subsample number  
* Added a `check_well_number_consistent_inspector` function to check inconsistencies between sample well number and associated trip well numbers  
* Added a `check_little_big_inspector` function to check inconsistencies between the percentage of little and big fish sampled  
* Added a `check_weighting_inspector` function to check inconsistencies between the sample weighting and sample weight or landed weight  
* Added a `check_weight_sample_inspector` function to check inconsistencies between the sample weight (m10 and p10) and the global sample weight  
* Added a `check_activity_sample_inspector` function to check inconsistencies between the sample and the activity in terms of presence  
* Added a `check_ldlf_inspector` function to check inconsistencies between the sample measurement types and species or weight values  
* Added a `check_distribution_inspector` function to check inconsistencies between the weights of small and big sample fish and the sum of the small and big weights in the associated well  
* Added a `check_sample_harbour_inspector` function to check inconsistencies between the presence of a sample and the absence of a harbour of landing  
* Added a `check_anapo_inspector` function to check inconsistencies activity position and VMS position  
* Added a `check_anapo_activity_consistent_inspector` function to check inconsistencies between the VMS and the presence of activity  

For shiny (Added 17 functions):   
* Added a `text_error_trip_select_server` function display error message if the trip selection elements are not correctly filled in  
* Added a `config_data_server` function read the .yml file of configuration for the connection  
* Added a `trip_select_server` function retrieves the list of trips and VMS selected by the user  
* Added a `calcul_check_server` function performs all calculations to test for inconsistencies  
* Added a `error_trip_select_serveur` function displays the errors and notifications that occur when you want to start the calculation  
* Added a `table_server` function format table display serveur  
* Added a `window_button_download` function selection window for choosing the type of file to download  
* Added a `table_ui` function format table display ui  
* Added a `table_display_trip` function which formats the trip data for display inconsistency  
* Added a `data_to_text` function create a data.frame in character  
* Added a `data_button_plot` function create the button in the table that will create the plot  
* Added a `column_grounding` function detects activity linked solely to grounding  
* Added a `text_object_more_or_less` function to list elements with a number of occurrences other than 2  
* Added a `set_start_configuration` function set startup configuration  
* Added a `start_configuration` function launch ui  
* Added a `set_server_authentication` function set server authentication  
* Added a `server_authentication` function authentication result for secure connection  

For plot (Added 5 functions):   
* Added a `plot_temporal_limit` function create the plot of the consistency of the dates by trip  
* Added a `plot_position` function create the plot of the consistency of the position for the activity  
* Added a `plot_eez` function create the plot of the consistency of the eez for the activity  
* Added a `plot_anapo` function create the plot of the consistency of the position for the activity and VMS  
* Added a `plot_anapo_activity` function create the plot of the consistency of the position for VMS  
