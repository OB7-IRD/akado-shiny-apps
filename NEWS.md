# AkadoR 2.0.1- 2025.07.18

## Changed
- Sets minimum version package dependencies : codama (>= 1.3.1), DBI (>= 1.2.3), dplyr (>= 1.1.4), DT (>= 0.33), magrittr (>= 2.0.3), plotly (>= 4.10.4), purrr (>= 1.0.2), sf (>= 1.0.19), shinycssloaders (>= 1.1.0), shinydashboard (>= 0.7.2), shinyjs (>= 2.1.0), shinymanager (>= 1.0.410), terra (>= 1.8.21), tibble (>= 3.2.1), tidyr (>= 1.3.1), units (>= 0.8.5), writexl (>= 1.5.1), knitr (>= 1.49), rmarkdown (>= 2.29), spelling (>= 2.3.1)
- Add check in `check_consistency_list` : for elements 'rename_id_column_user' and 'order_id_column_user' of column_user_info 
- observeEvent button_download : Specific output 'table' 

## Remove
- `mod_tab_menu_server` : Remove return reactive 'type_check'

# AkadoR 2.0.0- 2025.07.10

## Added

For display table (Added 3 functions):  
- Add `display_raising_factor` : Function to change table display raising_factor
- Add `display_little_big` : Function to change table display little_big
- Add `display_anapo` : Function to change table display anapo

For plot (Added 5 functions):  
- Add `plot_temporal_limit_data` : Function to create list data/argument for the plot plot_temporal_limit
- Add `plot_position_data` : Function to create list data/argument for the plot plot_position
- Add `plot_eez_data` : Function to create list data/argument for the plot plot_eez
- Add `plot_anapo_data` : Function to create list data/argument for the plot plot_anapo
- Add `plot_anapo_activity_data` : Function to create list data/argument for the plot plot_anapo_activity

New functionality (2) : 
- Add database selection by user and filter database by user type
- Add check selection by user and filter check by user type

## Changed
- Modularization of the application using :
    - lists : check_info, sql_info, tab_info, type_check_info, column_user_info
    - modules : mod_table, mod_tab_content, mod_tab_menu, mod_radiobuttons_type_check

Change check (1) : 
- `check_harbour_inspector` :
    - Change output column order :
        - inversion of harbour_label_departure and harbour_label_landing_trip_previous

Change SQL : 
- `trip` :
    - Split SQL in tow sql `trip` and `previous_trip`
    - Add column  :
        - wellcontentstatus_landing_label
- `trip_selected_startdate_enddat` and `trip_selected_vesselcode_enddate`: 
    - Merging the two sql into a single `trip_selected`
- `vms` :
    - Add column : 
        - vms_id : identifier for VMS with vessel and date in SQL

# AkadoR 1.0.2- 2025.03.31

## Added
- Add `check_category_species_forbidden_well_inspector` : function to check inconsistencies between the weight categories and the species in the well
- Add `coordinate_dd_to_dmd` : function for converting DD (Decimal Degrees) coordinates in DDM (Degrees, Decimal Minutes)

## Changed
- full trip selection : 
    - Add reconstructing full trips between several databases
- `check_activity_sample_inspector` :
    - Add exemple and basic unit testing
- `check_anapo_activity_consistent_inspector` :
    - Add exemple and basic unit testing
- `check_anapo_inspector` :
    - Remove warning check Anapo if dataframe_calcul is empty
    - Standardization names arguments : 
        - minimum_number_vms -> threshold_number_vms 
    - Add exemple and basic unit testing
- `check_distribution_inspector` :
    - Standardization names arguments : 
        - weightcategory_small -> weight_category_small
        - weightcategory_big -> weight_category_big
        - weightcategory_unknown -> weight_category_unknown 
        - species -> species_category_unknown 
    - Add argument : species_category_small_big 
    - Add exemple and basic unit testing
- `check_eez_inspector` :
    - Standardization names arguments : 
        - internationalwaters_code -> international_waters_code
        - vesselactivity -> vessel_activity 
    - Add exemple and basic unit testing
- `check_fishing_context_inspector` :
    - Standardization names arguments : 
        - schooltype_object  -> school_type_object 
    - Add argument : school_type_free; school_type_unknown 
    - Add exemple and basic unit testing
- `check_fishing_time_inspector` :
    - Add exemple and basic unit testing
- `check_harbour_inspector` :
    - Add exemple and basic unit testing
- `check_landing_consistent_inspector` :
    - Add exemple and basic unit testing
- `check_landing_total_weight_inspector` :
    - Add exemple and basic unit testing
- `check_ldlf_inspector` :
    - Add exemple and basic unit testing
- `check_length_class_inspector` :
    - Add exemple and basic unit testing
- `check_little_big_inspector` :
    - Add exemple and basic unit testing
- `check_measure_inspector` :
    - Add exemple and basic unit testing
- `check_operation_inspector` :
    - Add argument : vessel_activity, school_type, success_status_school_type, success_status_weight
    - Add exemple and basic unit testing
- `check_raising_factor_inspector` :
    - Standardization names arguments : 
        - speciesfate -> species_fate 
        - vesselactivity -> vessel_activity 
    - Add exemple and basic unit testing
- `check_position_inspector` :
    - Standardization names arguments : 
        - path_shp_sea -> dataframe3
    - Remove arguments : layer_shp_sea 
    - Add exemple and basic unit testing
- `check_sample_harbour_inspector` :
    - Add exemple and basic unit testing
- `check_sample_without_measure_inspector` :
    - Add exemple and basic unit testing
- `check_sample_without_species_inspector` :
    - Add exemple and basic unit testing
- `check_sea_time_inspector` :
    - Add exemple and basic unit testing
- `check_species_inspector` :
    - Add exemple and basic unit testing
- `check_super_sample_number_consistent_inspector` :
    - Add exemple and basic unit testing
- `check_temperature_inspector` :
    - Add exemple and basic unit testing
- `check_temporal_limit_inspector` :
    - Add exemple and basic unit testing
- `check_time_route_inspector` :
    - Standardization names arguments : 
        - max_seatime -> threshold_sea_time
        - max_fishingtime -> threshold_fishing_time
        - vesselactivity_seatime -> vessel_activity_sea_time
        - objectoperation_seatime -> object_operation_sea_time
        - vesselactivity_fishingtime -> vessel_activity_fishing_time
        - objectoperation_fishingtime -> object_operation_fishing_time
    - Patch if referential contains NA
    - Add exemple and basic unit testing
- `check_trip_activity_inspector` :
    - Add exemple and basic unit testing
- `check_weight_inspector` :
    - Add exemple and basic unit testing
- `check_weight_sample_inspector` :
    - Add exemple and basic unit testing
- `check_weighting_inspector` :
    - Standardization names arguments : 
        - sampletype_code_landing_baitboat -> sample_type_code_landing_baitboat
        - landingtype_baitboat -> landing_type_baitboat 
    - Add exemple and basic unit testing
- `check_weighting_sample_inspector` :
    - Standardization names arguments : 
        - speciesfate -> species_fate 
        - dataframe1 -> dataframe2
        - dataframe2 -> dataframe3
     - Add argument : dataframe1
    - Add exemple and basic unit testing
- `check_well_number_consistent_inspector` :
    - Standardization names arguments : 
        - vesseltype -> vessel_type 
    - Add exemple and basic unit testing
- `data_to_text` : 
     - Remove text format and evaluate
- `referential_file` : 
    - Add referential shape_sea
- Changes position display DD to DDM in app Shiny

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
