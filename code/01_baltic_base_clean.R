
#
# SET-UP --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

# rm(list=ls())

### Source the '00_global.R' script with required packages and functions
source(file.path(dirname( rstudioapi::getSourceEditorContext()$path), '00_baltic_global.R'))


### Re-set working directory (if necessary)
# main_dir = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
# setwd(main_dir)

### make a copy of the file    ---------------------------------------------------------------------------------------------------------------------------------------------------------------------
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('code', 'code/00_archive', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# NOTES --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

### > use provided lat-long AND/OR IP to add/check geo-location?

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 1. Baseline operations  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estonia_save = fread(file.path('data', 'raw', 'bw_survey_raw_final.csv'))  # read raw .csv file downloaded from Qualtrics


### extract question text  ----------------------------------------------------------------------------------------------------------------------------------------------------------
qs_text = data.frame(
          q_var_raw  = names(clean_names(estonia_save)),
          q_text = as.character(t(estonia_save[1,])))



### dataset-wide cleaning ----------------------------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia_save %>% 
  clean_names() %>% slice(-c(1,2)) %>% # clean column names
  
  setDT %>%  # set as data.table
  
  mutate(id = row_number()) %>% # assign ID as row number
  
  # cleaning across all columns
  mutate(across(where(~ all(grepl("^\\d*\\.?\\d*$", .x[!is.na(.x)]))), as.numeric), # treat numeric columns as such
         across(where(is.character), ~tolower(.)), # all to lowercase
         
         ## empty string + DK + prefer not to say + not alive at the time -> ALL to NAs
         across(everything(), ~replace(.x, str_detect(.x, "^$|don't know|prefer not to|not alive at the time"), NA)) 
         )

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 2. Rename columns ----------------------------------------------------------------------------------------------------------------------------------------------------------

estonia = estonia %>% 
  rename(
    consent = q2b,
    # q_language = q_language,
    
    gender = q3,
    yob    = q4,
    
    county = residence_county_1,
    lg    = residence_locality_1,
    locality = residence_locality_2,
    
    edu    = q7,
    ethnic = q8,
    ethnic_other = q8_6_text,
    
    lang_home_now = q9_1,
    lang_home_past = q9_2,
    lang_home_work = q9_3,
    lang_home_other = q9_4,
    
    satisf_financial = q10,
    satisf_lead_econ = q11_1,
    satisf_lead_eu = q11_2,
    satisf_lead_russia = q11_3,
    satisf_lead_pub_serv = q11_4,
    satisf_lead_ukraine_ref = q11_5,
    satisf_lead_ethnic = q11_6,
    
    pol_interest_understand = q12_1,
    pol_interest_qualified = q12_2,
    pol_interest_good_job = q12_3,
    pol_interest_have_say = q12_4,
    pol_interest_demo_best = q12_5,
    pol_interest_capt_best = q12_6,
    
    lr = q13_1,
    party2023 = q14,
    
    fc_party = formal_civic_1,
    fc_religion = formal_civic_2,
    fc_recreation = formal_civic_3,
    fc_environ = formal_civic_4,
    fc_assoc = formal_civic_5,
    fc_charity = formal_civic_6,
    
    ic_talk_friend = informal_civic_1,
    ic_talk_neighbor = informal_civic_2,
    ic_talk_stranger = informal_civic_3,
    ic_go_public = informal_civic_4,
    ic_go_event = informal_civic_5,
    ic_talk_inperson = informal_civic_6,
    
    child_obedience = children_char_1,
    child_hardwork = children_char_2,
    child_responsible = children_char_3,
    child_imagination = children_char_4,
    child_tolerant = children_char_5,
    child_independent = children_char_6,
    child_determine = children_char_7,
    child_religion = children_char_8,
    child_kind = children_char_9,
    
    repress_ww2 = qe_repressions_1_1,
    repress_deport = qe_repressions_1_2,
    repress_prison = qe_repressions_1_3,
    repress_killed = qe_repressions_1_4,
    repress_torture = qe_repressions_1_5,
    repress_discriminate = qe_repressions_1_6,
    repress_seizure = qe_repressions_1_7,
    repress_religion = qe_repressions_1_8,
    repress_surveillance = qe_repressions_1_9,
    
    repress2_open = qe_repressions_2_1,
    
    pol_past_letter = political_activity_1,
    pol_past_contact = political_activity_2,
    pol_past_protest_elections = political_activity_3,
    pol_past_protest_other = political_activity_4,
    pol_past_ngo = political_activity_5,
    pol_past_charity = political_activity_6,
    
    ukr_comment = qe_future_1_1,
    ukr_contact = qe_future_1_2,
    ukr_protest = qe_future_1_3,
    ukr_donate = qe_future_1_4,
    ukr_house = qe_future_1_5,
    ukr_buy = qe_future_1_6,
    ukr_culture = qe_future_1_7,
    ukr_flag = qe_future_1_8,
    ukr_wear = qe_future_1_9,
    ukr_peacekeeping = qe_future_1_10,
    
    
    bw1_proud = qbw_1_1,
    bw1_trust_gov = qbw_1_2,
    bw1_trust_people = qbw_1_3,
    bw1_leave_past = qbw_1_4,
    bw1_soviet = qbw_1_5,
    bw1_eu = qbw_1_6,
    bw1_ukraine_people = qbw_1_7,
    bw1_ukraine_war = qbw_1_8,
    bw1_ref25 = qbw_1_9,
    
    bw2_communist = qbw_2_1,
    bw2_pop_front = qbw_2_2,
    bw2_forest = qbw_2_3,
    bw2_exit = qbw_2_4,
    bw2_radio = qbw_2_5,
    bw2_finnish_tv = qbw_2_6,
    bw2_russian_tv = qbw_2_7,
    bw2_protest_pre = qbw_2_8,
    bw2_protest_bw = qbw_2_9,
    bw2_ref91 = qbw_2_10,
    
    bw3_me = qbw_3_1,
    bw3_father = qbw_3_2,
    bw3_mother = qbw_3_3,
    
    bw4_pos_neg = qbw_4_1,
    bw4_personal = qbw_4_2,
    bw4_discuss = qbw_4_3,
    bw4_unity = qbw_4_4,
    bw4_commemorate = qbw_4_5,
    
    bw5_lg_you = qbw_5a_1,
    bw5_locality_you = qbw_5a_2,
    bw5_lg_parent = qbw_5b_1,
    bw5_locality_parent = qbw_5b_2,
    
    party_th_isamaa = q12_1_2, # Fatherland, conservative, currently leading the pools
    party_th_kesk = q12_2_2, # Centre Part, 3rd in 2023 elections with 15%
    party_th_ekre = q12_3_2, # Conservative People's Party of Estonia - nationalist, right-wing, 2nd in 2023 elections with 16%
    party_th_sde = q12_4_2, # Social Democratic Party; founded 1990 as ESDP under Marju Lauristin, 
    party_th_parem = q12_5_2, # the Right, economic liberals, currently no MPs (founded 2022)
    party_th_eer = q12_6_2, # Greens
    party_th_ee200 = q12_7, # current coalition partner
    party_th_euvp = q12_8, # United Left, pro-EU and progressive
    party_th_reform = q12_9, # Reform, current ruling party, 31% in 2023 elections
    
    ngo_policy1 = outcomes_1_1,
    ngo_policy2 = outcomes_1_2,
    ngo_policy3 = outcomes_1_3,
    ngo_policy4 = outcomes_1_4,
    ngo_policy5 = outcomes_1_5,
    
    ngo_part1 = outcomes_2_1,
    ngo_part2 = outcomes_2_2,
    ngo_part3 = outcomes_2_3,
    ngo_part4 = outcomes_2_4,
    
    donation = donation_1,
    
    ngo_real1 = realistic_outcomes_1,
    ngo_real2 = realistic_outcomes_2,
    ngo_real3 = realistic_outcomes_3,
    ngo_real4 = realistic_outcomes_4,
    ngo_real5 = realistic_outcomes_5,
    
    # treatment_is = treatment_is,
    # treatment_fc = treatment_fc
    
  )

### add cleaned variable names to the df with question text  ----------------------------------------------------------------------------------------------------------------------------------------------------------
qs_text = cbind(names(estonia), qs_text) %>% rename(q_var = 1)

write_flex(qs_text, file.path('data', 'clean', 'qs_text.csv'), format = format1)


### filter consent and attention check  ----------------------------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia %>% 
  filter(consent == 'yes') %>% 
  filter(attn_check1 == 'red') %>% 
  filter(attn_check2 == 'red') 

### + age variable  ----------------------------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia %>% mutate(age = 2025-as.numeric(yob))



# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 3. Recode Likert to numeric ------------------------------------------------------------------------------------------------------------------------

likert_agree <- c(
  "strongly disagree" = 1,
  "somewhat disagree" = 2,
  "somewhat agree" = 3,
  "strongly agree" = 4)

likert_import<- c(
  "very important" = 1,
  "somewhat important" = 2,
  "not very important" = 3,
  "not at all important" = 4)


likert_satisf <- c(
  "very satisfied" = 1,
  "somewhat satisfied" = 2,
  "somewhat dissatisfied" = 3,
  "very satisfied" = 4)


likert_likely <- c(
  "highly likely" = 1,
  "somewhat likely" = 2,
  "somewhat unlikely" = 3,
  "very unlikely" = 4)

likert_satisf5 <- c(
  "very satisfied" = 1,
  "somewhat satisfied" = 2,
  "neither satisfactory nor unsatisfactory" = 3,
  "somewhat dissatisfied" = 4,
  "very satisfied" = 5)

likert_support <- c(
  "fully support" = 1,
  "support" = 2,
  "neither support nor do not support" = 3,
  "do not support" = 4,
  "fully do not support" = 5)

likert_willing <- c(
  "fully willing" = 1,
  "somewhat willing" = 2,
  "neither willing nor unwilling" = 3,
  "unwilling" = 4,
  "fully unwilling" = 5)

likert_realistic <- c(
  "highly realistic" = 1,
  "realistic" = 2,
  "neither realistic nor unrealistic" = 3,
  "unrealistic" = 4,
  "highly unrealistic" = 5)



estonia <- estonia %>%
  mutate(
    across(matches('^pol_interest|^bw1|^bw4'),  ~recode(.x, !!!likert_agree)),
    across(matches('^child'),  ~recode(.x, !!!likert_import)),
    across(matches('^ukr'),  ~recode(.x, !!!likert_likely)),
    across(matches('^satisf_financial'),  ~recode(.x, !!!likert_satisf5)),
    across(matches('^satisf'),  ~recode(.x, !!!likert_satisf)),
    across(matches('^ngo_policy'),  ~recode(.x, !!!likert_support)),
    across(matches('^ngo_part'),  ~recode(.x, !!!likert_willing)),
    across(matches('^ngo_real5'),  ~recode(.x, !!!likert_realistic)),
  )


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 4. Dummies -------------------------------------------------------------------------------------------------------------------------------------------------


make_dummies_keep_multi <- function(data, id_col, multi_col) {
  
  id_col <- rlang::ensym(id_col)
  multi_col <- rlang::ensym(multi_col)
  multi_col_name <- rlang::as_name(multi_col)
  multi_col_multi <- paste0(multi_col_name, "_multi")
  
  temp = data %>%
    select(!!id_col, !!multi_col) %>%
    mutate(!!multi_col_multi := !!multi_col) %>%  # keep original as new col
    separate_rows(!!multi_col, sep = ",") %>%
    dummy_cols(select_columns = multi_col_name,
               remove_selected_columns = TRUE) %>%
    clean_names() %>%
    group_by(!!id_col, !!rlang::sym(multi_col_multi)) %>%
    summarise(across(everything(), max), .groups = "drop")
  
  data = left_join(data %>% select(-c(multi_col)), temp)
  
  return(data)
}

for(var1 in names(estonia)[grepl('^lang_home|^pol_past|^repress_|^bw2|^bw3', names(estonia))]){
  estonia  = make_dummies_keep_multi(estonia, id, !!rlang::ensym(var1))
  # print(dim(estonia))
}




# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 5. Geo-variables ------------------------------------------------------------------------------------------------------------------------------------

### LG boundaries  ------------------------------------------------------------------------------------------------------------------------------------
estonia_lg_sf = read_sf(file.path(main_dir, 'data', 'spatial', 'estonia_lg_2023.shp')) %>% clean_names() %>% 
  rename(county = mnimi, lg = onimi) %>% 
  select(c(county,lg)) %>% 
  mutate(across(c(county, lg), ~tolower(.)))


# mapa(estonia_county_sf, 'county')

##### + population -----------------------------------------------------------------------------------------------------------------------------------------------
population <- fread(file.path(main_dir, 'data', 'raw', 'population_2016_2023.csv')) %>% 
  filter(year == 2023) %>% 
  select(lg, population) %>% 
  mutate(across(c(lg), ~tolower(.)))

estonia_lg_sf = left_join(estonia_lg_sf, population) %>% st_as_sf()

#### + county union ------------------------------------------------------------------------------------------------------------------------------------------
estonia_county_sf <- estonia_lg_sf %>%
  group_by(county) %>%  
  summarise(geometry = st_union(geometry)) %>%
  ungroup()


### locality (from post codes)  ------------------------------------------------------------------------------------------------------------------------------------
if(!exists('estonia_postal_all')){ # takes a while to read the full Excel, so only do so if one doesn't exist yet
  estonia_postal_all = read.xlsx(file.path(main_dir, 'data', 'raw', 'postal_codes.xlsx')) %>% clean_names()
}


estonia_postal = estonia_postal_all  %>% 
  select(4:6, 11:12) %>% distinct %>% 
  rename(county = 1, lg = 2, settlement = 3, lat = coordinate_x, lon = coordinate_y) %>%
  
  mutate(# if city, then it's good enough and proceed 
         settlement = ifelse(grepl('linn', lg), lg, settlement),
          
         # clean incorrectly read diacritics...
         across(where(is.character), ~gsub( "Ã¼", "ü",   .)),
         across(where(is.character), ~gsub( "Ã¤", "ä",  .)),
         across(where(is.character), ~gsub( "Ãµ", "õ",   .)),
         across(where(is.character), ~gsub( "Ã¶", "ö",   .)),
         across(where(is.character), ~gsub( "Ã¥", "å",   .)), # very rare (few instances) but still
         
         across(where(is.character), ~gsub( "Ãœ", "Ü",   .)),
         across(where(is.character), ~gsub( "Ã„", "Ä",   .)),
         across(where(is.character), ~gsub( "Ã•", "Õ",   .)),
         across(where(is.character), ~gsub( "Ã–", "Ö",   .)),
         
         across(everything(), ~trimws(.)), # remove empty spaces
         across(where(is.character), ~tolower(.)), # character to lowercase
         across(c(lat, lon), ~as.numeric(.)) # coordinates as numeric
        
  ) %>% 
  # coordinates are there per postal code - average them for at settlement level (take median as with mean 1 mislabelled post
  # code would skew the result?? -> doesn't really make a difference, as non-linn settlements have a single unique coordinate)
  fgroup_by(county, lg, settlement) %>% 
  fsummarise(lat = fmedian(lat), lon = fmedian(lon)
            # lat_n = fdistinct(lat), lon_n = fdistinct(lon)
            )




### BW pathway -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### read pathway coordinates
baltic_way = jsonlite::fromJSON(file.path(main_dir, 'data', 'spatial', 'Baltic_way.json'), flatten=TRUE)
# baltic_way$jsondata$data$features$geometry.coordinates

baltic_way <- st_as_sf(x = data.frame(baltic_way$jsondata$data$features$geometry.coordinates),
                       coords = c("X1", "X2"),
                       crs = 4326)

### treat as line
baltic_way <- st_as_sf(baltic_way) %>% 
  summarise(geometry = st_combine(geometry)) %>% 
  st_cast("LINESTRING") 

### ensure common projection
baltic_way <- st_transform(baltic_way, 4326) # 3301 - should be in metres; 4326 - should be in degrees
estonia_lg_sf <- st_transform(estonia_lg_sf, 4326)

st_crs(baltic_way) = 4326
st_crs(estonia_lg_sf) = 4326

baltic_way  = st_make_valid(baltic_way)
estonia_lg_sf  = st_make_valid(estonia_lg_sf)

### leave only Estonian segment of the BW line
baltic_way <- st_intersection(baltic_way, estonia_lg_sf)



### get distance from the BW line to...
## LG centroids
dist_matrix <- st_distance(estonia_lg_sf %>% st_centroid(), baltic_way)
estonia_lg_sf$dist_bw_lg = apply(dist_matrix, 1, min)/1e3

### *checks - visualize the pathway and the distance
ggplot()+
  geom_sf(data = estonia_lg_sf, aes(fill = dist_bw_lg))+
  # geom_sf(data = baltic_way)+
  geom_sf(data = baltic_way, color = 'red', linewidth = 1.8) + 
  map_theme


## Individual localities
estonia_postal <- st_as_sf(estonia_postal, coords = c("lat", "lon"), crs = 3301)
estonia_postal <- st_transform(estonia_postal, 4326)

dist_matrix <- st_distance(estonia_postal, baltic_way)
estonia_postal$dist_bw_locality = apply(dist_matrix, 1, min)/1e3

### *checks - visualize distance
mapa(estonia_lg_sf, 'dist_bw_lg')

ggplot()+
  geom_sf(data = estonia_postal, aes(fill = dist_bw_locality),
          shape = 21) + 
  geom_sf(data = baltic_way, color = 'red', linewidth = 1.8) + 
  map_theme




### BW_locality - any -------------------------------------------------------------------------------------------------------------------------------------------

### create a unified location for the 1989 (Baltwic Way) residence - it's either you or your parents - never both (but possibly both missing)
# table(is.na(estonia$bw5_lg_parent), is.na(estonia$bw5_lg_you))

sf(is.na(estonia_lg_sf$lg))

sf(funique(estonia$lg) %in% estonia_lg_sf$lg)
funique(estonia$lg)[!funique(estonia$lg) %in% estonia_lg_sf$lg]

estonia[, bw5_lg_any := fifelse(is.na(bw5_lg_you), bw5_lg_parent, bw5_lg_you)]
estonia[, bw5_locality_any := fifelse(is.na(bw5_locality_you), bw5_locality_parent, bw5_locality_you)]

### *checks -> how many unique localities in 2025 and 1989
fdistinct(estonia$locality) # 295
fdistinct(estonia$bw5_locality_any) # 264



estonia_temp = estonia


### <> add to survey ----------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia_temp

### treat NAs as string ('missing') for easier merging
estonia = estonia %>% mutate(across(c(county, lg, locality,
                                      bw5_lg_any, bw5_locality_any), ~ifelse(is.na(.), 'missing', .)))

#### LG ---------------------------------------------------------------------------------------------------------------------------------------
estonia = left_join(estonia,
                    estonia_lg_sf %>% st_drop_geometry() %>% select(-c(county, population)) %>% 
                      rename(dist_bw_lg_current = dist_bw_lg))


estonia = left_join(estonia, 
                    estonia_lg_sf %>% st_drop_geometry() %>% select(-c(county, population)) %>% 
                      rename(bw5_lg_any = lg, dist_bw_lg_1989 = dist_bw_lg))


table(is.na(estonia$dist_bw_lg_current), is.na(estonia$lg))

### * checks - both equations should give the same distribution
sf(estonia$lg == 'missing') # 13
sf(is.na(estonia$dist_bw_lg_current)) # 13

sf(estonia$bw5_lg_any == 'missing') # 146
sf(is.na(estonia$dist_bw_lg_1989)) # 146

#### Locality ---------------------------------------------------------------------------------------------------------------------------------------

### treat 'estonia_postal' as data.frame with lat-lon as numeric variables
estonia_postal = estonia_postal %>% 
  mutate( lat = st_coordinates(geometry)[, 1],
          lon = st_coordinates(geometry)[, 2]) %>% 
  st_drop_geometry()

estonia = left_join(
  estonia,
  estonia_postal %>% st_drop_geometry() %>% select(-c(county)) %>%
    rename(locality = settlement, lat_current = lat, lon_current = lon, dist_bw_locality_current = dist_bw_locality)
  )

estonia = left_join(
  estonia,
  estonia_postal %>% select(-c(county)) %>% 
    rename(bw5_lg_any = lg, bw5_locality_any = settlement, lat_1989 = lat, lon_1989 = lon, dist_bw_locality_1989 = dist_bw_locality)
  )


### * checks - both equations should give the same distribution
sf(estonia$locality == 'missing') #77
sf(is.na(estonia$dist_bw_locality_current)) # 77

sf(estonia$bw5_locality_any == 'missing') # 267
sf(is.na(estonia$dist_bw_locality_1989)) # 267


plot(estonia$lat_1989,
     estonia$lon_1989)

plot(estonia$lat_current,
     estonia$lon_current)



### replace NA locality with LG centroids -------------------------------------------------------------------------------------------------------------------------------------
estonia = estonia %>% 
  mutate(
    dist_bw_locality_current_2 = ifelse(is.na(dist_bw_locality_current), dist_bw_lg_current, dist_bw_locality_current),
    dist_bw_locality_1989_2 = ifelse(is.na(dist_bw_locality_1989), dist_bw_lg_1989, dist_bw_locality_1989),
  )

sf(is.na(estonia$dist_bw_locality_current))
sf(is.na(estonia$dist_bw_locality_current_2))

sf(is.na(estonia$dist_bw_locality_1989))
sf(is.na(estonia$dist_bw_locality_1989_2))





### revert from 'missing' to NAs
estonia = estonia %>% mutate(across(c(county, lg, locality,
                                      bw5_lg_any, bw5_locality_any), 
                                    ~ifelse(. == 'missing', NA, .)))


# ' --------------------------------------------------------------------------------------------------------------------------------------------------------------
# 6. SAVE ----------------------------------------------------------------------------------------------------------------------------------------------------------------------
# 
write_flex(estonia, file.path('data', 'clean', 'baltic_way_survey.csv'), format = format1)



### (*)checks -------------------------------------------------------------------------------------------------------------------------------------------------
### -> basic correlations with Finnish TV 
sf(estonia$bw2_finnish_tv)
pr_na(estonia$bw2_finnish_tv)

prop.table(table( estonia$bw1_soviet,  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
prop.table(table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'), 2)
(table( is.na(estonia$bw2_ref91),  is.na(estonia$bw2_finnish_tv), useNA='ifany'))


table(grepl("nobody|prefer|don't know|^$", estonia$bw2_ref91), 
      grepl("nobody|prefer|don't know|^$", estonia$bw2_finnish_tv), useNA = 'ifany')


### duration
summary(estonia$duration_in_seconds/60)
sf(estonia$duration_in_seconds > 60 & estonia$duration_in_seconds < 15*60)
hist(estonia$duration_in_seconds/60)



### lat-lon
plot(estonia$location_latitude[estonia$location_latitude >55 & estonia$location_latitude < 65],
     estonia$location_longitude[estonia$location_latitude >55 & estonia$location_latitude < 65])


# ' --------------------------------------------------------------------------------------------------------------------------------------------
# **SCRAPBOOK -----------------------------------------------------------------------------------------------------------------------------------
#


### read questionnaire? -------------------------------------------------------------------------------------------------
# library(jsonlite)
# temp = fromJSON(file.path('data', 'qualtrics_questionnaire.qsf'), flatten = TRUE)$SurveyElements %>% clean_names()
# dim(temp)
# names(temp)
# 
# temp$element %>% sf
# temp2 = temp %>%  filter(element == "SQ")
# 
# temp2$payload[36]%>% unlist
# 
# survey_qs =  temp %>%  filter(element == "SQ") %>% pull(secondary_attribute)


# ' --------------------------------------------------------------------------------------------------------------------------------------------
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#