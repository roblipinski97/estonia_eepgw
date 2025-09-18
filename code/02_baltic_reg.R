
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

### Coding decisions to be made:
# > using me and/or parents 
# >>> me if, say, adult by 1989, so born <1972 and parents otherwise

# 

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# PREPARE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

estonia = fread(file.path('data', 'clean', 'baltic_way_survey.csv')) 

# ethnic dummy
estonia[, ethnic_estonia := fifelse(ethnic == 'estonian', 'Estonian', 'Other')]


# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# BALTIC WAY -------------------------------------------------------------------------------------------------------------------------------------------------------------
#


### recode BW participation (bw3) to direct/indirect/none
estonia = estonia %>% mutate(
  bw3_me_dummy = case_when(
    str_detect(bw3_me_multi, 'took part in it by being part|witnessed first-hand') ~ 'Participated',
    str_detect(bw3_me_multi, '^watched|only watched') ~ 'Watched',
    str_detect(bw3_me_multi, 'was not involved') ~ 'Not involved',
    .default = NA),
  bw3_any_parent_dummy = case_when(
    str_detect(bw3_father_multi, 'took part in it by being part|witnessed first-hand') |
      str_detect(bw3_mother_multi, 'took part in it by being part|witnessed first-hand') ~ 'Participated',
    str_detect(bw3_father_multi, '^watched|only watched') |
      str_detect(bw3_mother_multi, '^watched|only watched') ~ 'Watched',
    str_detect(bw3_father_multi, 'was not involved') | 
      str_detect(bw3_mother_multi, 'was not involved')~ 'Not involved',
    .default = NA),
  bw3_any_dummy  = case_when(
    bw3_me_dummy == "Participated" | bw3_any_parent_dummy == "Participated" ~ "Participated",
    bw3_me_dummy == "Watched"      | bw3_any_parent_dummy == "Watched"      ~ "Watched",
    bw3_me_dummy == "Not involved" | bw3_any_parent_dummy == "Not involved" ~ "Not involved",
    .default = NA)
)

### *checks > does the re-coding work for 'bw3_any_dummy'?
table(
  estonia$bw3_me_dumm,
  estonia$bw3_any_parent_dummy,
  estonia$bw3_any_dummy,
  useNA = 'ifany')

### XXX inconsistency - >50% of people say 'I took part in BW' didn't select 'you' when asked about participation
### in the previous (bw2_protest_bw_you) question
names(estonia)[grepl('bw2', names(estonia))]

table(
  estonia$bw3_me_multi,
  estonia$bw2_protest_bw_you,
  useNA = 'ifany')

### NOTE: quite strong correlation between father and mother participation - either both did something or both didn't
table(
  estonia$bw3_father_was_not_involved_in_any_of_those_ways,
  estonia$bw3_mother_was_not_involved_in_any_of_those_ways,
  useNA = 'ifany')


### first-stage -------------------------------------------------------------------------------------------------------------------------
# prob of participation (you) by distance
m1a = glm(bw3_me_dummy != 'Not involved' ~ gender + age + edu + citizenship + locality_tallinn +
           dist_bw_locality_1989,
         family = binomial(link = 'logit'),
         data = estonia[estonia$yob < 1985,])

summary(m1a)


# prob of participation (any parent) by distance
m1b = glm(bw3_any_parent_dummy != 'Not involved' ~ gender + age + edu + citizenship + locality_tallinn +
            dist_bw_locality_1989,
          family = binomial(link = 'logit'),
          data = estonia[estonia$yob < 1985,])

summary(m1b)



m1c = lm(bw3_any_dummy == 'Participated' ~ gender + age + edu + citizenship + locality_tallinn +
            dist_bw_locality_1989,
          data = estonia[estonia$yob < 1985,])


m1c = lm(bw3_any_dummy == 'Participated' ~ 
           dist_bw_locality_1989,
         data = estonia[estonia$yob < 1985,])

summary(m1c)


### second-stage -------------------------------------------------------------------------------------------------------------------------
names(estonia)[grepl('bw2', names(estonia))]

m2 = lm(bw2_ref91_you ~ gender + age + edu + citizenship + locality_tallinn + 
          bw3_any_dummy,
        data = estonia) 
summary(m2)


## IV ---------------------------------------------------------------------------------------------------------------------------------------------------------
m3a = ivreg(bw1_eu ~ 
                 ifelse(bw3_any_dummy == 'Participated', 1,0)|
                 dist_bw_locality_1989 ,
               data = estonia) 

summary(m3a, diagnostics = T)



m3b = iv_robust(bw1_eu ~ 
                 ifelse(bw3_any_dummy == 'Participated', 1,0) + gender + age + edu + citizenship + locality_tallinn| 
                 dist_bw_locality_1989 + gender + age + edu + citizenship + locality_tallinn,
               data = estonia, cluster = county) 

summary(m3b)

estonia[, locality_tallinn := fifelse(locality == 'tallinn', 'Tallinn', 'Non-Tallinn')]

pr_na(estonia$bw3_any_dummy)

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# FINNISH TV -------------------------------------------------------------------------------------------------------------------------------------------------------------
#

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# *REPRESSIONS -------------------------------------------------------------------------------------------------------------------------------------------------------------
#

# treatment 1 = repressions > priming > future
# treatment 2 = repressions > future
# treatment 3 = future > repressions
estonia$treatment_is %>% sf
ukr_comment

for(var1 in names(estonia)[grepl('ukr_', names(estonia))]){
  
  formula1 <- as.formula( paste(var1, "~ gender + age + edu + treatment_is"))
  m1 = lm(formula1, data = estonia)
  
  print(var1)
  print(extract_coeftest(m1, 1))
}




# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# *SCRAPBOOK -------------------------------------------------------------------------------------------------------------------------------------------------------------
#

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ACTA EST FABULA -------------------------------------------------------------------------------------------------------------------------
#