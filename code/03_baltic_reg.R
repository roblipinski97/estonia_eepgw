
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
# >>> me if, say, adult by 1989, so born <1972 and parents otherwise?

# > use LG centroid where locality missing or coordinates of the main town or keep NA?

# > include controls for pre-BW protests

# > BW 89 distance separately for respondents and parents?

# > omit Tallinn? (West German TV papers omit Berlin; Gessler et al. (2021) omit Budapest)

# > use TRAVEL distance

# > do placebo with alternative (quickest) Riga-Tallinn route (via Parnu and Marjamaa)

# > robust standard errors?

# > balance table by distance bins -> follows Madestam et al. (2013) -> use 89's census data XXX

# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# PREPARE --------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

estonia = fread(file.path('data', 'clean', 'baltic_way_survey.csv')) 

# ethnic dummy
estonia[, ethnic_estonia := fifelse(ethnic == 'estonian', 'Estonian', 'Other')]
estonia$ethnic_estonia <- relevel(factor(estonia$ethnic_estonia), ref = "Other")

# capital (Tallinn) dummy
estonia[, lg_tallinn := fifelse(locality == 'tallinn', 'Tallinn', 'Non-Tallinn')]
estonia[, bw5_lg_you_tallinn := fifelse(bw5_lg_you == 'tallinn', 'Tallinn', 'Non-Tallinn')]
estonia[, bw5_lg_parent_tallinn := fifelse(bw5_lg_parent == 'tallinn', 'Tallinn', 'Non-Tallinn')]
estonia$lg_tallinn <- relevel(factor(estonia$lg_tallinn), ref = "Non-Tallinn")


# education (broader)
estonia[, edu2 := fcase(str_detect(edu, 'primary|secondary'), 'Secondary or less',
                        str_detect(edu, 'bachelor'), "Bachelor's",
                        str_detect(edu, 'master'), "Master's or PhD")]

table(estonia$edu, estonia$edu2, useNA = 'ifany')

### *checks
table(estonia$yob < 1989) # how many people old enough to possibly participate
table(is.na(estonia$bw5_lg_parent), # pattern of 1989 location NAs - you vs. parents
      is.na(estonia$bw5_lg_you))





# '----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# FIRST-STAGE ------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#

estonia_1989 = estonia %>% filter(yob<=1989)


m1a = lm(bw3_me_dummy_part ~ bw5_dist_lg_minute_you, 
         data = estonia_1989)
summary(m1)

m1b = lm(bw3_me_dummy_part ~ poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn+ 
           bw5_dist_lg_minute_you, 
        data = estonia_1989)

m1c = lm(bw3_me_dummy_part ~ poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn+ 
          county+ 
          bw5_dist_lg_minute_you, 
        data = estonia_1989)


extract_coeftest(m1c,0)


### > overleaf -----------------------------------------------------------------------------------------------

# save for plotting
save(list = ls(pattern = "^m1"), 
     file = file.path(main_dir, 'Tables', paste0( 'first_stage.RData')))


dta_table = reduce(map(mget(ls(pattern = '^m1')), ~ extract_coeftest(., 0)),
                   left_join, 
                   by = "var")


### add controls dummy
dta_table[nrow(dta_table)+1,] = c('Soc-dem. controls', 'No', 'Yes', 'Yes')


### add FE dummy
dta_table[nrow(dta_table)+1,] = c('County FE', 'No', 'No', 'Yes')


### add N
dta_table[nrow(dta_table)+1,] = c('No. obs', map(mget(ls(pattern = '^m1')), ~  formatC(nobs(.), big.mark = ',')))



### add R2
dta_table[nrow(dta_table)+1,] =  c('$R^{2}$', 
                                   reduce(map(mget(ls(pattern = '^m1')), ~ round(summary(.)$r.squared, 3)), data.frame))



### add control mean
dta_table[nrow(dta_table)+1,] =  c('$\\hat{X}_{\\text{BWPart}}$', 
                                   sprintf('%.2f', 
                                           c(rep(mean_miss(estonia_1989$bw3_me_dummy_part), 3)))
)


### re-name
dta_table = dta_table %>% mutate(
  var = recode(var,
               'bw5_dist_lg_minute_you' = 'Distance (minutes)',
               # 'treatPositive' = 'Positive priming',
  )
)

### add size + textbg
dta_table$var = paste0('\\footnotesize{\\textbf{', dta_table$var, '}}')

### replace <NA> with -
dta_table = dta_table %>% mutate(across(everything(), ~if_else(is.na(.), '$-$', .)))


### put all columns in one dataframe column with LaTeX table separators
dta_table$cell1 = paste0(apply(dta_table, 1, paste, collapse = "&"), '\\')

### replace $-$ with \text{-} 
dta_table$cell1 = gsub('$-$', '\\text{-}', dta_table$cell1, fixed=T)


### print to copy to Overleaf  
for(i in 1:nrow(dta_table)){
  cat(paste0(dta_table$cell1[i],'\\', '\n'))
}


### save 
fwrite(dta_table, na = NA, row.names = F,
       file = file.path(main_dir, 'Tables', paste0( 'first_stage.csv')))




# '  ---------------------------------------------------------------------------------------------------------------------------------------------------------
# IV  -------------------------------------------------------------------------------------------------------------------------
names(estonia)[grepl('bw2', names(estonia))]


m2a = ivreg(bw2_ref91_you ~ poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn + county + bw3_me_dummy_part|
                            poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn + county + bw5_dist_lg_minute_you, 
            data = estonia_1989[estonia_1989$yob < 1971,])

summary(m2a)


# m3b = iv_robust(bw1_eu ~ 
#                  ifelse(bw3_any_dummy == 'Participated', 1,0) + gender + age + edu + citizenship + lg_tallinn| 
#                  dist_bw_locality_1989 + gender + age + edu + citizenship + lg_tallinn,
#                data = estonia, cluster = county) 


estonia$pol_interest_demo_best
var1 = 'pol_interest_demo_best'


estonia$letter
for(var1 in names(estonia)[grepl('^ukr', names(estonia))]){
  
  if ("package:tidylog" %in% search()) {detach("package:tidylog", unload = TRUE, character.only = TRUE)}
  
  if(grepl('_multi', var1)){next}
  
  estonia_1989 = estonia_1989 %>% mutate(dv = !!rlang::ensym(var1))
  
  # temp = AER::ivreg(dv ~ bw3_me_dummy_participated + age + gender + ethnic_estonia + lg_tallinn |
  #                     dist_bw_locality_1989 + age + gender + ethnic_estonia + lg_tallinn,
  #                   data = estonia[estonia$yob<1990,])
  # 

  m2a = iv_robust(dv ~ poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn + bw3_me_dummy_part|
                poly(age, 2) + gender + edu2 + ethnic_estonia + bw5_lg_you_tallinn + bw5_dist_lg_minute_you, 
              data = estonia_1989)
  
  print(var1)
  print(extract_coeftest(m2a, 0) )
  
}

hist(estonia$ukr_protest)
hist(estonia$ukr_flag)


### Table 1 Soviet-era political activity -------------------------------------------------------------------------------------------------------------------------------------------



for(var1 in names(estonia)[grepl('^fc', names(estonia))]){
  
  if(grepl('_multi', var1)){next}
  
  estonia = estonia %>% mutate(dv = !!rlang::ensym(var1))
  
  # temp = AER::ivreg(dv ~ bw3_me_dummy_participated + age + gender + ethnic_estonia + lg_tallinn |
  #                     dist_bw_locality_1989 + age + gender + ethnic_estonia + lg_tallinn,
  #                   data = estonia[estonia$yob<1990,])
  # 
  
  
  temp = iv_robust(dv ~ bw3_me_dummy_participated + age + gender + ethnic_estonia + lg_tallinn |
                     dist_bw_locality_1989 + age + gender + ethnic_estonia + lg_tallinn,
                   data  = estonia[estonia$yob<1990,], 
                   cluster = county) 
  
  
  temp2=summary(temp)
  
  print(var1)
  print( (temp2$coefficients[2,]) )
  
}



names(estonia)[grepl('repress_', names(estonia))]

temp = iv_robust(bw2_protest_pre_you ~ bw3_me_dummy_participated + age + gender + ethnic_estonia + lg_tallinn |
                   dist_bw_locality_1989 + age + gender + ethnic_estonia + lg_tallinn,
                 data  = estonia[estonia$yob<1990,], 
                 cluster = county) 

summary(temp)


m1 = lm(bw3_me_dummy_participated ~ repress_, 
        data = estonia)

summary(m1)




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