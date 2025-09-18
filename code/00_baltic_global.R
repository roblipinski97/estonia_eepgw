
### Author: Robert Lipiński
### Date created: 17/10/2024

#
# SET-UP -----------------------------------------------------------------------------------------------
#

# rm(list=ls())

Sys.setlocale("LC_ALL", "Russian_Russia.UTF-8")


### make a copy of the file    -----------------------------------------------------------------------------------------------
file.copy(rstudioapi::getSourceEditorContext()$path,
          gsub('code', 'code/00_archive', gsub('\\.R', ' - copy.R', rstudioapi::getSourceEditorContext()$path)),
          overwrite = T, copy.date = T)


### set path --------------------------------------------------------------------------------------------------------------------------------------------------------
# System <- Sys.getenv(x = NULL, unset = "")

main_dir = dirname(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(main_dir)

# '  --------------------------------------------------------------------------------------------------------------------------------------------------------
# PACKAGES --------------------------------------------------------------------------------------------------------------------------------------------------------
#

### Load all required packages (pacman installs the ones still not present)

if( !is.element("pacman", installed.packages() )){
  install.packages("pacman", dep= T)
}

pacman::p_load(tidyverse, haven, stringr,
               janitor, data.table, ggplot2, stringi, dplyr,
               foreign, labelled, fastDummies, car, arrow,
               lubridate, scales, purrr, openxlsx, readxl,
               
               stargazer,  ggpubr, paletteer, grid,
               gridExtra, patchwork,cowplot,
               tidylog, hunspell, future, ids,
               knitr, showtext, sysfonts,
               ggtext, psych, tm, ggridges, 
               tableone, zoo, rlang, stringfish, microbenchmark,
               gender, genero, tm, stringdist, fuzzyjoin, beepr,
               collapse, pryr, fst, tools, sf,
               update = F)


### Load fonts --------------------------------------------------------------------------------------------------------------------------------------------------------
# loadfonts(quiet = T)
# fonts()

### Source: https://brailleinstitute.org/freefont
# Add the OpenType font to the system (replace with your actual font name and path)
# font_add("atkinson_regular", "~/Moje/Graphs/00_other/atkinson_regular.otf")
# font_add("atkinson_bold", "~/Moje/Graphs/00_other/atkinson_bold.otf")
# 
# # Enable showtext to use the added font
# showtext_auto()

temp = data.frame(x = sample(1:100, 100), y = sample(1:100, 100))


### Make sure common functions are taken from the right packages
select <- dplyr::select
recode <- dplyr::recode
# filter <- dplyr::filter # disables my tidylog!

# ' --------------------------------------------------------------------------------------------------------------------------------------------------------
# SETTINGS ------------------------------------------------------------------------------------------
#

# Number of columns shown in Viewer()
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)


# ' --------------------------------------------------------------------------------------------------------------------------------------------------------
# KEY GLOBAL PARAMETERS  --------------------------------------------------------------------------------------------------
#

### baseline file format
format1 = 'csv'

### turn off scientific notation
options(scipen = 999)

 
# ' --------------------------------------------------------------------------------------------------------------------------------------------------------
# CUSTOM FUNCTIONS ---------------------------------------------------------------------------------------------------------------------------------------------------------
#


### custom summaries -----------------------------------------------------------------------------------------------------------------------------------------------------
### Create new functions that will be handy to use throughout the scripts

mean_miss <- function(x){mean(x, na.rm = TRUE)}
median_miss <- function(x){median(x, na.rm = TRUE)}
sum_miss  <- function(x){sum(x, na.rm = TRUE)}
sd_miss   <- function(x){sd(x, na.rm = TRUE)}
lower_ci  <- function(x){mean_miss(x) - 1.96 * (sd_miss(x) / sqrt(length(x)))}
upper_ci  <- function(x){mean_miss(x) + 1.96 * (sd_miss(x) / sqrt(length(x)))}
min_miss  <- function(x){min(x, na.rm = TRUE)}
max_miss  <- function(x){max(x, na.rm = TRUE)}
mode_miss <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

sf        <- function(x){return(summary(factor(x)))}
pr        <- function(x){return(prop.table(table(x, useNA = "no"))*100)}
table_na  <- function(x){return(table(x, useNA = "ifany"))}
pr_na     <- function(x){return(prop.table(table(x, useNA = "ifany"))*100)}
pr_isna   <- function(x){return(pr(is.na(x)))}
table_s   <- function(x, s){return(sort(table(x), decreasing = s))}

# return top N values with % in descending order
pr_top = function(dta, var, n1 = 10){
  return(
    base::print(
      dta %>%
        ungroup %>% 
        select(-any_of(c('n'))) %>% 
        count(!!sym(var), sort = TRUE) %>%  # count each category and sort descending
        mutate(percentage = 100 * n / sum(n)) %>%  # calculate percentage
        slice_head(n=n1))
  )
}
# table_s   <- function(x, s = F){return(prettyNum(sort(table(x, useNA = 'ifany'), decreasing = s), big.mark = ','))} # with big.mark -> gives characters rather than numbers
pr_s   <- function(x, s = F){return(prop.table(sort(table(x, useNA = 'ifany'), decreasing = s)))} # sorted proportional table (in %)


fdistinct <- function(x){return(length(funique(x)))}

str_wrap_br = function(x, n){ # wrap string with <br> instead of \n (mainly for ggtext labels in ggplot)
  x = gsub('\n', '<br>', str_wrap(x, n), fixed=T)
  return(x)
}

name_check <- function(x, text1){names(x)[grepl(text1, names(x))]}

see <- function(x, n){return(x[sample(1:nrow(x), n),])}


### longer helpers --------------------------------------------------------------------------------------------------------------------------------------------------------------------------


read_flex = function(file, format  = format1, sheet = 1,  path = getwd()){
  
  # determine file extension
  if(file_ext(file) != ''){
    file_full = file  # use file extension if one present in file name
    format = file_ext(file)
  }else{
    file_full = paste0(file, '.', format) # else add format defined in the function 
  }
  
  # search for file recursively
  files <- list.files(path = path, pattern = paste0("^", file, '.', format),
                      recursive = TRUE, full.names = TRUE)
  
  if( length(files) == 0 ) {stop("File not found: ", filename) }
  if( length(files) == 1 ) {file_full = files[1]}
  if( length(files) > 1 ) {
    warning("Multiple files found. Using the first one: ", files[1])
    file_full = files[1]
  }
  

  
  if(format == 'csv'){dta = fread(file_full, encoding = 'UTF-8')}
  if(format == 'xlsx'){dta = openxlsx2::read_xlsx(file_full, sheet)}
  if(format == 'parquet'){dta = arrow::read_parquet(file_full)}
  if(format == 'rds'){dta = readRDS(file_full) }
  if(format == 'qs'){dta = qs::qread(file_full, nthreads = nthreads1, use_alt_rep=F) }
  
  return(dta)
}



write_flex = function(x, file, format = format1){
  
  if(file_ext(file) != ''){
    file_full = file  # use file extension if one present in file name
    format = file_ext(file)
  }else{
    file_full = paste0(file, '.', format) # else add format defined in the function 
  }
  
  if(format == 'csv'){dta = fwrite(x, file_full, row.names = F, na = NA, encoding = 'UTF-8')}
  if(format == 'xlsx'){dta = openxlsx::write.xlsx(x, file_full)}
  if(format == 'parquet'){dta = arrow::write_parquet(x, file_full)}
  if(format == 'rds'){dta = saveRDS(x, file_full) }
  if(format == 'qs'){dta = qs::qsave(x, file_full, nthreads = nthreads1) }
  
}




x = c(1.01, .3412)
round_flex = function(x){
  if(abs(x) < 1){
    x = sprintf("%.3f",round(x, 3))
  }else if(abs(x) >= 1  & abs(x) < 10){
    x = sprintf("%.2f",round(x, 2))
  }else if(abs(x) >= 10 & abs(x) < 100){
    x = sprintf("%.1f",round(x, 1))
  }else if(abs(x) >= 100){
    x= sprintf("%.0f",round(x, 0))
  }
  
  return(x)
}



sig_stars = function(var){
  
  var = ifelse(var == '<0.001', round(0.00, 2), round(as.numeric(var), 3))
  
  if(is.numeric(var)){
    var = ifelse(test = var < 0.10 & var >= 0.05, yes  = paste0(var, '$^{\\dotr}$'), no = var)
    var = ifelse(test = var < 0.05  & var >= 0.01, yes  = paste0(var, '$^{*}$'), no = var)
    var = ifelse(test = var < 0.01  & var >= 0.001, yes  = paste0(var, '$^{**}$'), no = var)
    var = ifelse(test = var < 0.001, yes  = paste0(var, '$^{***}$'), no = var)  
  }
  
  return(var)
}

# Function to summarize all columns in the dataset
sf_dataset = function(data){
  for(var1 in names(data)){
    print(var1)
    data = data %>% mutate(value = !!rlang::ensym(var1))
    print(sf(data$value))
  }
}


# Function to find observations above X st. dev. from the mean
outlier_sd = function(data, var, x){
  lower = mean_miss(data[,var]) - x*sd_miss(data[, var])
  upper = mean_miss(data[,var]) + x*sd_miss(data[, var])
  return(which(data[,var] < lower | 
                 data[,var] > upper))
}
#outlier_sd(dta, 'covid_vaccine', 3)




# Extract coefficients from the model into a (semi-)clean LaTeX row
extract_coeftest = function(m1, length1){
  
  ### Extract coefficients
  
  # If OLS
  if(class(m1) %in% c('lm')){
    temp = summary(m1)$coefficients
    temp = data.frame(beta = temp[,1], se = temp[,2], p_value = temp[,4])
  }
  if(class(m1) %in% c('coeftest')){
    temp = temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  } 
  if(class(m1) %in% c('iv_robust')){
    temp = data.frame(beta = m1$coefficients, se = m1$std.error, p_value = m1$p.value)
  }
  
  
  # length1 = 2
  # m1 = lm(var ~  class_code * ecm_include_patient, data = dta_reg)
  # m1 = coeftest(m1, cluster.vcov(m1, dta_reg$list_id, df_correction = T))
  # temp = data.frame(beta = m1[,1], se = m1[,2], p_value = m1[,4])
  
  temp = add_column(temp, .before = 1, 'var' = rownames(temp))
  temp = temp[(nrow(temp)-length1):nrow(temp),]
  
  ### Round numbers
  temp$beta = sapply(temp$beta, round_flex)
  temp$se   = str_trim(paste0('(', sapply(temp$se, round_flex), ')'))
  
  ### Add significance stars
  temp$beta = ifelse(test = temp$p_value < 0.10 & temp$p_value >= 0.05, yes  = paste0(temp$beta, '$^{\\dotr}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.05  & temp$p_value >= 0.01, yes  = paste0(temp$beta, '$^{*}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.01  & temp$p_value >= 0.001, yes  = paste0(temp$beta, '$^{**}$'), no = temp$beta)
  temp$beta = ifelse(test = temp$p_value < 0.001, yes  = paste0(temp$beta, '$^{***}$'), no = temp$beta)
  
  
  ### To long
  temp$var_clean = temp$var
  temp = gather(temp, key, beta, -c(var, var_clean, p_value, se))
  temp = temp[order(temp$var), ]
  
  ### Clean variable (row) names
  #temp = temp %>% mutate(across(c(var_clean), ~ paste0('\\multirow{2}{*}{', .x, '}')))
  #temp$var_clean[seq(2,nrow(temp), 2)] = ''
  
  
  ###  Put minuses in $ signs, as otherwise they won't print correctly in LaTeX
  temp$beta = gsub('-', '$-$', temp$beta, fixed=T)
  
  ###  Put all columns in one dataframe column with LaTeX table separators
  # temp$cell1 = paste0(apply(temp[,c('var_clean', 'beta')], 1, paste, collapse = "&"), '\\')
  
  ### Final selection of columns
  temp = temp %>% dplyr::select(c(var, beta))
  
  ###  Return
  return(temp)
}


#
# PLOT THEMES --------------------------------------------------------------------------------------------------
#

est_blue = '#0072ce' # Estonian blue flag

update_geom_defaults("point", list(shape = 21, fill = "#0072ce", color = 'black'))
update_geom_defaults("bar", list(fill = "#0072ce", colour = "black"))




### Define default ggplot theme

theme_set(  theme_bw() +
              theme(
                plot.title = element_markdown(color = 'black', size = 33, hjust = 0.5),
                plot.subtitle = element_markdown(color = 'grey15', size = 32, hjust = 0.5),
                plot.caption= element_textbox_simple(face = 'plain', color = 'grey30', size = 19, hjust = 0),
                
                
                axis.text.x = element_markdown(size = 20),
                axis.text.y = element_markdown(size = 20),
                axis.title.x = element_markdown(size = 20),
                axis.title.y = element_markdown(size = 20),
                
                axis.line = element_line(),
                axis.ticks = element_line(),
                
                legend.direction = 'horizontal',
                legend.position = 'bottom',
                legend.title = element_text(size = 34, hjust = 0.5),
                legend.text = element_text(size = 30, hjust = 0),
                
                legend.key.spacing.x = unit(3, 'cm'),
                
                plot.background = element_rect(fill = 'white', color = 'NA'),
                panel.background = element_rect(fill = 'white', color = 'NA'),
                panel.border = element_blank(),
                panel.grid.major = element_blank(), #remove major gridlines
                panel.grid.minor = element_blank(), #remove minor gridlines
                
                strip.background = element_rect(color = NA, fill = NA),
                strip.text = element_markdown(size = 20),
              )
            
)



### set palettes
colors_map1 = paletteer_d("ggsci::default_nejm")
palette1 = paletteer_d("ggsci::category20c_d3")

map_theme = theme_minimal() +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    legend.title = element_text(hjust = 0.5, size = 45, color = 'black'),
    legend.text = element_text(face = 'plain', size = 20),
    legend.ticks = element_line(color = 'black'),
    # legend.key.width = unit(4.5, "cm"),
    # legend.key.height = unit(1, "cm"),
    legend.title.align = 0.5,
    # legend.box.just = "center",
    
    plot.title = element_markdown(size = 80, color = 'black', hjust = 0.5),
    plot.subtitle = element_markdown(color = 'black', size = 70, hjust = 0.5),
    plot.caption= element_textbox_simple(color = 'black', size = 35, hjust = 0),
    
    axis.text = element_blank(),
    panel.background = element_rect(fill = 'white', colour = 'transparent'),
    plot.background  = element_rect(fill = 'white', colour = 'transparent'),
    panel.grid = element_blank())



mapa <- function(x, fill_column = NULL){
  if(is.null(fill_column)){
    ggplot(x) + geom_sf(col = 'grey20', lwd = .2, fill = NA) + map_theme
  } else {
    ggplot(x) + geom_sf(aes(fill = .data[[fill_column]]), col = 'grey20', lwd = .2) +
      guides(fill = 'none') + map_theme
  }
}

# x = vote_shp
# fill_column = 'pop_2019'
# id_column = 'code_lau'

mapa2 <- function(x, fill_column, id_column = NA){
  
  bbox <- c(xmin = 21.5, ymin = 57.5, xmax = 28.2, ymax = 59.9)
  bbox <- st_polygon(list(rbind(c(bbox["xmin"], bbox["ymin"]),
                                c(bbox["xmax"], bbox["ymin"]),
                                c(bbox["xmax"], bbox["ymax"]),
                                c(bbox["xmin"], bbox["ymax"]),
                                c(bbox["xmin"], bbox["ymin"]))))
  bbox <- st_sf(geometry = st_sfc(bbox))
  bbox <- st_set_crs(bbox, st_crs(x))
  
  if(!is.na(id_column)){
    x$id = x[[id_column]]
    temp <- st_intersection(x, bbox)
    x = x %>% filter(id %in% temp$id)
  }
  
  
  x = x %>% st_transform(crs = crs1)
  
  
  mean1 = mean(x[[fill_column]], na.rm=T)
  median1 = mean(x[[fill_column]], na.rm=T)
  min1 = min(x[[fill_column]], na.rm=T)
  max1 = max(x[[fill_column]], na.rm=T)
  
  ggplot() +
    geom_sf(data = x,
            aes(fill = .data[[fill_column]]),
            color = NA, linewidth = 0
    )+
    scale_fill_gradient2(low = "darkblue", high = "#EC5F06",
                         mid = "white", midpoint = median1,
                         breaks = seq(min1, max1, length.out = 7)
    ) +
    
    guides(fill = guide_colorbar(title.position = 'left')) +
    
    map_theme +
    theme(
      legend.position = 'left',
      legend.direction = 'vertical',
      legend.title = element_markdown(size = 20, angle = 90),
      legend.text = element_text(face = 'plain', size = 20),
      legend.ticks = element_line(color = 'grey70', size = .9),
      legend.key.width = unit(.8, 'cm'),
      legend.key.height = unit(3.6, 'cm')
    )
  
  
}

#
# FIN DEL CÓDIGO --------------------------------------------------------------------------------------------------------
#
