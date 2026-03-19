library(data.table)
library(autumn) # Guid: https://www.youtube.com/watch?v=qeGltVhozNI
library(openxlsx)
library(stringi)
library(stringr)
library(stringdist)
library(fixest)
library(ggplot2)
library(Amelia)
library(Hmisc)

options(scipen=999)
set.seed(666)

# Prepare target distributions for raking----
## Rosstat: fd-age-sex----
# Load the region-agegroup-sex population
# (https://rosstat.gov.ru/compendium/document/13284)
region_sex_age_yearly <- fread("data/auxdata/region_sex_age_yearly_population_2018_2024.csv")

# Add federal district identifiers
region_sex_age_yearly[region %in% c("Белгородская область", "Брянская область", "Владимирская область", "Воронежская область", "Ивановская область", "Калужская область", "Костромская область", "Курская область", "Липецкая область", "Московская область", "Орловская область", "Рязанская область", "Смоленская область", "Тамбовская область", "Тверская область", "Тульская область", "Ярославская область"), federal_district := "Центральный ФО"]
region_sex_age_yearly[region %in% c("г. Москва"), federal_district := "г. Москва"]
region_sex_age_yearly[region %in% c("Республика Карелия", "Республика Коми", "Архангельская область", "Ненецкий автономный округ", "Вологодская область", "Калининградская область", "Ленинградская область", "Мурманская область", "Новгородская область", "Псковская область"), federal_district := "Северо-Западный ФО"]
region_sex_age_yearly[region %in% c("г. Санкт-Петербург"), federal_district := "г. Санкт-Петербург"]
region_sex_age_yearly[region %in% c("Республика Адыгея", "Республика Калмыкия", "Республика Крым", "Краснодарский край", "Астраханская область", "Волгоградская область", "Ростовская область", "г. Севастополь"), federal_district := "Южный ФО"]
region_sex_age_yearly[region %in% c("Республика Дагестан", "Республика Ингушетия", "Кабардино-Балкарская Республика", "Карачаево-Черкесская Республика", "Республика Северная Осетия – Алания", "Чеченская Республика", "Ставропольский край"), federal_district := "Северо-Кавказский ФО"]
region_sex_age_yearly[region %in% c("Республика Башкортостан", "Республика Марий Эл", "Республика Мордовия", "Республика Татарстан", "Удмуртская Республика", "Чувашская Республика", "Пермский край", "Кировская область", "Нижегородская область", "Оренбургская область", "Пензенская область", "Самарская область", "Саратовская область", "Ульяновская область"), federal_district := "Приволжский ФО"]
region_sex_age_yearly[region %in% c("Курганская область", "Свердловская область", "Тюменская область", "Ханты-Мансийский автономный округ – Югра", "Ямало-Ненецкий автономный округ", "Челябинская область"), federal_district := "Уральский ФО"]
region_sex_age_yearly[region %in% c("Республика Алтай", "Республика Тыва", "Республика Хакасия", "Алтайский край", "Красноярский край", "Иркутская область", "Кемеровская область", "Новосибирская область", "Омская область", "Томская область"), federal_district := "Сибирский ФО"]
region_sex_age_yearly[region %in% c("Республика Бурятия", "Республика Саха (Якутия)", "Забайкальский край", "Камчатский край", "Приморский край", "Хабаровский край", "Амурская область", "Магаданская область", "Сахалинская область", "Еврейская автономная область", "Чукотский автономный округ"), federal_district := "Дальневосточный ФО"]

# Unify region names
region_sex_age_yearly[, region := gsub(" – ", "-", region)]

# Define age groups of interest
# (3) collapse young categories
region_sex_age_yearly[age >= 18 & age <= 34, agegroup := "18-34"]
region_sex_age_yearly[age >= 35 & age <= 49, agegroup := "35-49"]
region_sex_age_yearly[age >= 50 & age <= 64, agegroup := "50-64"]
region_sex_age_yearly[age >= 65, agegroup := "65+"]

# Male variable
region_sex_age_yearly[sex == "male", male := 1]
region_sex_age_yearly[sex == "female", male := 0]
region_sex_age_yearly[, sex := NULL]

# Compute region adult population counts
region_population_yearly <- region_sex_age_yearly[!is.na(agegroup), .(population = sum(population)), by = c("region", "year")]

# Compute federal district population counts by age groups
federal_district_population_sex_agegroup_yearly <- region_sex_age_yearly[!is.na(agegroup), .(population = sum(population)), by = c("federal_district", "year", "male", "agegroup")]

# Incorporate new regions 
# Данные ТО: источник не установлен
# Данные ЦИК о числе избирателей на выборах Президента РФ 2024: http://www.cikrf.ru/analog/prezidentskiye-vybory-2024/p-itogi/tablica-%E2%84%967-(467)-2024.pdf [2025.02.26]			
# Данные Kontur Population (ноябрь 2023): https://data.humdata.org/dataset/kontur-population-dataset [2025.02.26]			

# Данные                          ТО	    ЦИК	    Kontur
#95 Донецкая Народная Республика	2013774	2031147	3511891
#96 Луганская Народная Республика	1683694	1762225	1800482
#97 Запорожская область	          496157	535728	1476988
#98 Херсонская область	          543294	482579	871375

# Create new region population DT
newregion_population <- data.table(region = c("Донецкая Народная Республика", "Луганская Народная Республика", "Херсонская область", "Запорожская область"), 
                                   year = 2024, 
                                   population = c(2031147, 1762225, 482579, 535728))
# Attach them to the regional target
region_population_yearly <- rbind(region_population_yearly, newregion_population)

# Create new federal district DT — gathering all new regions in one district
newfd_population <- data.table(federal_district = "Новые регионы", 
                               year = 2024, 
                               CJ(male = c(1,0), agegroup = c("18-34", "35-49", "50-64", "65+")),
                               population = sum(2031147, 1762225, 482579, 535728))

# Linear imputation of age categories for a new district. 
# We will use the average proportions computed by federal districts (excluding msk and spb)
averaged_prop <- federal_district_population_sex_agegroup_yearly[year == 2024 & !federal_district %in% c("г. Москва", "г. Санкт-Петербург")]
averaged_prop[, proportion := population/sum(population), by = .(federal_district, male)]
averaged_prop <- averaged_prop[, .(proportion = mean(proportion)), by = .(male, agegroup)]

newfd_population[averaged_prop, "proportion" := i.proportion, on = .(agegroup, male)]
newfd_population[, imputed_population := population*proportion]
newfd_population[, c("population", "proportion") := NULL]
setnames(newfd_population, "imputed_population", "population")

# Attach imputed population the federal district target
federal_district_population_sex_agegroup_yearly <- rbind(federal_district_population_sex_agegroup_yearly, newfd_population)
rm(newfd_population, newregion_population, averaged_prop)
fwrite(federal_district_population_sex_agegroup_yearly, file = "data/auxdata/federal_district_population_sex_agegroup_yearly.csv")


## Rosstat: fd-higheduc----
region_educ_population_census <- fread("data/auxdata/region_educ_population_census.csv")
# Add index of age category for convenience using
region_educ_population_census[, index := .GRP, by = age]
# Keep only needed values
region_educ_population_census <- region_educ_population_census[!region %in% c("Российская Федерация") & !grepl("федеральный округ", region, ignore.case = T)]

# Rename a couple of region 
region_educ_population_census[region == "Якутия", region := "Республика Саха (Якутия)"]
region_educ_population_census[region == "Ханты-Мансийский автономный округ", region := "Ханты-Мансийский автономный округ – Югра"]

# Mark federal districts
region_educ_population_census[region %in% c("Белгородская область", "Брянская область", "Владимирская область", "Воронежская область", "Ивановская область", "Калужская область", "Костромская область", "Курская область", "Липецкая область", "Московская область", "Орловская область", "Рязанская область", "Смоленская область", "Тамбовская область", "Тверская область", "Тульская область", "Ярославская область"), federal_district := "Центральный ФО"]
region_educ_population_census[region %in% c("г. Москва"), federal_district := "г. Москва"]
region_educ_population_census[region %in% c("Республика Карелия", "Республика Коми", "Архангельская область", "Ненецкий автономный округ", "Вологодская область", "Калининградская область", "Ленинградская область", "Мурманская область", "Новгородская область", "Псковская область"), federal_district := "Северо-Западный ФО"]
region_educ_population_census[region %in% c("г. Санкт-Петербург"), federal_district := "г. Санкт-Петербург"]
region_educ_population_census[region %in% c("Республика Адыгея", "Республика Калмыкия", "Республика Крым", "Краснодарский край", "Астраханская область", "Волгоградская область", "Ростовская область", "г. Севастополь"), federal_district := "Южный ФО"]
region_educ_population_census[region %in% c("Республика Дагестан", "Республика Ингушетия", "Кабардино-Балкарская Республика", "Карачаево-Черкесская Республика", "Республика Северная Осетия – Алания", "Чеченская Республика", "Ставропольский край"), federal_district := "Северо-Кавказский ФО"]
region_educ_population_census[region %in% c("Республика Башкортостан", "Республика Марий Эл", "Республика Мордовия", "Республика Татарстан", "Удмуртская Республика", "Чувашская Республика", "Пермский край", "Кировская область", "Нижегородская область", "Оренбургская область", "Пензенская область", "Самарская область", "Саратовская область", "Ульяновская область"), federal_district := "Приволжский ФО"]
region_educ_population_census[region %in% c("Курганская область", "Свердловская область", "Тюменская область", "Ханты-Мансийский автономный округ – Югра", "Ямало-Ненецкий автономный округ", "Челябинская область"), federal_district := "Уральский ФО"]
region_educ_population_census[region %in% c("Республика Алтай", "Республика Тыва", "Республика Хакасия", "Алтайский край", "Красноярский край", "Иркутская область", "Кемеровская область", "Новосибирская область", "Омская область", "Томская область"), federal_district := "Сибирский ФО"]
region_educ_population_census[region %in% c("Республика Бурятия", "Республика Саха (Якутия)", "Забайкальский край", "Камчатский край", "Приморский край", "Хабаровский край", "Амурская область", "Магаданская область", "Сахалинская область", "Еврейская автономная область", "Чукотский автономный округ"), federal_district := "Дальневосточный ФО"]

# Remove age under 18
region_educ_population_census <- region_educ_population_census[index >= 4]
# We keep all cases which are post degree, graduated, or incomplete high education
region_educ_population_census <- region_educ_population_census[, c("region", "federal_district", "age", "respondents", "kadry_vysshej_kvalifikacii", "vysshee", "nepolnoe_vysshee"), with = F]
# Calculate all respondents
region_educ_population_census[, n := sum(respondents), by = federal_district]
# Transform data into long table
region_educ_population_census <- melt(region_educ_population_census, id.vars = c("region", "federal_district", "age", "n"), measure.vars = c("kadry_vysshej_kvalifikacii", "vysshee", "nepolnoe_vysshee"), value.name = "1")
region_educ_population_census[`1` == "-", `1` := 0]
region_educ_population_census[, `1` := as.integer(`1`)]

# Calculate number of people without high education
region_educ_population_census <- region_educ_population_census[, .(n = n[1], `1` = sum(`1`)), by = federal_district]
region_educ_population_census[, `0` := n - `1`]
# Check
region_educ_population_census[, .(N = sum(n), educ_high = sum(`1`), percent = sum(`1`)/sum(n))]

federal_district_population_educ <- melt(region_educ_population_census, id.vars = c("federal_district"), measure.vars = c("0", "1"), variable.name = "educhigh", value.name = "population", value.factor = F, variable.factor = F)

# Add new regions
# Create new federal district DT — gathering all new regions in one district
newfd_educ <- data.table(federal_district = "Новые регионы", 
                         educhigh = c("0", "1"),
                         population = sum(2031147, 1762225, 482579, 535728))

# Linear imputation of age categories for a new district. 
# We will use the average proportions computed by federal districts (excluding msk and spb)
averaged_prop_educ <- federal_district_population_educ[!federal_district %in% c("г. Москва", "г. Санкт-Петербург")]
averaged_prop_educ[, proportion := population/sum(population), by = .(federal_district)]
averaged_prop_educ <- averaged_prop_educ[, .(proportion = mean(proportion)), by = .(educhigh)]

newfd_educ[averaged_prop_educ, "proportion" := i.proportion, on = .(educhigh)]
newfd_educ[, imputed_population := population*proportion]
newfd_educ[, c("population", "proportion") := NULL]
setnames(newfd_educ, "imputed_population", "population")

# Attach imputed population the federal district target
federal_district_population_educ <- rbind(federal_district_population_educ, newfd_educ)
rm(newfd_educ, averaged_prop_educ)
fwrite(federal_district_population_educ, file = "data/auxdata/federal_district_population_educ.csv")


# RCVS data-----
# Load full cross-section data prepared beforehand
rcvs <- as.data.table(readRDS("data/rdd/rcvs_18_21_24_2025-07-05.Rds"))
# Keep only needed cols
rcvs <- rcvs[, c("ID", "year", "Q1005", "Q2", "Age_Groups", "Q2001", "resp_is_male", "resp_edu", "resp_is_crime_victim")]
rcvs[, names(rcvs) := lapply(.SD, as.character)]
# Rename some variables
setnames(rcvs, c("ID", "Q1005", "Q2", "Age_Groups", "Q2001", "resp_is_male", "resp_is_crime_victim"), c("id", "region", "age", "agegroup", "federal_district", "male", "victim"))

# Collapse tow youngest categories into one
rcvs[agegroup == "18-24", agegroup := "18-34"]
rcvs[agegroup == "25-34", agegroup := "18-34"]

# Check and replace NA
# sex
rcvs[is.na(male), .N]

# region
rcvs[region %in% c("Нет данных", "Отказ"), .N] # 37
rcvs[region %in% c("Нет данных", "Отказ"), region := NA_character_]

# age
rcvs[is.na(agegroup) | agegroup == "Нет данных", .N] # 5
rcvs[is.na(agegroup) | agegroup == "Нет данных", agegroup := NA_character_]

# education
rcvs[resp_edu == "Высшее и незаконченное высшее", resp_edu := 1]
rcvs[resp_edu %in% c("Полное среднее и ниже", "Среднее спец-ное/техническое или нач-ное профес-ное"), resp_edu := 0]
# Missings of 2018 year — only 3000 observations we collected for non-victims with full socdem
rcvs[, .N, by = .(year, resp_edu)]

# Define msk and spb as federal districts 
rcvs[region == "г. Москва", federal_district := "г. Москва"]
rcvs[region == "г. Санкт-Петербург", federal_district := "г. Санкт-Петербург"]

# Unify districts names
rcvs[federal_district == "Уральский", federal_district := "Уральский ФО"]
rcvs[federal_district == "Приволжский", federal_district := "Приволжский ФО"]
rcvs[federal_district == "Центральный", federal_district := "Центральный ФО"]
rcvs[federal_district == "Северо-Западный", federal_district := "Северо-Западный ФО"]
rcvs[federal_district == "Южный", federal_district := "Южный ФО"]
rcvs[federal_district == "Северо-Кавказский", federal_district := "Северо-Кавказский ФО"]
rcvs[federal_district == "Дальневосточный", federal_district := "Дальневосточный ФО"]
rcvs[federal_district == "Сибирский", federal_district := "Сибирский ФО"]

# Gathering new regions in one district 
rcvs[region %in% c("Запорожская область", "Херсонская область", "Луганская Народная Республика", "Донецкая Народная Республика"), federal_district := "Новые регионы"]
rcvs[federal_district == "Не определён", federal_district := NA_character_]

# Make proper data format
rcvs[, c("male", "resp_edu", "victim", "age") := lapply(.SD, as.numeric), .SDcols = c("male", "resp_edu", "victim", "age")]


# Imputation high education for 2018 year----
rcvs18_imputed <- amelia(rcvs[year == 2018], m = 5, p2s = 2, idvars = c("id", "year", "federal_district", "agegroup"), 
                         noms = c("male", "victim", "resp_edu"), 
                         ords = c("age"), max.resample = 500, tolerance = 1e-6, parallel = "multicore", ncpus = 12, cs = "region")
rcvs18_imputed <- rbindlist(rcvs18_imputed$imputations, idcol = "version")
# Restore federal_district
rcvs18_imputed[is.na(region), federal_district := NA]


# Calculate proportions----
## Create a variable with federal district-sex-age group----
rcvs[!is.na(agegroup) & !is.na(federal_district), weighting_var_federal := paste(federal_district, male, agegroup, year, sep = "_")]
## Prepare federal district-high education tier
rcvs[!is.na(resp_edu) & !is.na(federal_district), weighting_var_higheducation := paste(federal_district, resp_edu, year, sep = "_")]
## Prepare region-year pairs
rcvs[!is.na(region), region := paste(region, year, sep = "_")]

# Create target distribution
# (...target_map = c("variable" = 1, "level" = 2, "proportion" = 3))
## Prepare federal district-sex-age group weighting---- 
target_distribution_federal_district_population_sex_agegroup_yearly <- copy(federal_district_population_sex_agegroup_yearly)
target_distribution_federal_district_population_sex_agegroup_yearly[, variable := "weighting_var_federal"]
target_distribution_federal_district_population_sex_agegroup_yearly[, level := paste(federal_district, male, agegroup, year, sep = "_")]
# Keep only cells in the data 
target_distribution_federal_district_population_sex_agegroup_yearly <- target_distribution_federal_district_population_sex_agegroup_yearly[level %in% rcvs$weighting_var_federal]
# From counts to proportions and impute averaged values for new region district
target_distribution_federal_district_population_sex_agegroup_yearly[, proportion := population/sum(population), by = year]
target_distribution_federal_district_population_sex_agegroup_yearly <- target_distribution_federal_district_population_sex_agegroup_yearly[, c("year", "variable", "level", "proportion")]

## Add federal high educations proportions----
target_distribution_higheducation <- copy(federal_district_population_educ)
target_distribution_higheducation[, variable := "weighting_var_higheducation"]
# Repeat dataset three times for each year
target_distribution_higheducation <- rbind(cbind(target_distribution_higheducation, "year" = 2018), cbind(target_distribution_higheducation, "year" = 2021), cbind(target_distribution_higheducation, "year" = 2024))
# Remove ineligible new regions from 2018 and 2021 datasets
target_distribution_higheducation <- target_distribution_higheducation[!(year %in% c(2018, 2021) & federal_district == "Новые регионы")]
target_distribution_higheducation[, level := paste(federal_district, educhigh, year, sep = "_")]
target_distribution_higheducation <- target_distribution_higheducation[level %in% rcvs$weighting_var_higheducation]
target_distribution_higheducation[, proportion := population/sum(population), by = year]
target_distribution_higheducation <- target_distribution_higheducation[, c("year", "variable", "level", "proportion")]

## Add regional population----
target_regional_distribution <- copy(region_population_yearly)
target_regional_distribution[, variable :=  "region"]
setnames(target_regional_distribution, "region", "level")
target_regional_distribution[, level := paste(level, year, sep = "_")]
target_regional_distribution <- target_regional_distribution[level %in% rcvs$region]
# From counts to proportions
target_regional_distribution[, proportion := population/sum(population), by = year]
target_regional_distribution <- target_regional_distribution[, c("year", "variable", "level", "proportion")]

# Attach regional populations to federal district age proportions
# (the order is significant)
target_distribution <- rbind(target_regional_distribution, target_distribution_federal_district_population_sex_agegroup_yearly, target_distribution_higheducation)
rm(target_regional_distribution, target_distribution_federal_district_population_sex_agegroup_yearly,  target_distribution_higheducation)

# If data contains an NA in raking variables, harvest() will ignore those observations when raking 
# on the variables where they are NA. This effectively means that when raking an age variable, 
# respondents with missing age are assumed to be correctly proportioned by age. 
# In addition, calculates of weighted marginals (for instance, for error), ignore NA respondents.

# Store list with options before run loop
all_combinations <- list(c("region", "weighting_var_federal"),
                         "weighting_var_federal", 
                         "weighting_var_higheducation",
                         "region",
                         c("weighting_var_federal", "weighting_var_higheducation"),
                         c("region", "weighting_var_federal", "weighting_var_higheducation"))

# Weighting original data----
# Store function for pooled sd
compute_sd_polled <- function(x, w) { sqrt( ((var(x, na.rm = T)*(length(na.omit(x))-1)) + (wtd.var(x, weights = w, na.rm = T)*(length(na.omit(x))-1))) / (length(na.omit(x))*2-2) ) }
# Store DT frame before run loop for computed metrics
performance <- data.table()
for(type in all_combinations) {
  
  # Store empty DT frame for final dataset
  raking_results <- data.table()
  
  for(y in c(2018, 2021, 2024)) {

    dttemp <- autumn::harvest(data = rcvs[year == y], 
                              target = target_distribution[year == y][variable %in% unlist(type)], verbose = 3, max_iterations = 10000, 
                              target_map = c("variable" = 2, "level" = 3, "proportion" = 4))
    # Return result
    raking_results <- rbind(raking_results, dttemp)
    
  }
                 
  # Compute DEFF
  deff <- raking_results[, .(deff_kish = design_effect(weights)), keyby = .(year)]
  
  # Compute standardized mean differences
  computed_smd <- raking_results[, sapply(.SD, function(x) { list(smd = (mean(x, na.rm = T) - weighted.mean(x, w = weights, na.rm = T)) / compute_sd_polled(x, w = weights)) }), .SDcols = c("male", "resp_edu", "victim"), by = year][,-"year"]
  
  # Check changes
  male <- raking_results[, .(male = mean(male), wmale = weighted.mean(male, w = weights)), keyby = .(year)][, -"year"]
  educ <- raking_results[, .(higheduc = mean(resp_edu, na.rm = T), whigheduc = weighted.mean(resp_edu, w = weights, na.rm = T)), keyby = .(year)][, -"year"]
  victim <- raking_results[, .(victim = mean(victim, na.rm = T), wvictim = weighted.mean(victim, w = weights, na.rm = T)), keyby = .(year)][, -"year"]
  
  # Gathering all together
  dttemp <- data.table(combination = paste0(unlist(type), collapse = " & "), deff, male, educ, victim, computed_smd)
  performance <- rbind(performance, dttemp)
  
}

# Change values with more self-explained names
performance[, combination := gsub("weighting_var_federal", "fd-age-sex", combination)]
performance[, combination := gsub("weighting_var_higheducation", "fd-higheduc", combination)]

# Make proper order
setcolorder(performance, c("year", "combination", "deff_kish", "male", "wmale", "male.smd", "higheduc", "whigheduc", "resp_edu.smd", "victim", "wvictim", "victim.smd"))
setnames(performance, "resp_edu.smd", "higheduc.smd")
# Round
performance[, names(.SD) := lapply(.SD, round, 3), .SDcols = names(performance)[!names(performance) %in% c("year", "combination")]]
# Save performance output
#write.xlsx(performance, file = "output/performance_originaldata.xlsx")


# Weighting imputed data----
## Create a variable with federal district-sex-age group----
rcvs18_imputed[!is.na(agegroup) & !is.na(federal_district), weighting_var_federal := paste(federal_district, male, agegroup, year, sep = "_")]
## Prepare federal district-high education tier
rcvs18_imputed[!is.na(resp_edu) & !is.na(federal_district), weighting_var_higheducation := paste(federal_district, resp_edu, year, sep = "_")]
## Prepare region-year pairs
rcvs18_imputed[!is.na(region), region := paste(region, year, sep = "_")]

# Store DT frame and vector of imputation result
performance_imputed <- data.table()
versions <- unique(rcvs18_imputed$version)

# Run loop for imputed data
for(type in all_combinations) {
  
  # Store empty DT frame for final dataset
  raking_imputed_results <- data.table()
  
  for(v in versions) {
    
    dttemp <- autumn::harvest(data = rcvs18_imputed[version == v], 
                              target = target_distribution[year == 2018][variable %in% unlist(type)], verbose = 3, max_iterations = 10000, 
                              target_map = c("variable" = 2, "level" = 3, "proportion" = 4))
    # Return result
    raking_imputed_results <- rbind(raking_imputed_results, dttemp)
    
  }
  
  # Averaging imputation weights
  raking_imputed_results <- raking_imputed_results[, .(weights = mean(weights), male = male[1], resp_edu = resp_edu[1], victim = victim[1]), by = .(id, year)]
  
  # Compute DEFF
  deff <- raking_imputed_results[, .(deff_kish = design_effect(weights)), keyby = .(year)]
  
  # Compute standardized mean differences
  computed_smd <- raking_imputed_results[, sapply(.SD, function(x) { list(smd = (mean(x, na.rm = T) - weighted.mean(x, w = weights, na.rm = T)) / compute_sd_polled(x, w = weights)) }), .SDcols = c("male", "resp_edu", "victim"), by = year][,-"year"]
  
  # Check changes
  male <- raking_imputed_results[, .(male = mean(male), wmale = weighted.mean(male, w = weights)), keyby = .(year)][, -"year"]
  educ <- raking_imputed_results[, .(higheduc = mean(resp_edu, na.rm = T), whigheduc = weighted.mean(resp_edu, w = weights, na.rm = T)), keyby = .(year)][, -"year"]
  victim <- raking_imputed_results[, .(victim = mean(victim, na.rm = T), wvictim = weighted.mean(victim, w = weights, na.rm = T)), keyby = .(year)][, -"year"]
  
  # Gathering all together
  dttemp <- data.table(combination = paste0(unlist(type), collapse = " & "), deff, male, educ, victim, computed_smd)
  performance_imputed <- rbind(performance_imputed, dttemp)
  
}

# Change values with more self-explained names
performance_imputed[, combination := gsub("weighting_var_federal", "fd-age-sex", combination)]
performance_imputed[, combination := gsub("weighting_var_higheducation", "fd-higheduc", combination)]

# Make proper order
setcolorder(performance_imputed, c("year", "combination", "deff_kish", "male", "wmale", "male.smd", "higheduc", "whigheduc", "resp_edu.smd", "victim", "wvictim", "victim.smd"))
setnames(performance_imputed, "resp_edu.smd", "higheduc.smd")
# Round
performance_imputed[, names(.SD) := lapply(.SD, round, 3), .SDcols = names(performance_imputed)[!names(performance_imputed) %in% c("year", "combination")]]
# Save performance output
#write.xlsx(performance_imputed, file = "output/performance_imputed.xlsx")

# Save weights----
computed_raking_weights <- rbind(raking_imputed_results[, .(id, year, weights)], raking_results[year != 2018, .(id, year, weights)])
setorderv(computed_raking_weights, c("year", "id"))  

## Unify performance list
performance_unified <- rbind(performance[year != 2018], performance_imputed) 
# Make proper order
setorderv(performance_unified, c("combination"))
# Save performance output
write.xlsx(performance_unified, file = "output/performance_unified.xlsx")

save(computed_raking_weights, file = "data/auxdata/computed_raking_weights_17apr25.rdata", compress = "gzip")