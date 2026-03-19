library(data.table)
library(haven)
library(labelled)

# To demonstrate sample bias, we use sample moments from one of the largest 
# national household surveys: KOUZs..KOUZs is Comprehensive Monitoring of Living Conditions  
# (CMLC, Kompleksnoe nablyudenie uslovii zhizni naselenia). 
# This is a biennial survey conducted by the Federal State Statistics Service of Russian Federation (Rosstat), 
# involving over 100,000 respondents. Our choice is motivated by the ease of aligning the primary data from both surveys. 
# The output is available at: data/auxdata/kouzh_18_20_22_24_5sep25.rdata.

# Load KOUZs household and individual level data 
# https://rosstat.gov.ru/free_doc/new_site/kouz18/index.html
kouzh_2018_individual <- as.data.table(as.data.frame(read_sav("kouzh/2018/processed/IND.sav")))
kouzh_2018_household <- as.data.table(as.data.frame(read_sav("kouzh/2018/processed/HHOLD.sav")))
# https://rosstat.gov.ru/free_doc/new_site/GKS_KOUZH-2020/index.html
kouzh_2020_individual <- as.data.table(as.data.frame(read_sav("kouzh/2020/processed/IND.sav")))
kouzh_2020_household <- as.data.table(as.data.frame(read_sav("kouzh/2020/processed/HHOLD.sav")))
# https://rosstat.gov.ru/free_doc/new_site/GKS_KOUZH_2022/index.html
kouzh_2022_individual <- as.data.table(as.data.frame(read_sav("kouzh/2022/processed/IND.sav")))
kouzh_2022_household <- as.data.table(as.data.frame(read_sav("kouzh/2022/processed/HHOLD.sav")))
# https://rosstat.gov.ru/free_doc/new_site/GKS_KOUZH_2024/index.html
kouzh_2024_individual <- as.data.table(as.data.frame(read_sav("kouzh/2024/processed/IND_OSN.sav")))
kouzh_2024_household <- as.data.table(as.data.frame(read_sav("kouzh/2024/processed/HHOLD_OSN.sav")))


# Rename variables of interest 
setnames(kouzh_2018_individual, c("H00_06", "H01_00", "H00_02", "H01_01", "H01_02", "I07_01", "I05_01", "I01_01", "I06_01", "KVZV"), c("household_id", "individual_id", "region_id", "sex", "age", "education_level", "work_status", "marital_status", "receives_pension", "survey_weight"))
setnames(kouzh_2018_household, c("H00_06", "H00_04", "H00_02", "H02_36_05_01", "CHLICN", "DOX_mean", "DEN_NA_DUSHU"), c("household_id", "is_rural", "region_id", "has_cellphone", "household_size", "household_income", "mean_household_income"))

setnames(kouzh_2020_individual, c("H00_06", "H01_00", "H00_02", "H01_01", "H01_02", "I07_01", "I05_01", "I01_01", "I06_01", "KVZV"), c("household_id", "individual_id", "region_id", "sex", "age", "education_level", "work_status", "marital_status", "receives_pension", "survey_weight"))
setnames(kouzh_2020_household, c("H00_06", "H00_04", "H00_02", "H02_36_05_01", "CHLICN", "DOX_mean", "DEN_NA_DUSHU"), c("household_id", "is_rural", "region_id", "has_cellphone", "household_size", "household_income", "mean_household_income"))

setnames(kouzh_2022_individual, c("H00_06", "H01_00", "H00_02", "H01_01", "H01_02", "I07_01", "I05_01", "I01_01", "I06_01", "KVZV"), c("household_id", "individual_id", "region_id", "sex", "age", "education_level", "work_status", "marital_status", "receives_pension", "survey_weight"))
setnames(kouzh_2022_household, c("H00_06", "H00_04", "H00_02", "H02_36_05_01", "CHLICN", "DOX_mean", "DEN_NA_DUSHU"), c("household_id", "is_rural", "region_id", "has_cellphone", "household_size", "household_income", "mean_household_income"))

setnames(kouzh_2024_individual, c("H00_06", "H01_00", "H00_02", "H01_01", "H01_02", "I07_01", "I05_01", "I01_01", "I06_01", "KVZV"), c("household_id", "individual_id", "region_id", "sex", "age", "education_level", "work_status", "marital_status", "receives_pension", "survey_weight"))
# CHLICN (Число наличных лиц в домохозяйстве) -> ALL_CHLICN
setnames(kouzh_2024_household, c("H00_06", "H00_04", "H00_02", "H02_36_05_01", "ALL_CHLICN", "DOX_mean", "DEN_NA_DUSHU"), c("household_id", "is_rural", "region_id", "has_cellphone", "household_size", "household_income", "mean_household_income"))

##### Select and save victimization data for the year 2024 separately.
## [K24] Существуют ли в Вашем населенном пункте (в районе Вашего проживания) проблемы
# I02_03_01 Высокий уровень преступности (нарушение общественного порядка)

## I02_07О Насколько безопасно Вы себя чувствуете на улице в Вашем населенном пункте (в районе Вашего проживания) в темное время суток?

## K 24.1 Приходилось ли Вам в течение последних 12 месяцев сталкиваться с действиями противоправного характера в отношении Вас, Ваших детей, Вашего имущества или общего имущества домохозяйства?
# I02_04_01_01 Кража
# I02_04_01_02 Ограбление
# I02_04_01_03 Нападение или угроза
# I02_04_01_04 Психологическое насилие
# I02_04_01_05 Сексуальные правонарушения
# I02_04_01_06 Конфликты в быту
# I02_04_01_07 Мошенничество
# I02_04_01_08 Причинение вреда здоровью
# I02_04_01_09 Хулиганство
kouzh_victim_24 <- kouzh_2024_individual[, .(household_id, individual_id, age, survey_weight, I02_04_01_01, I02_04_01_02, I02_04_01_03, I02_04_01_04, I02_04_01_05, I02_04_01_06, I02_04_01_07, I02_04_01_08, I02_04_01_09)]
kouzh_victim_24 <- kouzh_victim_24[age >= 18]
setnames(kouzh_victim_24, c("I02_04_01_01", "I02_04_01_02", "I02_04_01_03", "I02_04_01_04", "I02_04_01_05", "I02_04_01_06", "I02_04_01_07", "I02_04_01_08", "I02_04_01_09"), c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство"))

# Remove attributes
kouzh_victim_24 <- copy(remove_attributes(kouzh_victim_24, attributes = c("label", "format.spss", "display_width", "labels")))

# Replace missings with NA
kouzh_victim_24[, names(kouzh_victim_24) := lapply(.SD, function(x) { ifelse(x == "", NA_character_, x) })]

# Replace values
kouzh_victim_24[, c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство") := lapply(.SD, as.numeric), .SDcols = c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство")]
kouzh_victim_24[, c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство") := lapply(.SD, function(x) { abs(x-2) }), .SDcols = c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство")]

# Add flag with any victimization
kouzh_victim_24[, victim12m := apply(.SD, 1, sum, na.rm = T), .SDcols = c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство")]
kouzh_victim_24[, victim12m := ifelse(victim12m >= 1, 1, 0)]
kouzh_victim_24[, .(victim12m_rate = round(mean(victim12m)*100,2), victim12m_weighted = round(weighted.mean(victim12m, w = survey_weight)*100,2))]

t(kouzh_victim_24[, lapply(.SD, function(x) { round(weighted.mean(x, w = survey_weight, na.rm = T)*100,2) }), .SDcols = c("Кража", "Ограбление", "Нападение_или_угроза", "Психологическое_насилие", "Сексуальные_правонарушения", "Конфликты_в_быту", "Мошенничество", "Причинение_вреда_здоровью", "Хулиганство")])

# Save file
save(kouzh_victim_24, file = "data/auxdata/kouzh_victim_24.rdata", compress = "xz")

##### 

# Create an object with KOUZh data of interest
col_interest <- c("household_id", "individual_id", "sex", "age", "education_level", "work_status", "marital_status", "receives_pension", "survey_weight")
kouzh_2018_data <- kouzh_2018_individual[, ..col_interest]
kouzh_2020_data <- kouzh_2020_individual[, ..col_interest]
kouzh_2022_data <- kouzh_2022_individual[, ..col_interest]
kouzh_2024_data <- kouzh_2024_individual[, ..col_interest]

# Add household-level variables
kouzh_2018_data <- merge(kouzh_2018_data, kouzh_2018_household[, c("household_id", "is_rural", "has_cellphone", "household_size", "household_income", "mean_household_income")], by = "household_id", all.x = T, all.y = F)
kouzh_2020_data <- merge(kouzh_2020_data, kouzh_2020_household[, c("household_id", "is_rural", "has_cellphone", "household_size", "household_income", "mean_household_income")], by = "household_id", all.x = T, all.y = F)
kouzh_2022_data <- merge(kouzh_2022_data, kouzh_2022_household[, c("household_id", "is_rural", "has_cellphone", "household_size", "household_income", "mean_household_income")], by = "household_id", all.x = T, all.y = F)
kouzh_2024_data <- merge(kouzh_2024_data, kouzh_2024_household[, c("household_id", "is_rural", "has_cellphone", "household_size", "household_income", "mean_household_income")], by = "household_id", all.x = T, all.y = F)

# Remove unnecessary files
rm(kouzh_2018_individual, kouzh_2020_individual, kouzh_2022_individual, kouzh_2024_individual, kouzh_2018_household, kouzh_2020_household, kouzh_2022_household, kouzh_2024_household); gc()

# Bind
kouzh_2018_data[, year := 2018]
kouzh_2020_data[, year := 2020]
kouzh_2022_data[, year := 2022]
kouzh_2024_data[, year := 2024]

kouzh_2018_data[, names(kouzh_2018_data) := lapply(.SD, as.character)]
kouzh_2020_data[, names(kouzh_2020_data) := lapply(.SD, as.character)]
kouzh_2022_data[, names(kouzh_2022_data) := lapply(.SD, as.character)]
kouzh_2024_data[, names(kouzh_2024_data) := lapply(.SD, as.character)]

kouzh_18_20_22_24 <- copy(rbind(kouzh_2018_data, kouzh_2020_data, kouzh_2022_data, kouzh_2024_data))
rm(kouzh_2018_data, kouzh_2020_data, kouzh_2022_data, kouzh_2024_data); gc()

# Remove redundant attributes
kouzh_18_20_22_24 <- copy(remove_attributes(kouzh_18_20_22_24, attributes = c("label", "format.spss", "display_width", "labels")))

# Make variables conformable with RCVS survey
# Education
kouzh_18_20_22_24[, education := NA_real_]
kouzh_18_20_22_24[education_level %in% c(7, 8, 9), education := 1]
kouzh_18_20_22_24[education_level %in% c(5, 6), education := 2]
kouzh_18_20_22_24[education_level %in% c(1, 2, 3, 4), education := 3]
kouzh_18_20_22_24[, education_level := NULL]
setnames(kouzh_18_20_22_24, "education", "education_level")
kouzh_18_20_22_24[, .N, by = education_level]

# To binary variables
yes_no_variables <- c("work_status", "has_cellphone", "receives_pension")
kouzh_18_20_22_24[, c(yes_no_variables) := lapply(.SD, as.numeric), .SDcols = yes_no_variables]
kouzh_18_20_22_24[, c(yes_no_variables) := lapply(.SD, function(x) { abs(x-2) }), .SDcols = yes_no_variables]

# Male dummy
kouzh_18_20_22_24[, male := NA_real_]
kouzh_18_20_22_24[sex == 1, male := 1]
kouzh_18_20_22_24[sex == 2, male := 0]
kouzh_18_20_22_24[, .N, by = male]
kouzh_18_20_22_24[, sex := NULL]

# Unemployed dummy
setnames(kouzh_18_20_22_24, "work_status", "employed")
kouzh_18_20_22_24[, unemployed := 1 - employed]
kouzh_18_20_22_24[, .N, by = unemployed]

# Lives alone dummy
kouzh_18_20_22_24[, lives_alone := NA_real_]
kouzh_18_20_22_24[!is.na(household_size), lives_alone := 0]
kouzh_18_20_22_24[household_size == 1, lives_alone := 1]
kouzh_18_20_22_24[, .N, by = lives_alone]

# Is married dummy
kouzh_18_20_22_24[, married := NA_real_]
kouzh_18_20_22_24[!is.na(marital_status), married := 0]
kouzh_18_20_22_24[marital_status %in% c(1, 2), married := 1]
kouzh_18_20_22_24[, .N, by = married]

# Create age groups and remove children from the data
kouzh_18_20_22_24 <- kouzh_18_20_22_24[age >= 18]
kouzh_18_20_22_24[, agegroup := NA_character_]
kouzh_18_20_22_24[age >= 18 & age <= 24, agegroup := "18-24"]
kouzh_18_20_22_24[age >= 25 & age < 35, agegroup := "25-34"]
kouzh_18_20_22_24[age >= 35 & age < 50, agegroup := "35-49"]
kouzh_18_20_22_24[age >= 50 & age < 65, agegroup := "50-64"]
kouzh_18_20_22_24[age >= 65, agegroup := "65+"]
kouzh_18_20_22_24[, agegroup := as.factor(agegroup)]
kouzh_18_20_22_24[, .N, keyby = agegroup]

# Deflate income and other variables to 2022 rubles
# Load GDP deflator data from https://rosstat.gov.ru/free_doc/new_site/vvp/vvp-god/tab4.htm
# https://www.fedstat.ru/indicator/57352
# 2024: https://rosstat.gov.ru/folder/313/document/254129#:~:text=ВВП%20за%202022%20год%20–156941,дефлятор%20–%20118%2C2%25.
gdp_deflator <- fread("year,GDP_deflator
	2018,110.0
	2019,103.3
  2020,100.9
  2021,118.2 
  2022,118.2
  2023,107.0
  2024,108.9
")
gdp_deflator[, gdp_deflator_base := NA_real_]
gdp_deflator[year == 2024, gdp_deflator_base := 100]

# Carry backward
for(y in (2024-1):min(gdp_deflator$year)) {
  gdp_deflator[year == y, gdp_deflator_base := gdp_deflator[year == (y + 1)]$gdp_deflator_base/(gdp_deflator[year == (y + 1)]$GDP_deflator/100)]
}
# Carry forward
for(y in (2024+1):max(gdp_deflator$year)) {
  gdp_deflator[year == y, gdp_deflator_base := gdp_deflator[year == (y - 1)]$gdp_deflator_base*(gdp_deflator[year == y]$GDP_deflator/100)]
}

gdp_deflator[, deflator2024rub := (gdp_deflator_base / 100)]

# Add the deflator to the variables
kouzh_18_20_22_24[, year := as.integer(year)]
kouzh_18_20_22_24[gdp_deflator, "deflator2024rub" := deflator2024rub, on = .(year)]

vars_to_deflate <- c("household_income", "mean_household_income")
kouzh_18_20_22_24[, c(vars_to_deflate) := lapply(.SD, as.numeric), .SDcols = vars_to_deflate]
kouzh_18_20_22_24[, c(vars_to_deflate) := lapply(.SD, function(x) { x / deflator2024rub }), .SDcols = vars_to_deflate]

# Urban/rural dummy
kouzh_18_20_22_24[is_rural == 1, is_rural := 0]
kouzh_18_20_22_24[is_rural == 2, is_rural := 1]
kouzh_18_20_22_24[, is_rural := as.numeric(is_rural)]

# Population and income variables to thousands
vars_to_1000_kouzh <- c("household_income", "mean_household_income")
kouzh_18_20_22_24[, c(vars_to_1000_kouzh) := lapply(.SD, function(x) { round(x/1000, 0) }), .SDcols = vars_to_1000_kouzh]
kouzh_18_20_22_24[, c(vars_to_1000_kouzh) := lapply(.SD, function(x) { ifelse(x == 0, 1, x) }), .SDcols = vars_to_1000_kouzh]

# Save point
save(kouzh_18_20_22_24, file = "data/auxdata/kouzh_18_20_22_24_5sep25.rdata", compress = "xz")