library(data.table)
library(autumn)
library(openxlsx)
library(haven)
library(lubridate)
library(fastDummies)
library(Hmisc)
options(scipen=999)

# Prepare data to compute interview propensity ----
# Core idea: We need to calculate two weights: 
# 1) initial contact propensity, and 2) full-interview propensity.
# The first weight captures unobservable characteristics of non-response, 
# such as interviewer effort and spatial/temporal coverage specifics.
# The second weight balances SES (socio-economic status) covariates across collected observations.

# Wave-2021
# Prepare year 2021 (take it from unified dataset)
rdd21 <- as.data.table(readRDS("data/rdd/rcvs_18_21_24_2026-03-14.Rds"))
rdd21 <- rdd21[year == 2021]
col_interest <- c("ID", "Q1005", "resp_is_male", "resp_is_married", "resp_is_living_alone", "Q2003", "resp_ses_is_employed", "Age_Groups", "resp_edu", "Q57")
rdd21 <- rdd21[, ..col_interest]

# Recode settlement type into rural area flag
rdd21[, is_rural := 0]
rdd21[Q2003 == "нет данных", is_rural := NA_integer_]
rdd21[Q2003 == "сельский населенный пункт", is_rural := 1]
rdd21[, Q2003 := NULL]

# We will use: male, marital_status, resp_is_living_alone, resp_ses_is_employed, agegroup, education_level, incomelvl
setnames(rdd21, c("ID", "resp_is_male", "resp_is_married", "resp_is_living_alone", "Age_Groups", "resp_edu", "Q57", "resp_ses_is_employed", "Q1005"),  c("id", "male", "married", "living_alone", "agegroup", "educationlvl", "incomelvl", "employed", "region"))
rdd21[, employed := 1-employed]
setnames(rdd21, "employed", "unemployed")

# Collapse the two youngest age categories into one
rdd21[agegroup %in% c("18-24", "25-34"), agegroup := "18-34"]
rdd21[, agegroup := factor(agegroup, levels = c("18-34", "35-49", "50-64", "65+"))]

# Cohort-2024 Load raw panel-file
cohort <- as.data.table(readRDS("data/panel/rcvs_panel_2024_2026-03-14.Rds"))

# Remove persons who do not allow call them in new wave
rdd21 <- rdd21[id %in% cohort$ID]

# Keep only full-interview
cohort <- cohort[final_result %in% c(7,8)] # 3456
# Attach flag for cohort observations
rdd21[cohort, "interview" := 1, on = .(id = panel_respID)]
rdd21[is.na(interview), interview := 0]

# Check NA
rdd21[, lapply(.SD, function(x) { sum(is.na(x)) })]
rdd21[is.na(agegroup), agegroup := "Нет данных"]

#################### Prepare data to compute contact propensity-----
# Load calls data. This dataset contains all attempts to contact the respondents. 
# Most interviews are the result of multiple phone calls.
contact <- as.data.table(read.xlsx("data/panel/calls_DP.xlsx"))
# Keep only needed cols
contact <- contact[, .(ID.контакта, `Дата./.Время`, Телефон, Логин, Результат, Регион.оператора.связи)]
# Rename variables
names(contact) <- c("idcontact", "date", "phone", "login", "result", "region_mobile")

# Load once again full-cohort-2024 and restore some ID-contact by phone
cohort <- as.data.table(read_sav("data/panel/2024.11_Виктимизация_панель_DP.sav"))
cohort <- cohort[, .(panel_respID, Phone, ContactID)]
names(cohort) <- c("panelid", "phone", "idcontact")

cohort[contact, "idcontact_restored" := i.idcontact, on = .(phone)]
cohort[idcontact == "" & !is.na(idcontact_restored), idcontact := idcontact_restored]
cohort[, idcontact_restored := NULL]

## Date
contact[, date := as.Date(dmy_hms(date))]
contact[, .N, by = date]
contact[, date := as.factor(date)]

## Region
contact[is.na(region_mobile), region_mobile := "Нет данных"]

## Detailed result
# We define these outcomes as successful contact.
# Timur Osmanov: Вот эти результаты потенциально говорят, что был контакт с человеком:
# Не подходит
# Отказ
# Ошибка связи
# Перезвон
# Перенос
# Прервано
# Успешно
# Из них сомнительно "не подходит" и "ошибка связи". 
# Не подходит - вероятно, человек не подходит под критерии отбора, т.е. не входит в ЦА (скорее всего, моложе 18 лет или телефон предприятия). 
# Получается, респондента тут и нет.
# Ошибка связи - не факт, что был реальный контакт с человеком. Возможно, была тишина, сбои и т.д.
# Эти 2 результата я бы учитывал с некоторой натяжкой, как контакты с респондентами.
contact[result %in% c("Успешно", "Перенос", "Перезвон", "Ошибка связи", "Прервано", "Отказ"), result := 1]
contact[result != 1, result := 0]

## Interviewer
# Mark all cases without an interviewer as an answer machine
contact[is.na(login), login := "iv_answering_machine"]
# Collapsed singletons 
contact[login %in% c("Б2_1237", "Б2_0709"), login := "iv_answering_machine"]
# Rename interviewers
contact[, iv := paste0("iv", .GRP), by = login]
contact[, login := NULL]

## Collapse call-tries with maximum result
contact[idcontact %in% contact[result == 1]$idcontact, result := 1]

# Transform long-table into wide-table because almost all cases have multiply tries
multiply_tries_connects <- data.table::dcast(contact, idcontact ~ iv, fun.aggregate = function(x) {ifelse(length(x) >= 1, 1, 0)})
date_dummies <- data.table::dcast(contact, idcontact ~ date, fun.aggregate = function(x) {ifelse(length(x) >= 1, 1, 0)})

# Return region and result to iv table
contact <- contact[, .(result = result[1], region_mobile = region_mobile[1]), by = .(idcontact)]
multiply_tries_connects <- merge(multiply_tries_connects, contact, by = c("idcontact"))
multiply_tries_connects <- merge(multiply_tries_connects, date_dummies, by = c("idcontact"))
multiply_tries_connects[, result := as.numeric(result)]
# Rename output variable
setnames(multiply_tries_connects, "result", "contact")
# Attach panelid
multiply_tries_connects[cohort, "panelid" := panelid, on = .(idcontact)]
# Remove two extra uncompleted cases 
multiply_tries_connects <- multiply_tries_connects[!is.na(panelid)]

#################### Gathering one dataset----
rdd21 <- merge(rdd21, multiply_tries_connects, by.x  = "id", by.y = "panelid", sort = F, all.x = T)
# Check missings 
rdd21[, lapply(.SD, function(x) sum(is.na(x)))]
# Make proper cols order
setcolorder(rdd21, c("id", "interview", "contact"))

## Compute attrition weight----
# Scenario: using all available information to compute interview propensity.
# Result: This option in the final gives extreme weights and higher deff, maybe the contact information gives noise.
#date_fe <- grep("^2024", names(rdd21), value = T)
#model_interview_with_fe_interviewers = glm(data = rdd21[, -c(c("id", "idcontact", "contact", "region_mobile"), date_fe), with = F], formula = interview ~ ., family = binomial(link = "logit"))

model_interview = glm(data = rdd21, formula = interview ~ male + married + living_alone + is_rural + unemployed + agegroup + educationlvl + incomelvl + region, family = binomial(link = "logit"))
length(predict(model_interview, type = "response"))

# Calculate interview propensity with IPW approach
rdd21[, interview_weight := 1/predict(model_interview, type = "response")]
#rdd21[, interview_with_fe_interviewer_weight := 1/predict(model_interview_with_fe_interviewers, type = "response")]

# Winsorization weights with default value cap from the library: autumn
rdd21[interview_weight > 5, interview_weight := 5]
#rdd21[interview_with_fe_interviewer_weight > 5, interview_with_fe_interviewer_weight := 5]

# Use percentile approach
#rdd21[interview_weight <= quantile(interview_weight, probs = 0.02), interview_weight := quantile(interview_weight, probs = 0.02)]
#rdd21[interview_weight >= quantile(interview_weight, probs = 0.98), interview_weight := quantile(interview_weight, probs = 0.98)]
# Normalized to N size
rdd21[, interview_weight := interview_weight/sum(interview_weight)*.N]
#rdd21[, interview_with_fe_interviewer_weight := interview_with_fe_interviewer_weight/sum(interview_with_fe_interviewer_weight)*.N]

## Compute contact propensity----
setdummies <- names(rdd21)[grepl("^2024", names(rdd21)) | grepl("^iv", names(rdd21))]
setdummies <- c("contact", "region_mobile", setdummies)
  
model_contact <- glm(data = rdd21[,..setdummies], formula = contact ~ ., family = binomial(link = "logit"))
#model_contact = glm(data = rdd21[, -c("id", "idcontact", "interview", "region_mobile")], formula = contact ~ ., family = binomial(link = "logit"))
length(predict(model_contact, type = "response"))

# Calculate interview propensity with IPW
rdd21[, contact_weight := 1/predict(model_contact, type = "response")]

# Rule-based
rdd21[contact_weight > 5, contact_weight := 5]
# Use percentile approach
#multiply_tries_connects[contact_weight <= quantile(contact_weight, probs = 0.02), contact_weight := quantile(contact_weight, probs = 0.02)]
#multiply_tries_connects[contact_weight >= quantile(contact_weight, probs = 0.98), contact_weight := quantile(contact_weight, probs = 0.98)]

# Normalized to N size
rdd21[is.na(contact_weight), contact_weight := 1]
rdd21[, contact_weight := contact_weight/sum(contact_weight)*.N]

# Attach weights----
## Poststratification
load("data/auxdata/computed_raking_weights_17apr25.rdata")
rdd21[computed_raking_weights, "raking_weight" := weights, on = .(id)]
rm(computed_raking_weights); gc()

## Attach final weighting
rcvs_panel <- as.data.table(readRDS("/Users/kuchakov/R/_criminology/create_victimization_data/data/panel/rcvs_panel_21_24_2026-03-12.Rds"))
rcvs_panel[rdd21, c("crosssection2021_weight", "interview_weight", "contact_weight") := .(raking_weight, interview_weight, contact_weight), on = .(ID = id)]
rcvs_panel[is.na(crosssection2021_weight)]; rcvs_panel[is.na(interview_weight)]; rcvs_panel[is.na(contact_weight)]

# Fill-in weights for 2021 year
rcvs_panel[year == 2021, interview_weight := 1]
rcvs_panel[year == 2021, contact_weight := 1]

# Normalized to N size
rcvs_panel[, crosssection2021_weight := crosssection2021_weight/sum(crosssection2021_weight)*.N, by = year]
rcvs_panel[, interview_weight := interview_weight/sum(interview_weight)*.N, by = year]
#rcvs_panel[, interview_with_fe_interviewer_weight := interview_with_fe_interviewer_weight/sum(interview_weight)*.N, by = year]
rcvs_panel[, contact_weight := contact_weight/sum(contact_weight)*.N, by = year]

# Compute attrition weight
rcvs_panel[, attrition_weight := interview_weight*contact_weight]
rcvs_panel[, attrition_weight := attrition_weight/sum(attrition_weight)*.N, by = year]

# Compute general weight
rcvs_panel[, final_weight := crosssection2021_weight * attrition_weight]
rcvs_panel[, final_weight := final_weight/sum(final_weight)*.N, by = year]

# Check results----
rcvs_panel[, .N, by = resp_edu]
educ_dummies <- as.data.table(cbind(rcvs_panel[, dummy_cols(resp_edu, remove_first_dummy = F, ignore_na = T, remove_selected_columns = T)], rcvs_panel[, .(ID, year, final_weight)]))
educ_dummies <- data.table::melt(educ_dummies, id.vars = c("ID", "year", "final_weight"))
educ_dummies[, .(mean = mean(value, na.rm = T), wmean = weighted.mean(value, w = final_weight, na.rm = T)), by = variable]

rcvs_panel[resp_edu == "Высшее и незаконченное высшее", educ := 1]
rcvs_panel[resp_edu %in% c("Полное среднее и ниже", "Среднее спец-ное/техническое или нач-ное профес-ное"), educ := 0]
rcvs_panel[is.na(educ), educ := 0]

setnames(rcvs_panel, c("resp_is_male", "resp_is_crime_victim"), c("male", "victim"))

# DEFF
deff <- rcvs_panel[, .(deff_crosssection = design_effect(crosssection2021_weight), 
                        deff_interview = design_effect(interview_weight),
                        deff_contact = design_effect(contact_weight),
                        deff_attrition = design_effect(attrition_weight),
                        deff_final = design_effect(final_weight)), keyby = .(year)]

# Store function for pooled sd
# (Cochrane's guide https://onlinelibrary.wiley.com/doi/10.1002/cesm.12047)
compute_sd_polled <- function(x, w) { sqrt( ((var(x, na.rm = T)*(length(na.omit(x))-1)) + (wtd.var(x, weights = w, na.rm = T)*(length(na.omit(x))-1))) / (length(na.omit(x))*2-2) ) }
computed_smd <- rcvs_panel[, sapply(.SD, function(x) { list(smd = (mean(x) - weighted.mean(x, w = final_weight)) / compute_sd_polled(x, w = final_weight)) }), .SDcols = c("male", "educ", "victim"), by = year][,-"year"]

# Reasonable variables checking
male <- rcvs_panel[, .(male = mean(male), wmale = weighted.mean(male, w = final_weight)), keyby = .(year)][, -"year"]
educ <- rcvs_panel[, .(higheduc = mean(educ, na.rm = T), whigheduc = weighted.mean(educ, w = final_weight, na.rm = T)), keyby = .(year)][, -"year"]
victim <- rcvs_panel[, .(victim = mean(victim, na.rm = T), wvictim = weighted.mean(victim, w = final_weight, na.rm = T)), keyby = .(year)][, -"year"]

panel_performance <- data.table(deff, male, educ, victim, computed_smd)
panel_performance[, names(panel_performance) := lapply(.SD, round, 3)]
setcolorder(panel_performance, c("year", "deff_crosssection", "deff_interview", "deff_contact", "deff_attrition", "deff_final", "male", "wmale", "male.smd", "higheduc", "whigheduc", "educ.smd", "victim", "wvictim", "victim.smd"))

# Save table
write.xlsx(panel_performance, file = "output/panel_performance.xlsx")

panel_weights <- rcvs_panel[, .(year, ID, crosssection2021_weight, interview_weight, contact_weight, attrition_weight)]
save(panel_weights, file = "data/auxdata/computed_panel_weights_13mar26.rdata", compress = "gzip")