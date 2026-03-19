message("8. Rendering codebooks...")
# Prepare file for manual coding----
# Create intermediate files for future manual translation
# Need to run only once
# all_waves <- as.data.table(readRDS("data/rdd/rcvs_18_21_24_2025-04-16.Rds"))
# all_options <- rbindlist(lapply(all_waves, function(x) {
# 
#   data.table(labels = levels(x),  type = class(x),  n = sum(!is.na(x), na.rm = T), unique_labels = uniqueN(x[!is.na(x)]))
# 
# }), id = "variable", fill = T)
# 
# all_waves_values <- all_options[, .(variable, unique_labels, labels)]
# all_waves_variables <- all_options[, unique(.SD), .SDcols = c("variable", "type")]
# 
# write.xlsx(all_waves_values, "intermediate_files/all_waves_values.xlsx")
# write.xlsx(all_waves_variables, "intermediate_files/all_waves_variables.xlsx")

# Russian codebook----
values_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_values.xlsx"))
variables_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_variables.xlsx"))

# Load key pairs of variable and label
key_pairs <- fread("data/supplementary_data/key_pairs_rdd.csv")
key_pairs <- rbind(key_pairs, data.table(variable = c("deflator2024rub"), label = c("Коэффициент-дефлятор для приведения монетарных показателей к ценам 2024 года")))

# Bind values and variables full names
values_rus2eng <- variables_rus2eng[values_rus2eng, on = .(variable_id = variable)]
# Attach Russian labels
values_rus2eng[key_pairs, "variable_label_rus" := label, on = .(variable_id = variable)]

# Bind codes and levels
values_rus2eng[, full_level := paste0(factor_value, ": ", factor_label_rus)]
values_rus2eng[full_level == "NA: NA", full_level := NA_character_]

# Remove code and level
values_rus2eng[, c("factor_value", "variable_label_eng", "type","factor_label_eng", "factor_label_rus") := NULL]
# Make proper order
setcolorder(values_rus2eng, c("variable_id", "variable_label_rus", "full_level"))

# Attach external variables
values_rus2eng <- values_rus2eng[variable_id != "weight"]
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "weight (cross-section)", variable_label_rus = "Пост-стратификационные веса", full_level = NA_character_))
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "contact_weight (panel)", variable_label_rus = "Величина, обратно пропорциональная вероятности установления контакта (интервьюер успешно дозвонился до потенциального респондента)", full_level = NA_character_))
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "interview_weight (panel)", variable_label_rus = "Величина, обратно пропорциональная вероятности кооперации респондента (вероятность успешного завершения полного интервью)", full_level = NA_character_))
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "attrition_weight (panel)", variable_label_rus = "Комбинированный вес — произведение обратных вероятностей контакта и кооперации респондента", full_level = NA_character_))
# weight of base wave for the panel
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "crosssection2021_weight (panel)", variable_label_rus = "Кросс-секционный вес базовой волны 2021 года", full_level = NA_character_))

# Hide all NA's
values_rus2eng[, names(values_rus2eng) := lapply(.SD, function(x) { fifelse(is.na(x), "", x) })]

# Export codebook
write.csv(values_rus2eng, "data/supplementary_data/codebook_all_waves_rus.csv", row.names = F, fileEncoding = "UTF-8")

# Render codebook of pseudo panel in Russian
render("code/aux_code/codebook_pooled_data_rus.Rmd",
       output_dir = "results/",
       output_file = "codebook_pooled_data_rus.html",
       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())


# English codebook-----
# Incorporate rus2eng mappings into data
values_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_values.xlsx"))
variables_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_variables.xlsx"))

# Bind values and variables full names
values_rus2eng <- variables_rus2eng[values_rus2eng, on = .(variable_id = variable)]

# Bind codes and levels
values_rus2eng[, full_level := paste0(factor_value, ": ", factor_label_eng)]
values_rus2eng[full_level == "NA: NA", full_level := NA_character_]

# Remove code and level
values_rus2eng[, c("factor_value", "type", "factor_label_rus","factor_label_eng") := NULL]

# Hide all NA's
values_rus2eng[, names(values_rus2eng) := lapply(.SD, function(x) { fifelse(is.na(x), "", x) })]
values_rus2eng[variable_id == "deflator2024rub", full_level := NA_character_]

# Attach external variables
# Post-stratification weights
values_rus2eng[values_rus2eng$variable_id == "weight",]$variable_id <- "weight (cross-section)"
# Weights for panel attrition correction
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "contact_weight (panel)", variable_label_eng = "Inverse contact probability (the interviewer successfully contacted a potential respondent)", full_level = NA_character_))
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "interview_weight (panel)", variable_label_eng = "Inverse probability of respondent's cooperation (probability of a successful full interview)", full_level = NA_character_))
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "attrition_weight (panel)", variable_label_eng = "Product of the inverse probabilities of contact and cooperation", full_level = NA_character_))
# Baseline wave weights for the panel
values_rus2eng <- rbind(values_rus2eng, data.table(variable_id = "crosssection2021_weight (panel)", variable_label_eng = "2021 baseline survey cross-sectional weight", full_level = NA_character_))
	
# Export codebook
write.csv(values_rus2eng, "data/supplementary_data/codebook_all_waves_eng.csv", row.names = F, fileEncoding = "UTF-8")

# Render codebook of pseudo panel in English
render("code/aux_code/codebook_pooled_data_eng.Rmd",
       output_dir = "results/",
       output_file = "codebook_pooled_data_eng.html",
       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())


# Variables changes codebooks----
# Render codebook with changes of variables throughout the years
render("code/aux_code/codebook_variables_changes_rus.Rmd",
       output_dir = "results/",
       output_file = "codebook_variables_changes_rus.html",
       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())

render("code/aux_code/codebook_variables_changes_eng.Rmd",
       output_dir = "results/",
       output_file = "codebook_variables_changes_eng.html",
       encoding = "UTF-8", quiet = T, clean = T, envir = new.env())