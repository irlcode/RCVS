message("7. Prepare english version...")

# Create temporary files for manual recoding
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

# Incorporate rus2eng mappings into data
values_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_values.xlsx"))
variables_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/all_waves_variables.xlsx"))

# Factorize variable
# Turn-on translate region names
all_waves[, Q1005 := as.factor(Q1005)]
# Turn-off translation for ISO
all_waves[, region_iso := as.character(region_iso)]

## Prepare translation of cross-sections variables
crosssection_variables2translate <- colnames(all_waves)

for(column in crosssection_variables2translate) {
  
  # Translate variable labels 
  eval(parse(text = paste0("attr(all_waves$",column,", 'label') <- variables_rus2eng[variable_id == '",column,"']$variable_label_eng")))

  # Translate categories 
  isfactor <- eval(parse(text = paste0("is.factor(all_waves$",column,")")))
  if(isfactor == T) { 
    
      # Categories of factor from data (in Russian)
      values_in_data <- all_waves[, .(factor_label_rus = levels(get(column)))]
      # Get manually prepared categories (in English)
      translated_values <- values_rus2eng[variable == column, c("factor_label_eng", "factor_label_rus")]
      
      # Mapping
      mapping <- merge(values_in_data, translated_values, by = "factor_label_rus", sort = F, all.x = T)
      names(mapping) <- c("rus", "eng")
      
      # Attach to RCVS
      eval(parse(text = paste0("all_waves[, ",column," := factor(",column,", mapping$rus, mapping$eng)]")))
      # Restore label
      eval(parse(text = paste0("attr(all_waves$",column,", 'label') <- variables_rus2eng[variable_id == '",column,"']$variable_label_eng")))
      
  }
}

## Prepare translation of panel variables
panel_values_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/panels_values.xlsx"))
panel_variables_rus2eng <- as.data.table(read.xlsx("data/supplementary_data/english_version/panels_variables.xlsx"))

panel_variables2translate <- colnames(panel)

# Turn-off translation for ISO
panel[, region_iso := as.character(region_iso)]
# Turn-on translate region names
panel[, Q1005 := as.factor(Q1005)]

for(column in panel_variables2translate) {
  
  # Translate variable labels 
  eval(parse(text = paste0("attr(panel$",column,", 'label') <- panel_variables_rus2eng[variable_id == '",column,"']$variable_label_eng")))
  
  # Translate categories 
  isfactor <- eval(parse(text = paste0("is.factor(panel$",column,")")))
  if(isfactor == T) { 
    
    # Categories of factor from data (in Russian)
    values_in_data <- panel[, .(factor_label_rus = levels(get(column)))]
    # Get manually prepared categories (in English)
    translated_values <- panel_values_rus2eng[variable == column, c("factor_label_eng", "factor_label_rus")]
    
    # Mapping
    mapping <- merge(values_in_data, translated_values, by = "factor_label_rus", sort = F, all.x = T)
    names(mapping) <- c("rus", "eng")
    
    # Attach to RCVS
    eval(parse(text = paste0("panel[, ",column," := factor(",column,", mapping$rus, mapping$eng)]")))
    # Restore label
    eval(parse(text = paste0("attr(panel$",column,", 'label') <- variables_rus2eng[variable_id == '",column,"']$variable_label_eng")))
    
  }
}

# Save----
# Cross-sections
# R
saveRDS(all_waves, paste0("results/rcvs_18_21_24_",Sys.Date(),"_eng.Rds"), compress = "gzip")
# SPSS
write_sav(all_waves, paste0("results/rcvs_18_21_24_",Sys.Date(),"_eng.sav"), compress = F)
# csv
fwrite(all_waves, file =  paste0("results/rcvs_18_21_24_",Sys.Date(),"_eng.csv"))

# Panel
# R
saveRDS(panel, paste0("results/rcvs_panel_21_24_",Sys.Date(),"_eng.Rds"), compress = "gzip")
# SPSS
write_sav(panel, paste0("results/rcvs_panel_21_24_",Sys.Date(),"_eng.sav"), compress = F)
# csv
fwrite(panel, file =  paste0("results/rcvs_panel_21_24_",Sys.Date(),"_eng.csv"))