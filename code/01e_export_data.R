message("6. Export data...")

# Restore labels of variables
key_pairs_rdd <- fread("data/supplementary_data/key_pairs_rdd.csv")
key_pairs_rdd <- rbind(key_pairs_rdd, data.table(variable = c("deflator2024rub"), label = c("Коэффициент-дефлятор для приведения монетарных показателей к ценам 2024 года")))
for(column in names(all_waves)) {
  
  eval(parse(text = paste0("attr(all_waves$",column,", 'label') <- key_pairs_rdd[variable == '",column,"']$label")))
  
}

key_pairs_panel <- fread("data/supplementary_data/key_pairs_panel.csv")
key_pairs_panel <- rbind(key_pairs_panel, data.table(variable = c("deflator2024rub", "crosssection2021_weight"), label = c("Коэффициент-дефлятор для приведения монетарных показателей к ценам 2024 года", "Кросс-секционный вес базовой волны опроса 2021 года")))
for(column in names(panel)) {
  
  eval(parse(text = paste0("attr(panel$",column,", 'label') <- key_pairs_panel[variable == '",column,"']$label")))
  
}

# Save----
# Cross-sections
# R
saveRDS(all_waves, paste0("results/rcvs_18_21_24_",Sys.Date(),".Rds"), compress = "gzip")
# SPSS
write_sav(all_waves, paste0("results/rcvs_18_21_24_",Sys.Date(),".sav"), compress = F)
# csv
fwrite(all_waves, file =  paste0("results/rcvs_18_21_24_",Sys.Date(),".csv"))

# Panel
# R
saveRDS(panel, paste0("results/rcvs_panel_21_24_",Sys.Date(),".Rds"), compress = "gzip")
# SPSS
write_sav(panel, paste0("results/rcvs_panel_21_24_",Sys.Date(),".sav"), compress = F)
# csv
fwrite(panel, file =  paste0("results/rcvs_panel_21_24_",Sys.Date(),".csv"))