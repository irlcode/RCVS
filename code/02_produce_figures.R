message("9. Producing figures...")
# This script generates Figure 2 (Сonstruct validity), Figure 3 (Victimization Level) and Figure 4 (Crime Type),
# which present key findings from the RCVS data descriptor preprint.

# Weighted means
weighted.se <- function(x, w) { sqrt( (wtd.var(x, na.rm = T, weights = w)) / sum(w)) }

# Data preparation----
# Load RCVS RDD 2018-2024
file_path <- paste0("results/rcvs_18_21_24_", Sys.Date(), ".Rds")
rcvs <- as.data.table(readRDS(file_path))

# Victim status
rcvs[Q75 == "Да", victim5year := 1]
rcvs[Q75 == "Нет", victim5year := 0]

rcvs[Q76 == "Да", victim12m := 1]
rcvs[Q76 == "Нет", victim12m := 0]

# (Q24) Было ли к вам применено физическое насилие? 
# (Q21) Какова была примерная сумма материального ущерба в рублях?
rcvs[, crimes_with_crim_features_12m := 0]
rcvs[, crimes_with_crim_features_5year := 0]
rcvs[victim12m == 1 & (Q24 == "Да" | (Q21 == "Названо число" & Q21_1N > 0 ) | Q21 == "Затрудняюсь ответить / не знаю"), crimes_with_crim_features_12m := 1]
rcvs[victim5year == 1 & (Q24 == "Да" | (Q21 == "Названо число" & Q21_1N > 0 ) | Q21 == "Затрудняюсь ответить / не знаю"), crimes_with_crim_features_5year := 1]

# Crime type KOUZ----
# Load KOUZ
load("data/auxdata/kouzh_victim_24.rdata")
# Normalized weight for kouz (We remove all cases that are under 18)
kouzh_victim_24[, survey_weight := as.numeric(survey_weight)]
kouzh_victim_24[, survey_weight := survey_weight / sum(survey_weight) * .N]

kouzh_victim_24[, id := .GRP, by = .(household_id, individual_id)]

# Check general victimization lvl - 3.7
kouzh_victim_24[, .(mean = round(weighted.mean(victim12m, w = survey_weight)*100,3), se = round(weighted.se(victim12m, w = survey_weight)*100*1.96,3))]

kouzh_victim_24[, theft := Кража]
kouzh_victim_24[, fraud := Мошенничество]

kouzh_victim_24[, assault := 0]
kouzh_victim_24[Хулиганство == 1 | Конфликты_в_быту == 1 | Нападение_или_угроза == 1 | Причинение_вреда_здоровью == 1, assault := 1]

kouzh_victim_24[, robbery := Ограбление]

# Not appropriate for exteranal source of validation
crimetype_kouz <- kouzh_victim_24[, lapply(.SD, function(x) { weighted.mean(x, w = survey_weight, na.rm = T)*100 }), .SDcols = c("theft", "fraud", "assault", "robbery")]

# Official registered crime data from the Ministry of Internal Affairs.
# The State of Crime in Russia (Состояние преступности в России), 2024: https://%D0%BC%D0%B2%D0%B4.%D1%80%D1%84/reports/item/60248328/
# The State of Crime in Russia (Состояние преступности в России), 2018: https://%D0%BC%D0%B2%D0%B4.%D1%80%D1%84/reports/item/16053092/
# The State of Crime in Russia (Состояние преступности в России), 2022: https://%D0%BC%D0%B2%D0%B4.%D1%80%D1%84/reports/item/28021552/
egs4 <- as.data.table(read.xlsx("data/auxdata/official_crime_rate.xlsx"))
egs4 <- egs4[X1 %in% c("robbery", "theft", "fraud", "cybercime", "Всего")]

pop = data.table(year = c(2018, 2021, 2024), pop = c(145197002, 148470676, 148737698))

egs4 <- melt(egs4, id.vars = "X1", variable.name = "year", value.name = "crime")
egs4[, year := as.integer(as.character(year))]
egs4 <- merge(egs4, pop, by = "year")

# Calculate rate of changes for official statistics
egs4sum <- egs4[X1 == "Всего"] 
egs4sum[, lcrime := shift(crime, n = 1, type = "lag")]
egs4sum[, delta := (crime-lcrime)/lcrime*100]

egs4lvl <- egs4[X1 == "Всего"] 
egs4lvl[, mean := crime/pop*100]
egs4lvl[, variable := "victim12m"]

# Victimization prevalence----
rcvs[, .(victim5year = mean(victim5year, na.rm = T), victim12m = mean(victim12m, na.rm = T)), keyby = year]
#     year victim5year  victim12m
#<num>       <num>      <num>
#1:  2018   0.1788758 0.07664842
#2:  2021   0.1952026 0.10939350
#3:  2024   0.2222821 0.12927046

prevalence <- rcvs[, .(victim5year = weighted.mean(victim5year, na.rm = T, w = weight),
                       victim5year_me95 = weighted.se(victim5year, w = weight)*1.96,
                       victim12m = weighted.mean(victim12m, na.rm = T, w = weight),
                       victim12m_me95 = weighted.se(victim12m, w = weight)*1.96), keyby = year]

#    year victim5year victim5year_me95  victim12m victim12m_me95
#<num>       <num>            <num>      <num>          <num>
#1:  2018   0.1937287      0.005973371 0.08254128    0.004159208
#2:  2021   0.1910506      0.006414429 0.10606120    0.005024064
#3:  2024   0.2116573      0.007524349 0.12110268    0.006009531

prevalence_mean <- melt(prevalence, id.vars = "year", measure.vars = c("victim5year", "victim12m"), value.name = "mean")
prevalence_me <- melt(prevalence, id.vars = "year", measure.vars = c("victim5year_me95", "victim12m_me95"), value.name = "me")
prevalence_me[, variable := gsub("_me95", "", variable)]
prevalence <- merge(prevalence_mean, prevalence_me, by = c("year", "variable"))

prevalence_plot <- ggplot(prevalence, aes(year, mean, linetype = variable, label = mean)) + 
  geom_point(color = "firebrick2") +
  geom_line(color = "firebrick2") +
  geom_text_repel(aes(label = paste0(round(mean*100,1), "%")), size = 4, show.legend = F, nudge_y = 0.01) +
  geom_ribbon(aes(ymin = mean-me, ymax=mean+me), alpha = 0.2, size = 0.1) +
  scale_y_continuous(name = NULL, limits = c(0, 0.25), labels = function(x) paste0(x*100, "%"), expand = c(0,0.005)) +
  scale_x_continuous(name = NULL, breaks = c(2018, 2021, 2024)) +
  scale_linetype_manual(name = "Respondent has been victimized during the last:", 
                        values = c("solid", "dotted"),
                        labels = c("12 months", "5 years")) +
  ggtitle("(a) All victims (red is RCVS)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 11))

# Calculate victim rate of RCVS without cybercrime
rcvs[!crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), .(victim12m = round(weighted.mean(crimes_with_crim_features_12m, na.rm = T, w = weight)*100,3),
                                                                                            victim12m_me95 = round(weighted.se(crimes_with_crim_features_12m, w = weight)*1.96*100,3)), keyby = year]

# The same but with respect to real damage
prevalence_realdamage <- rcvs[, .(victim5year = weighted.mean(crimes_with_crim_features_5year, na.rm = T, w = weight),
                       victim5year_me95 = weighted.se(crimes_with_crim_features_5year, w = weight)*1.96,
                       victim12m = weighted.mean(crimes_with_crim_features_12m, na.rm = T, w = weight),
                       victim12m_me95 = weighted.se(crimes_with_crim_features_12m, w = weight)*1.96), keyby = year]

prevalence_realdamage_mean <- melt(prevalence_realdamage, id.vars = "year", measure.vars = c("victim5year", "victim12m"), value.name = "mean")
prevalence_realdamage_me <- melt(prevalence_realdamage, id.vars = "year", measure.vars = c("victim5year_me95", "victim12m_me95"), value.name = "me")
prevalence_realdamage_me[, variable := gsub("_me95", "", variable)]
prevalence_realdamage <- merge(prevalence_realdamage_mean, prevalence_realdamage_me, by = c("year", "variable"))

prevalence_realdamage_plot <- ggplot(prevalence_realdamage, aes(year, mean, linetype = variable, label = mean)) + 
  geom_point(color = "firebrick2") +
  
  geom_line(data = egs4lvl, aes(x = year, y = mean/100, color = "Reported")) +
  geom_point(data = egs4lvl, aes(x = year, y = mean/100, color = "Reported")) +
  geom_text_repel(data = egs4lvl, aes(x = year, y = mean/100, label = paste0(round(mean,1), "%")), size = 4, show.legend = F, nudge_y = -0.01) +
  
  geom_point(aes(x = 2024, y = 3.7/100, color = "CMLC")) +
  geom_text_repel(data = data.table(x = 2024, y = 3.7/100, variable = "victim12m"), aes(x = x, y = y, label = paste0("CMLC: ", y*100, "%")), size = 4, show.legend = F, nudge_y = 0.01) +

  geom_line(color = "firebrick2") +
  geom_text_repel(aes(label = paste0(round(mean*100,1), "%")), size = 4, show.legend = F, nudge_y = 0.01) +
  geom_ribbon(aes(ymin = mean-me, ymax=mean+me), alpha = 0.2, size = 0.1) +
  scale_y_continuous(name = NULL, limits = c(0, 0.25), labels = function(x) paste0(x*100, "%"), expand = c(0,0.005)) +
  scale_x_continuous(name = NULL, breaks = c(2018, 2021, 2024)) +
  scale_linetype_manual(name = "Respondent has been victimized during the last:", 
                        values = c("solid", "dotted"),
                        labels = c("12 months", "5 years")) +
  ggtitle("(b) Victims with physical or property damage") +
  
  scale_color_manual(labels = c("blue is CMLC", " black is police data"), values = c("royalblue", "black"), name = "12 mos.:") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.width = unit(1.5, "cm"),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 11))

get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")
  legend <- plot_table$grobs[[legend_plot]]
  return(legend)
}

common_legend <- get_only_legend(prevalence_realdamage_plot)

# Save results
prevalence_plots <- grid.arrange(arrangeGrob(prevalence_plot + theme(legend.position = "none"), prevalence_realdamage_plot + theme(legend.position = "none"), nrow = 1), common_legend, nrow = 2, heights= c(10,1))

ggsave(prevalence_plots, file = "figures/prevalence_plot.png", dpi = 300, width = 10, height = 6, bg = "white")

  

# RCVS crime types (last 12 months)----
crimetype_obj <- dummy_cols(rcvs[, .(ID, year, crime_type)], select_columns = "crime_type", remove_first_dummy = F, remove_most_frequent_dummy = F, ignore_na = T, remove_selected_columns = T)
crimetype_obj[, names(crimetype_obj) := lapply(.SD, function(x) { ifelse(is.na(x), 0, x) })]
crimetype_obj[, c("crime_type_Прочее", "crime_type_Недостаточно информации") := NULL]

crimetype_molten <- melt(crimetype_obj, id.vars = c("ID", "year"))
crimetype_molten[rcvs, c("weight", "victim12m") := .(i.weight, i.victim12m), on = .(ID, year)]

crimetype_molten[value == 1 & victim12m == 0, value := 0]
crimetype_molten[is.na(victim12m) & value == 1, value := NA_real_]

crimetype <- crimetype_molten[, .(mean = weighted.mean(value, w = weight, na.rm = T),  me = weighted.se(value, w = weight)*1.96), by = .(year, variable)]

crimetype[, variable := as.character(variable)]
crimetype[, variable := factor(variable, levels = c("crime_type_Нападение",
                                                    "crime_type_Грабеж и разбой", 
                                                    "crime_type_Кража", 
                                                    "crime_type_Мошенничество", 
                                                    "crime_type_Покушение на киберпреступление", 
                                                    "crime_type_Киберпреступление"), 
                               labels = c("Assault", "Robbery", "Theft", "Fraud", "Attempt of cybercrime", "Cybercrime"))]

crimetype_plot <- ggplot(crimetype, aes(year, mean, label = mean)) +
  geom_point(color = "firebrick2") +
  geom_line(color = "firebrick2") +
  geom_text_repel(aes(label = paste0(round(mean*100,1), "%")), size = 4, show.legend = F, nudge_y = 0.005) +
  geom_ribbon(aes(ymin = mean-me, ymax=mean+me), alpha = 0.2, size = 0.1) +
  facet_wrap(~variable, nrow = 3) +
  scale_x_continuous(name = NULL, breaks = c(2018, 2021, 2024)) +
  scale_y_continuous(name = NULL, limits = c(0, 0.054), breaks = c(1:5/100), labels = function(x) paste0(x*100, "%"), expand = c(0,0.005)) +
  theme_minimal() +
  theme(legend.key.width = unit(2, "cm"),
        panel.grid.minor = element_blank(),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 13))

ggsave(crimetype_plot, file = "figures/crimetype_plot.png", dpi = 300, width = 9, height = 9, bg = "white")
#ggsave(crimetype_plot, file = "results/crimetype_plot.pdf", dpi = 300, width = 9, height = 9, device = pdf)


# Check construct validity
dtyears <- rcvs[!is.na(Age_Groups), .(victim_lvl = weighted.mean(victim12m, na.rm = T, w = weight), me = weighted.se(victim12m, w = weight)*1.96), by = .(year, Age_Groups)][order(Age_Groups)]
dtyears[, year := as.factor(year)]

age_plot <- ggplot(dtyears, aes(Age_Groups, victim_lvl, col = year, group = year)) +
  geom_point(size = 2) +
  geom_line() +
  geom_errorbar(aes(ymin = victim_lvl-me, ymax=victim_lvl+me), width = 0.07) +
  scale_x_discrete(name = "Age 18 to 65+", 
                   expand = c(0,0.1)) +
  scale_y_continuous(name = "Prevalence level, %", 
                     limits = c(0,0.22), 
                     expand = c(0,0), 
                     labels = function(x) { paste0(x*100, "%") }) +
  scale_color_manual(values = c("#4daf4a", "#377eb8", "#e41a1c"), name = "Wave: ") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        legend.position = "bottom", 
        legend.key.width = unit(2, "cm"),
        text = element_text(size = 11),
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 13))
  
ggsave(age_plot, file = "figures/age_plot.png", dpi = 300, width = 8, height = 5, bg = "white")