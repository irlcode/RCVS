message("3. Gather panel...")

# This script harmonizes and merges two waves (2021–2024) of the RCVS longitudinal component. 
# The panel subsample does not include any additional questions. 
# The screening question (Q75: "Please recall whether you were robbed, assaulted, threatened, or were a victim of violence, fraud, 
# or other crimes in Russia during the last 5 years?") has been adjusted to a 3-year period to account for the follow-up survey interval.

# Add year of the wave
rcvs21[, year := 2021] 
cohort24[, year := 2024] 

# Selecting variables' names from all df's for the future manual unification
# var_names21 <- as.data.frame(names(rcvs21))
# var_names24 <- as.data.frame(names(cohort24))
# var_names24 <- rbind(var_names24, '', '', '', '')
# panel_var_names <- cbind(var_names21, var_names24)
# write.csv2(panel_var_names, "./intermediate_files/panel_var_names.csv",row.names = F)

# Talked/seen offender-----
# Unify Q611 and Q6 in 24 to match questions in Q6 18-21
cohort24[, Q6 := fcase(
  Q6 == "Да" | Q611 == "Да", "Да",
  Q6 == "Нет" | Q611 == "Нет", "Нет",
  Q6 == "Затрудняюсь ответить" | Q611 == "Затрудняюсь ответить", "Затрудняюсь ответить",
  default = NA_character_ 
)]

# Initiated criminal or administrative proceedings----
# Rebuild Q46 from old versions to the new one (binary)
# NB. This chunk should be disabled if the 01b_... script is used. 
#rcvs21[, Q46 := fcase(
#  Q46 == "Уголовное дело", "Да",
#  Q46 == "Административное дело", "Да",
#  Q46 == "Не было возбуждено ни уголовного, ни административного дела", "Нет",
#  Q46 == "Затрудняюсь ответить/Не помню", "Затрудняюсь ответить",
#  default = NA_character_ 
#)]

# Rename variables----
## Fix variables where bank and criminals are mixed
setnames(rcvs21, old = "Q47_5", new = "Q47_7")
setnames(rcvs21, old = "Q47_6", new = "Q47_5")
setnames(rcvs21, old = "Q47_7", new = "Q47_6")

## Fix variables where loan and income are mixed
setnames(cohort24, old = "Q571", new = "Q_loan")
setnames(cohort24, old = "Q570", new = "Q571")
setnames(cohort24, old = "Q570_1N", new = "Q571_1N")

# Keep pairs of labels and values for future restoration
attr(cohort24$year, "label") <- "Год"
attr(cohort24$Q6, "label") <- "В момент преступления вы разговаривали со злоумышленником?"
key_pairs_panel <- data.table(variable = names(cohort24), label = label(cohort24))
fwrite(key_pairs_panel, file = "data/supplementary_data/key_pairs_panel.csv")

# Download manually done unification
all_variables <- fread("data/supplementary_data/panels_var_names_fixed.csv")

# Collecting all common variables
vars_bind <- as.vector(c(all_variables[,common], c("Q111", "region_iso")))
# Remove duplicates (Q572, Q81, Q82, Q83, Q662, Q661, respectively)
vars_bind <- vars_bind[!vars_bind %in% c("resp_life_satisfaction", "resp_safety_alone", "resp_safety_home", "resp_safety_victimization", "crime_via_internet", "crime_via_phone")]

# Selecting only those people from 2021 who participated in 2024
dt21_filtered <- rcvs21[ID %in% cohort24$ID]

# Selecting only common variables in each wave
rcvs21_selected <- dt21_filtered[, ..vars_bind]
cohort24_selected <- cohort24[, ..vars_bind]

# Save classes of all variables
check_all_options21 <- suppressWarnings(rbindlist(lapply(rcvs21_selected, function(x) { 
  
  data.table(labels = levels(x),  type = class(x),  n = sum(!is.na(x), na.rm = T), unique_labels = uniqueN(x[!is.na(x)]))
  
}), id = "variable", fill = T))

vars_numeric <- unique(check_all_options21[type == "numeric", variable])
vars_char <- c("ID", "Q5_1T")
vars_factor <- setdiff(vars_bind, union(vars_numeric, vars_char))

# Binding all waves in one df
## Recoding all variables to character type
rcvs21_selected[, names(rcvs21_selected) := lapply(.SD, as.character)]
cohort24_selected[, names(cohort24_selected) := lapply(.SD, as.character)]

panel <- rbind(rcvs21_selected, cohort24_selected)

# Fix variables----
## "Hard to tell" variants unification
panel[, c(vars_factor) := lapply(.SD, function(x) { ifelse(x %in% c("Затрудняюсь ответить / не помню", "Затрудняюсь ответить / не помню", "ЗАТРУДНЯЮСЬ ОТВЕТИТЬ", "Затрудняюсь ответить (НЕ ЗАЧИТЫВАТЬ)", "Затрудняюсь ответить/Не помню", "Затрудняюсь ответить / не помню / не могу сказать", "Затрудняюсь ответить / не знаю", "Затрудняюсь ответить, отказ говорить", "Затрудняюсь,отказ от ответа"), "Затрудняюсь ответить", x) }), .SDcols = vars_factor]

## Fix Q1005
panel[Q1005 == "Архангельская область без Ненецкого автономного округа", Q1005 := "Архангельская область"]
panel[Q1005 == "Город федерального значения Москва", Q1005 := "г. Москва"]
panel[Q1005 == "Город федерального значения Санкт-Петербург", Q1005 := "г. Санкт-Петербург"]
panel[Q1005 == "Город федерального значения Севастополь", Q1005 := "г. Севастополь"]
panel[Q1005 == "Тюменская область без автономных округов", Q1005 := "Тюменская область"]
panel[Q1005 == "Ханты-Мансийский автономный округ - Югра", Q1005 := "Ханты-Мансийский автономный округ-Югра"]
panel[Q1005 == "Республика Адыгея (Адыгея)", Q1005 := "Республика Адыгея"]
panel[Q1005 == "Республика Северная Осетия - Алания", Q1005 := "Республика Северная Осетия-Алания"]
panel[Q1005 == "Чувашская Республика - Чувашия", Q1005 := "Чувашская Республика"]
panel[Q1005 == "Республика Татарстан (Татарстан)", Q1005 := "Республика Татарстан"]

panel[Q1005 == "Отказ", Q1005 := "Нет данных"]
# Harmonized with ISO coding
panel[Q1005 == "Нет данных", region_iso := NA_character_]

## Fix 2003
# Fix some inconsistency
panel[Q1005 == "г. Москва", Q2003 := "города 1 млн и более"]
panel[Q1005 == "г. Санкт-Петербург", Q2003 := "города 1 млн и более"]
panel[Q1005 == "г. Севастополь", Q2003 := "города от 250 тыс. до 1 млн"]

panel[Q2003 == "Москва", Q2003 := "города 1 млн и более"]
panel[Q2003 == "город более 1 млн", Q2003 := "города 1 млн и более"]
panel[Q2003 == "город от 500 тыс до 1 млн", Q2003 := "города от 250 тыс. до 1 млн"]
panel[Q2003 == "город от 250 до 500 тыс", Q2003 := "города от 250 тыс. до 1 млн"]
panel[Q2003 == "город от 100 до 250 тыс", Q2003 := "города от 50 до 250 тыс."]
panel[Q2003 == "город от 50 до 100 тыс", Q2003 := "города от 50 до 250 тыс."]
panel[Q2003 == "город менее 50 тыс", Q2003 := "города менее 50 тыс., ПГТ"]
panel[Q2003 == "пгт", Q2003 := "города менее 50 тыс., ПГТ"]
panel[Q2003 == "село", Q2003 := "сельский населенный пункт"]

## Fix Q661
panel[Q661 == "Да, через телефонный звонок", Q661 := "Да, через телефон"]
panel[Q661 == "Нет, не через телефонный звонок", Q661 := "Нет, не через телефон"]

## Fix Q14
panel[Q14 == "В закрытом учреждении (армия, закрытое училище, больница, исправительная колония и т.п.)", Q14 := "В закрытом учреждении (армия, больница, ИК и т.д.)"]
panel[Q14 == "В подъезде, лифте или во дворе", Q14 := "В подъезде или во дворе"]
panel[Q14 == "На работе или на учёбе (школа, университет, детский сад, техникум)", Q14 := "На работе или на учебе (школа, университет, детский сад, техникум)"]
panel[Q14 == "На природе, в лесу, в парке", Q14 := "На природе, в лесу или парке"]
panel[Q14 == "В общественных зданиях (магазин, кафе, банк, поликлиника)", Q14 := "Общественные здания (магазины, кафе, банки, поликлиники)"]

panel[Q14 == "Затрудняюсь ответить", Q14 := NA_character_]
panel[Q14 == "Другое", Q14 := NA_character_]

## Fix Q15
panel[Q15 == "Тёмное", Q15 := "Темное"]

## Fix Q10
panel[Q10 == "Другое", Q10 := "Другой знакомый"]
panel[offender_is_familiar == 0, Q10 := "Незнакомец"]

## Fix Q68
panel[Q68 == "Дело ещё в процессе", Q68 := "Дело еще в процессе"]

## Fix Q49
panel[Q49 == "Дело ещё в процессе", Q49 := "Дело еще в процессе"]
panel[Q49 == "Нет, виновный не известен", Q49 := "Нет, виновный неизвестен"]

## Fix Q50
panel[Q50 == "Бандитами, криминальными авторитетами", Q50 := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]
panel[Q50 == "Знакомые, близкие, друзья", Q50 := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]
panel[Q50 == "Частные лица (частные охранные структуры)", Q50 := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]

panel[Q50 == "Был известен или сам себя обнаружил", Q50 := "Затрудняюсь ответить / другое"]
panel[Q50 == "Другое", Q50 := "Затрудняюсь ответить / другое"]
panel[Q50 == "Не найден", Q50 := "Затрудняюсь ответить / другое"]
panel[Q50 == "Сдался", Q50 := "Затрудняюсь ответить / другое"]
panel[Q50 == "Затрудняюсь ответить", Q50 := "Затрудняюсь ответить / другое"]

## Fix Q51
panel[Q51 == "Дело ещё в процессе", Q51 := "Дело еще в процессе"]

## Fix Q53
panel[Q53 == "Не добивался / пока еще не добивался", Q53 := "Не добивался"]

## Fix Q69
panel[Q69 == "Другое", Q69 := "Затрудняюсь ответить"]

## Fix Q81
panel[Q81 == "Скорее небезопасно", Q81 := "Скорее не безопасно"]

## Fix Q572
panel[Q572 == "1 — полностью НЕ удовлетворен", Q572 := "1"]
panel[Q572 == "10 — полностью удовлетворен", Q572 := "10"]

## Fix Q78
panel[Q78 == "Да", Q78 := "Один"]
panel[Q78 == "Нет", Q78 := "С кем-то"]

## Fix Q80
panel[Q80 == "Размер домохозяйства (число)", Q80 := "Размер домохозяйства"]

## Fix Q61
panel[Q61 == "Неполное / незаконченное высшее образование (не меньше 3-х лет обучения в вузе)", Q61 := "Неполное/незаконченное высшее образование (не менее 3-х лет обучения в ВУЗе)"]
panel[is.na(Q61), Q61 := "Затрудняюсь ответить"]
panel[Q61 == "Начальное", Q61 := "Начальное или неполное среднее"]

## Fix Q64
panel[Q64 == "Сам работал, или сам работаю, или есть среди знакомых", Q64 := "Сам работал(-ю) или есть среди знакомых"]

## Fix crime_place
panel[crime_place == "В закрытом учреждении (армия, закрытое училище, больница, исправительная колония и т.п.)", crime_place := "В закрытом учреждении (армия, больница, ИК и т.д.)"]
panel[crime_place == "В подъезде, лифте или во дворе", crime_place := "В подъезде или во дворе"]
panel[crime_place == "На работе или на учёбе (школа, университет, детский сад, техникум)", crime_place := "На работе или на учебе (школа, университет, детский сад, техникум)"]
panel[crime_place == "На природе, в лесу, в парке", crime_place := "На природе, в лесу или парке"]
panel[crime_place == "В общественных зданиях (магазин, кафе, банк, поликлиника)", crime_place := "Общественные здания (магазины, кафе, банки, поликлиники)"]

panel[crime_place == "Затрудняюсь ответить", crime_place := NA_character_]
panel[crime_place == "Другое", crime_place := NA_character_]

## Fix crime_place_grouped
panel[crime_place_grouped == "В подъезде, лифте или во дворе", crime_place_grouped := "В подъезде или во дворе"]
panel[crime_place_grouped == "В общественных зданиях (магазин, кафе, банк, поликлиника)", crime_place_grouped := "Общественные здания (магазины, кафе, банки, поликлиники)"]

panel[crime_place_grouped == "Затрудняюсь ответить", crime_place_grouped := NA_character_]
panel[crime_place_grouped == "Другое", crime_place_grouped := NA_character_]

## Fix victim_who_found_offender
panel[victim_who_found_offender == "Бандитами, криминальными авторитетами", victim_who_found_offender := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]
panel[victim_who_found_offender == "Знакомые, близкие, друзья", victim_who_found_offender := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]
panel[victim_who_found_offender == "Частные лица (частные охранные структуры)", victim_who_found_offender := "Знакомые, близкие, друзья, частные лица (частные охранные структуры), бандиты, криминальные авторитеты"]

panel[victim_who_found_offender == "Был известен или сам себя обнаружил", victim_who_found_offender := "Затрудняюсь ответить / другое"]
panel[victim_who_found_offender == "Другое", victim_who_found_offender := "Затрудняюсь ответить / другое"]
panel[victim_who_found_offender == "Не найден", victim_who_found_offender := "Затрудняюсь ответить / другое"]
panel[victim_who_found_offender == "Сдался", victim_who_found_offender := "Затрудняюсь ответить / другое"]
panel[victim_who_found_offender == "Затрудняюсь ответить", victim_who_found_offender := "Затрудняюсь ответить / другое"]

## Recoding all variables to the original type
panel[, (vars_numeric) := lapply(.SD, as.numeric), .SDcols = vars_numeric]
panel[, (vars_factor) := lapply(.SD, as.factor), .SDcols = vars_factor]

# Set orders of factor variables----

## Set orders of yes/no/hard to tell variables
cols_yes_no <- c("Q75", "Q76", "Q18", "Q17", "Q22", "Q23", "Q24", "Q30", "Q27", "Q28", "Q31", "Q33", "Q34", "Q32", "Q6", "Q9", "Q11", "Q13", "Q39", "Q40", "Q46", "Q52", "Q54", "Q58", "Q69", "Q70", "Q71", "Q72", "Q73", "Q65")
panel[, c(cols_yes_no) := lapply(.SD, function(x) { factor(x, levels = c("Да", "Нет", "Затрудняюсь ответить")) }), .SDcols = cols_yes_no]

## Q26
panel[, Q26 := factor(Q26, levels = c("Нет", "Да, синяки или ссадины", "Да, повреждения серьёзнее, чем синяки или ссадины"))]

## Q2001
panel[, Q2001 := factor(Q2001, levels = c("Центральный ФО", "Северо-Западный ФО","Приволжский ФО", "Южный ФО", "Северо-Кавказский ФО", "Уральский ФО","Сибирский ФО", "Дальневосточный ФО", "Не определён"))]

## Q2003
panel[, Q2003 := factor(Q2003, levels = c("города 1 млн и более", "города от 250 тыс. до 1 млн", "города от 50 до 250 тыс.", "города менее 50 тыс., ПГТ", "сельский населенный пункт", "нет данных"))]

## Q661
panel[, Q661 := factor(Q661, levels = c("Да, через телефон", "Нет, не через телефон", "Затрудняюсь ответить"))]

## Q662
panel[, Q662 := factor(Q662, levels = c("Да, через интернет", "Нет, не через интернет", "Затрудняюсь ответить"))]

## Q15
panel[, Q15 := factor(Q15, levels = c("Светлое", "Темное", "Затрудняюсь ответить"))]

## Q16
panel[, Q16 := factor(Q16, levels = c("Январь", "Февраль", "Март", "Апрель", "Май", "Июнь", "Июль", "Август", "Сентябрь", "Октябрь", "Ноябрь", "Декабрь", "Затрудняюсь ответить"))]

## Q77
panel[, Q77 := factor(Q77, levels = c("Зима", "Весна", "Лето", "Осень", "Затрудняюсь ответить"))]

## Q221
panel[, Q221 := factor(Q221, levels = c("Повреждено/уничтожено", "Имуществом завладел другой человек", "Затрудняюсь ответить"))]

## Q7
panel[, Q7 := factor(Q7, levels = c("Один", "Несколько", "Затрудняюсь ответить"))]

# Q8
panel[, Q8 := factor(Q8, levels = c("Мужчина", "Женщина", "Затрудняюсь ответить"))]

## Q68
panel[, Q68 := factor(Q68, levels = c("Да", "Нет", "Дело еще в процессе", "Затрудняюсь ответить"))]

## Q49
panel[, Q49 := factor(Q49, levels = c("Да, виновный известен", "Нет, виновный неизвестен", "Дело еще в процессе", "Затрудняюсь ответить"))]

## Q51
panel[, Q51 := factor(Q51, levels = c("Да", "Нет", "Дело еще в процессе", "Затрудняюсь ответить"))]

## Q53
panel[, Q53 := factor(Q53, levels = c("Не добивался", 
                                       "Добивался, но ничего не удалось",
                                       "Добился частичной",
                                       "Добился полной компенсации",
                                       "Затрудняюсь ответить"))]

## Q81
panel[, Q81 := factor(Q81, levels = c("В полной безопасности", 
                                       "В относительной безопасности",
                                       "Скорее не безопасно",
                                       "Совсем не безопасно",
                                       "Затрудняюсь ответить"))]

## Q82
panel[, Q82 := factor(Q82, levels = c("Постоянно или почти всегда", 
                                       "Довольно часто",
                                       "Иногда, временами",
                                       "Никогда",
                                       "Затрудняюсь ответить"))]

## Q83
panel[, Q83 := factor(Q83, levels = c("Постоянно или почти всегда", 
                                       "Довольно часто",
                                       "Иногда, временами",
                                       "Никогда",
                                       "Затрудняюсь ответить"))]

## Q572
panel[, Q572 := factor(Q572, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "Затрудняюсь ответить"))]


## Q57
panel[, Q57 := factor(Q57, levels = c("Едва сводим концы с концами, денег не хватает на продукты",
                                       "На продукты хватает, на одежду нет",
                                       "На продукты и одежду хватает, на технику и мебель нет",
                                       "На технику и мебель хватает, на большее денег нет",
                                       "Можем позволить автомобиль, но квартиру или дачу нет",
                                       "Можем позволить себе практически все: квартиру и т.д.",
                                       "Затрудняюсь ответить",
                                       "Отказ от ответа"))]

## Q78
panel[, Q78 := factor(Q78, levels = c("Один", "С кем-то", "Затрудняюсь ответить"))]

## Q79
panel[, Q79 := factor(Q79, levels = c("Да, официальный супруг(-а)", "Да, гражданский супруг(-а)", "Нет", "Затрудняюсь ответить"))]

## Q64
panel[, Q64 := factor(Q64, levels = c("Нет", "Сам работал(-ю) или есть среди знакомых", "Затрудняюсь ответить"))]

## victim_compensation
panel[, victim_compensation := factor(victim_compensation, levels = c("Не добивался", "Добивался, но ничего не удалось", "Добился частичной", "Добился полной компенсации", "Затрудняюсь ответить"))]

## Q61
panel[, Q61 := factor(Q61, levels = c("Начальное или неполное среднее", "Полное среднее", "Среднее специальное, среднее техническое или начальное профессиональное", "Неполное/незаконченное высшее образование (не менее 3-х лет обучения в ВУЗе)", "Высшее", "Затрудняюсь ответить"))]

## resp_edu
panel[, resp_edu := factor(resp_edu, levels = c("Полное среднее и ниже", "Среднее спец-ное/техническое или нач-ное профес-ное", "Высшее и незаконченное высшее"))]

## Q47 fix some problems
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_1 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_2 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_3 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_4 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_5 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_6 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_888 := NA_integer_]
panel[year == 2021 & crime_type %in% c("Удаленное преступление", "Покушение на удаленное преступление"), Q47_999 := NA_integer_]

# Handle infinity result
panel[is.infinite(victim_damage_rub_lg), victim_damage_rub_lg := 0]

# Check results-----
# Compare all variable-value percentage
#panel_molten <- data.table::melt(panel, id.vars = c("ID", "year"), na.rm = F, value.factor = F, )
#panel_molten <- panel_molten[!variable %in% c("IVDur", "Q5_1T", "Q75_1N", "Q76_1N", "Q2", "Q21_1N", "Q80_1N", "victim_damage_rub", "victim_damage_rub", "victim_damage_rub_lg")]
#panel_molten <- panel_molten[, .(n = .N), by = .(year, variable, value)]
#panel_molten[, percent := round(n/sum(n)*100, 1), by = .(year, variable)]
#panel_molten <- dcast(panel_molten, variable + value ~ year, value.var = "percent")