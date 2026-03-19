library(data.table)
library(lubridate)

# In this script, we process the final case disposition codes.
# In CATI data collection, every contact attempt results in a specific outcome.
# On average, a completed interview requires multiple calls. There are more than 
# 20 distinct technical status codes (https://kb.survey-studio.com/help/calls/7022/), 
# following the AAPOR standard (https://aapor.org/wp-content/uploads/2023/05/Standards-Definitions-10th-edition.pdf).

# We perform three main tasks here:
# 1) Group technical call statuses into broader categories for ease of analysis.
# 2) Rank all attempts by informativeness, from the most definitive outcome 
#    (Complete Interview = 1) to the least define (No Contact = 9).
# 3) Assign the most informative status (the minimum value) as the final 
#    disposition code for each respondent based on all their call attempts.

# Load metadata of all calls
call1 <- fread("data/rdd/calls/calls_424twquw.csv", na.strings = "", select = c("Дата / Время", "Телефон", "Звонок", "Результат", "Инициатор отключения", "Запись разговора", "Интервью"))
call2 <- fread("data/rdd/calls/calls_ktybmsat.csv", na.strings = "", select = c("Дата / Время", "Телефон", "Звонок", "Результат", "Инициатор отключения", "Запись разговора", "Интервью"))

calls <- rbind(call1, call2)
rm(call1, call2); gc()

# Rename variables
names(calls) <- c("date", "phone", "duration", "result", "initiator", "record", "interview")
calls[, date := dmy_hms(date)]

# Count all unique phones after the filleting of Saturn
calls[, uniqueN(phone)] # 7418814

# Unify status
# Status: https://kb.survey-studio.com/help/calls/7022/
calls[, result_unified := NA_character_]

calls[result == "Успешно", result_unified := "Успешно"]
calls[result == "Не подходит", result_unified := "Не подходит"]
calls[result == "Отказ", result_unified := "Отказ"]
calls[result == "Сброс на предобработке", result_unified := "Отказ на стадии рекрута"]

# Find all cases without audio records
calls[is.na(record), result_unified := "Не дозвонились"]
# Add DEX working
calls[result %in% c("Автоответчик (Определил оператор)", "Автоответчик (Интеллектуальный робот)"), result_unified := "Не дозвонились"]
calls[result %in% c("Неверный номер"), result_unified := "Не дозвонились"]

calls[result %in% c("Нет операторов", "Ошибка перевода"), result_unified := "Ошибка соединения со стороны оператора"]

calls[result %in% c("Ошибка связи", "Ошибка обработки", "Ошибка вызова", "Квота"), result_unified := "Ошибка"]

calls[result %in% c("Перенос", "Перезвон"), result_unified := "Перезвон"]

calls[is.na(result_unified) & !is.na(result), result_unified := result]

# Status -> scale
calls[, scale := NA_integer_]
calls[result_unified == "Успешно", scale := 1]
calls[result_unified == "Отказ", scale := 2]
calls[result_unified == "Не подходит", scale := 3]

calls[result_unified == "Прервано", scale := 4]
calls[result_unified == "Перезвон", scale := 5]

calls[result_unified == "Отказ на стадии рекрута", scale := 6]

calls[result_unified == "Ошибка", scale := 7]
calls[result_unified == "Ошибка соединения со стороны оператора", scale := 8]
calls[result_unified == "Не дозвонились", scale := 9]

# Collapse: select from all the tries the one with the maximum result for each respondent
calls_wide <- calls[, .SD[which.min(scale)], by = phone]