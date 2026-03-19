library(data.table)
library(openxlsx)
library(stringi)
library(stringr)

# This script processes Rosstat yearbooks to obtain data at the following granularity: 
# region — year — age group (single-year increments) — sex. 
# These data are required for calculating post-stratification counts (targets). 
# The output is available at: data/auxdata/region_sex_age_yearly_population_2018_2024.csv

# Generate list names in XLSX
temp <- CJ(okrug = 1:8, region = 1:18)
temp[, workbook_name := paste0("2.", okrug, ".", region, ".")]
workbook_names <- unique(temp$workbook_name)

# Align with the data
# from https://rosstat.gov.ru/compendium/document/13284
workbook_actual_names <- unique(c(getSheetNames("data/auxdata/Бюллетень_2018.xlsx"), getSheetNames("data/auxdata/Бюллетень_2021.xlsx"), getSheetNames("data/auxdata/Бюллетень_2024.xlsx")))
workbook_names <- workbook_names[workbook_names %in% workbook_actual_names]

# Manually attach
workbook_names <- unique(c(workbook_names, "2.7.11.", "2.7.12.", "2.8.9."))

# Init an object to store the results
region_sex_age_yearly <- data.table()

for(w in workbook_names) {

	# w <- workbook_names[1]

	# Load 2018 data
  pop_2018 <- data.table()
	tryCatch({

		sheet_name <- paste0("Таб.", substr(w, 1, nchar(w)-1))

		if(sheet_name %in% getSheetNames("data/auxdata/Бюллетень_2018.xlsx")) {

			pop_2018 <- as.data.table(read.xlsx("data/auxdata/Бюллетень_2018.xlsx", sheet = sheet_name))

			# Fix Chukotka error
			if(w == "2.8.9.") {

				pop_2018 <- pop_2018[, -1]
			}
			# Columns and vars of interest
			region_name <- as.character(pop_2018[2,1])

			pop_2018 <- pop_2018[, c(1, 3, 4)]
			setnames(pop_2018, c("age", "male", "female"))

			pop_2018 <- pop_2018[age %in% c(1:100, c("80 лет и более"))]
			# 80 лет и более
			# 85 лет и более
			pop_2018[age == "80 лет и более", age := "80"]
			pop_2018[, age := as.integer(age)]

			# Assign regions and years
			pop_2018[, region := region_name]
			pop_2018[, year := 2018]

		}
	}, error = function(e) {})

	# Load 2021 data
  pop_2021 <- data.table()
	tryCatch({

		if(w %in% getSheetNames("data/auxdata/Бюллетень_2021.xlsx")) {

			pop_2021 <- as.data.table(read.xlsx("data/auxdata/Бюллетень_2021.xlsx", sheet = w))

			# Columns and vars of interest
			region_name <- as.character(pop_2021[1,1])

			pop_2021 <- pop_2021[, c(1, 3, 4)]
			setnames(pop_2021, c("age", "male", "female"))

			pop_2021 <- pop_2021[age %in% c(1:100, c("80 и старше"))]
			pop_2021[age == "80 и старше", age := "80"]
			pop_2021[, age := as.integer(age) ]

			# Assign regions and years
			pop_2021[, region := region_name]
			pop_2021[, year := 2021]

		}

	}, error = function(e) {})

	# Load 2024 data
  pop_2024 <- data.table()
	tryCatch({

		if(w %in% getSheetNames("data/auxdata/Бюллетень_2024.xlsx")) {

		 
			pop_2024 <- as.data.table(read.xlsx("data/auxdata/Бюллетень_2024.xlsx", sheet = w))

			# Columns and vars of interest
			region_name <- as.character(pop_2024[1,1])

			pop_2024 <- pop_2024[, c(1, 3, 4)]
			setnames(pop_2024, c("age", "male", "female"))

			pop_2024 <- pop_2024[age %in% c(1:100, c("100 и более"))]
			pop_2024[age == "100 и более", age := "100"]
			pop_2024[, age := as.integer(age)]

			# Assign regions and years
			pop_2024[, region := region_name]
			pop_2024[, year := 2024]

		}

	}, error = function(e) {})

	# Add the data
	region_sex_age_yearly <- rbind(region_sex_age_yearly, pop_2018, fill = T)
	region_sex_age_yearly <- rbind(region_sex_age_yearly, pop_2021, fill = T)
	region_sex_age_yearly <- rbind(region_sex_age_yearly, pop_2024, fill = T)

	message(w)

}

# Replace "-" with zeros
region_sex_age_yearly[male == "–", male := 0]
region_sex_age_yearly[female == "–", female := 0]

# Unify observations for year 2024
region_sex_age_yearly[year == 2024 & age >= 80, age := 80]
region_sex_age_yearly[, male := as.integer(male)]
region_sex_age_yearly[, female := as.integer(female)]

# Aggregate year 2024
region_sex_age_yearly <- rbind(region_sex_age_yearly[year != 2024], region_sex_age_yearly[year == 2024, .(male = sum(male), female = sum(female)), by = .(age, region, year)])

# Remove region groupings
region_sex_age_yearly <- region_sex_age_yearly[!(region %in% c("Архангельская область (включая Ненецкий автономный округ)", "Архангельская область", "Тюменская область (включая Ханты-Мансийский автономный округ-Югра и Ямало-Ненецкий автономный округ)")) ]
region_sex_age_yearly[, region := str_squish(region)]

region_sex_age_yearly[ region %in% c("Тюменская область без автономных округов", "Тюменская область без автономий"), region := "Тюменская область" ]
region_sex_age_yearly[ region %in% c("Архангельская область без автономного округа", "Архангельская область без автономии"), region := "Архангельская область" ]

# Fix inconsistencies
region_sex_age_yearly[ region == "Республика Северная Осетия-Алания", region := "Республика Северная Осетия – Алания" ]
region_sex_age_yearly[ region == "Ханты-Мансийский автономный округ-Югра", region := "Ханты-Мансийский автономный округ – Югра" ]
region_sex_age_yearly[ region == "Кемеровская область – Кузбасс", region := "Кемеровская область" ]

# Do not ask...
region_sex_age_yearly[ region == "Oмская область", region := "Омская область"]

# Lacking regions in 2018:
#setdiff(unique(region_sex_age_yearly$region), unique(region_sex_age_yearly[year == 2018]$region))

# Wide to long
region_sex_age_yearly <- melt(region_sex_age_yearly, id.vars = c("region", "year", "age"), measure.vars = c("male", "female"),  variable.name = "sex", value.name = "population")

# Export point
fwrite(region_sex_age_yearly, file = "data/auxdata/region_sex_age_yearly_population_2018_2024.csv")