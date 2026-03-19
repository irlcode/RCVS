library(data.table)
library(openxlsx)
library(zoo)
library(stringr)

## This script extracts educational attainment data from the 2020 Russian Census.
# To calculate post-stratification weights, we require data on higher education 
# at the federal district level, broken down by age groups.
# These data are required for calculating post-stratification counts (targets). 
# The output is available at: data/auxdata/region_educ_population_census.csv.

# https://rosstat.gov.ru/vpn/2020/Tom3_Obrazovanie
educ <- as.data.table(read.xlsx("data/auxdata/Tom3_tab1_VPN-2020.xlsx", startRow = 6))
# Restore names
names(educ) <- c("variable", "all", "respondents", "kadry_vysshej_kvalifikacii", "vysshee",	"magistratura", "specialitet", "bakalavriat", "nepolnoe_vysshee", "srednee_professionalnoe", "specialist_srednego_zvena",	"kvalificirovannyj_rabochij", "srednee",	"osnovnoe", "nachalnoe",	"doshkolnoe", "ne_imejushhie_obrazovanija", "iz_nih_negramotnye", "ne_ukazavshie_uroven_obrazovanija")

# Remove noise  
educ[variable == "Ханты-Мансийский автономный \r\nокруг – Югра", variable := "Ханты-Мансийский автономный округ"]
educ[variable == "Республика Саха (Якутия)", variable := "Якутия"]

# Keep only unique names: regions and all federal districts
regions <- educ[!is.na(variable), .N, by = variable][N == 1]

# Extract regional and federal district distributions (name location index + 18 rows below)
start_indexes <- educ[, which(grepl(paste0(regions$variable, collapse = "|"), variable, ignore.case = T))]

region_educ_population_census <- data.table()
for(ind in 1:length(start_indexes)) {
  
  tempdt <- educ[start_indexes[ind]:(start_indexes[ind] + 18)]
  # Make separate variable with location name
  tempdt[1, region := variable]
  # Fill-in names
  tempdt[, region := na.locf(region, fromLast = F)]
  # Remove row from col "variable" with region name
  tempdt <- tempdt[-1,]
  # Bind
  region_educ_population_census <- rbind(region_educ_population_census, tempdt)
  
}

# Remove unnecessary rows
region_educ_population_census <- region_educ_population_census[!grepl("Городское", variable, ignore.case = T)]
region_educ_population_census <- region_educ_population_census[!grepl("в том числе в возрасте, лет", variable, ignore.case = T)]
region_educ_population_census <- region_educ_population_census[!grepl("Мужчины и женщины в возрасте 6 лет и более", variable, ignore.case = T)]

# Reshape file 
#region_educ_population_census <- melt(region_educ_population_census, id.vars = c("region", "variable"), variable.name = "educlvl", value.name = "n", variable.factor = F)
#region_educ_population_census[n == "-", n := NA_integer_]
#region_educ_population_census[, n := as.integer(n)]
#region_educ_population_census[, variable := gsub(" – ", "-", variable, fixed = T)]
region_educ_population_census[, variable := gsub(" – ", "-", variable, fixed = T)]
setnames(region_educ_population_census, "variable", "age")

# Remove region groupings
region_educ_population_census <- region_educ_population_census[!region %in% c("Архангельская область", "Тюменская область")]
region_educ_population_census[region == "Архангельская область без автономного округа", region := "Архангельская область"]
region_educ_population_census[region == "Тюменская область без автономных округов", region := "Тюменская область"]

# Rename regions
region_educ_population_census[region == "Кемеровская область – Кузбасс", region := "Кемеровская область"]

# Make proper order
setcolorder(region_educ_population_census, "region")

# Export result
fwrite(region_educ_population_census, file = "data/auxdata/region_educ_population_census.csv")