message("4. Attach deflators...")

# The dataset contains several monetary variables in Russian Rubles (RUB). 
# To ensure comparability across waves, we adjusted these for inflation using deflator 
# indices from the Federal State Statistics Service (Rosstat). 
# All values are converted to constant 2024 prices using the following calculation: x/deflator2024rub.

# Load GDP deflator data from Rosstat: 
# https://rosstat.gov.ru/free_doc/new_site/vvp/vvp-god/tab4.htm
# https://www.fedstat.ru/indicator/57352
# 2024: https://rosstat.gov.ru/folder/313/document/254129#:~:text=ВВП%20за%202022%20год%20–156941,дефлятор%20–%20118%2C2%25.
gdp_deflator <- fread("year,GDP_deflator
	1996,145.8	
	1997,115.1	
	1998,118.6
	1999,172.5	
	2000,137.6	
	2001,116.5	
	2002,115.6	
	2003,113.8	
	2004,120.3	
	2005,119.3	
	2006,115.2
	2007,113.8
	2008,118.0	
	2009,102.0
	2010,114.2	
	2011,115.9
	2012,108.9
	2013,105.3
	2014,107.5
	2015,107.2
	2016,102.8
	2017,105.3
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

all_waves[gdp_deflator, "deflator2024rub" := deflator2024rub, on = .(year)]
panel[gdp_deflator, "deflator2024rub" := deflator2024rub, on = .(year)]