
	cd "C:\Users\lago\Dropbox\WIID\Simplified WIID\Standardization\Public\"

	* UN Country codes
* https://population.un.org/wpp/Download/Metadata/Documentation --> Locations (XLSX, 131 KB)
* https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX	

	tempfile populationun0
	import excel "Data/Metadata/UN Data\WPP2019_F01_LOCATIONS.xlsx", sheet("Location (2)") firstrow clear
	
	ren ISO3Alphacode c3
	ren Regionsub countryun
	save `populationun0', replace
	
	* UN Population estimates
* https://population.un.org/wpp/Download/Standard/CSV/ --> Total Population All variants (CSV, 21.35 MB)
* https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv	
	import delimited "Data/Metadata/UN Data\WPP2019_TotalPopulationBySex.csv" , clear 
	ren location countryun
	ren time year
	ren poptotal population
	keep if variant=="Medium"
	replace country="Côte d'Ivoire" if country=="CÃ´te d'Ivoire"
	merge m:1 country using `populationun0'
	keep countryun year population c3	
	replace population = population*1000
	save "Data/Metadata/population_un", replace	

	* WB Population estimates (https://databank.worldbank.org/source/world-development-indicators)
	
	import delimited "Data/Metadata/World Development Indicators/a7b8a64a-e564-4846-8571-74a8f24f93ad_Data.csv" , varnames(nonames) clear 
	ren v1 country_wb
	ren v2 c3
	drop v3 v4
	local j = 1960
	forvalues i=5/70 {
	cap ren v`i' population_wb`j'
	cap replace  population_wb`j'="" if population_wb`j'==".."
	local j = `j'+1
	}
	drop if _n==1
	drop if c3==""
	reshape long population_wb, i(c3 country) j(year)
	
	destring *, replace
	save "Data/Metadata/population_wb", replace	
