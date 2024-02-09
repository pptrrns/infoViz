
	cd "C:\Users\lago\Dropbox\WIID\Simplified WIID\Standardization\Public\Data\Metadata\"

	* UN Country codes
	*https://population.un.org/wpp/Download/Files/4_Metadata/WPP2019_F01_LOCATIONS.XLSX	
	tempfile populationun0
	import excel "UN Data\WPP2019_F01_LOCATIONS.xlsx", sheet("Location (2)") firstrow clear
	
	ren ISO3Alphacode c3
	ren Regionsub countryun
	save `populationun0', replace
	
	* UN Population estimates
	*https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2019_TotalPopulationBySex.csv
	
	import delimited "UN Data\WPP2019_TotalPopulationBySex.csv" , clear 
	ren location countryun
	ren time year
	ren poptotal population
	keep if variant=="Medium"
	replace country="Côte d'Ivoire" if country=="CÃ´te d'Ivoire"
	merge m:1 country using `populationun0'
	keep countryun year population c3	
	replace population = population*1000
	save populationun, replace	
