
	cd "C:/Users/lago/Dropbox/WIID/Simplified WIID/Standardization/Public/"

	import excel "Data/Metadata/World Development Indicators\CLASS.xlsx", sheet("List of economies") firstrow clear
	ren Economy country_wb
	ren Code c3
	ren Region region_wb
	ren Incomegroup incomegroup
	drop L* O*
	drop if incomeg==""
	
	ren incom* _incom*
	ren reg* _reg*
	
	gen incomegroup	= .
	gen region_wb 	= .
	
	replace incomeg=1 	if _incomeg=="High income"
	replace incomeg=2 	if _incomeg=="Upper middle income"
	replace incomeg=3 	if _incomeg=="Lower middle income"
	replace incomeg=4 	if _incomeg=="Low income"
	
	replace region_wb=1 if _region=="North America"
	replace region_wb=2 if _region=="Latin America & Caribbean"
	replace region_wb=3 if _region=="Europe & Central Asia"
	replace region_wb=4 if _region=="Middle East & North Africa"
	replace region_wb=5 if _region=="Sub-Saharan Africa"
	replace region_wb=6 if _region=="South Asia"
	replace region_wb=7 if _region=="East Asia & Pacific"
	
	lab def incomeg 0 Missing 1 "High income" 2 "Upper middle income" 3 "Lower middle income" 	4 "Low income" 
	lab def region_wb 0 Missing 1 "North America" 2 "Latin America & Caribbean" 3 "Europe & Central Asia" 4 "Middle East & North Africa" 5 "Sub-Saharan Africa" 6 "South Asia" 7 "East Asia & Pacific"
	lab val incomeg incomeg 
	lab val region_wb region_wb
		

	drop _*
	
	save "Data/Metadata/incomeg", replace