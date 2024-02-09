
	* Code that creates reduced WIID datset with relevant observations, with series identification
	
	* Main path
	cd "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\WIID Companion\Standardization\Public\Data"
	
	* Date (latest WIID)
	global date 17NOV2023
	
	* Official WIID file
	global data  "WIID_$date.dta"
	
	tempfile aux1 aux3 aux4 aux5 aux6 aux7 aux8 duplicate countrysum seriessum resourcesum
		
	use "$data" , clear

	* Series type
	drop wiidc*
	
	merge 1:1 id using seriestype, keepus(giniseries shareseries wiidcompanion) update replace
*	drop if _m==2 									/* None */
	cap drop _m
	
	* New geo variables: 
	
	gen 	histent = inlist(country, "Czechoslovakia", "Yugoslavia", "Serbia and Montenegro" , "Soviet Union")
	
	gen 	former = 0
	replace former = 1 if inlist(country, "Czechoslovakia", "Czechia" , "Slovakia")
	replace former = 2 if inlist(country, "Yugoslavia", "Croatia" , "Bosnia and Herzegovina" ,  "Kosovo",  "Montenegro", "North Macedonia" , "Serbia" , "Serbia and Montenegro" ,"Slovenia")
	replace former = 3 if inlist(country, "Soviet Union", "Armenia" , "Azerbaijan" ,"Belarus" ,"Georgia" , "Kazakhstan" , "Kyrgyzstan") 
	replace former = 3 if inlist(country , "Moldova" , "Russia" ,  "Tajikistan" , "Turkmenistan" , "Ukraine" , "Uzbekistan" , "Estonia", "Latvia" , "Lithuania")
	replace former = 4 if inlist(country, "Sudan" , "South Sudan")
	replace former = 5 if inlist(country, "Ethiopia" , "Eritrea")

	lab def former 0 None 1 "Czechoslovakia" 2 "Yugoslavia" 3 "Soviet Union" 4 "Sudan" 5 "Ethiopia", replace
	lab val former former

	cap drop ussr
	gen 	ussr = 0
	replace ussr = 1 if inlist(c3, "ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU")
	replace ussr = 1 if inlist(c3, "MDA", "RUS", "TJK", "TKM", "UKR", "UZB", "SUN") 

	gen exsocialist = (inlist(former, 1,2,3) | inlist(country, "Albania", "Bulgaria" , "Hungary", "Poland", "Romania"))
	
		* Alternative Region, former 'Soviet Union' countries (except EU: Latvia, Lethonia, Estonia)

	cap drop ussr
	gen 	ussr = ( (region_un_s==203 | region_un_s==402) & eu==0)
	replace ussr = 1 if inlist(country, "Armenia", "Azerbaijan", "Georgia")
	replace	ussr = 0 if country=="Czechoslovakia"

	tab country if ussr==1	
	
	gen 	region = region_wb
	replace region = 30 if former==3 & c3!="EST" & c3!="LVA" & c3!="LTU" 
	lab def region_wb 30 "non EU soviet" , modify
	lab val region region_wb
	

	*****************	1. Preparing the data
		
	* What indices are available in each observation?	

	gen 	aux =(q1!=. | q2!=. | q3!=. | q4!=. | q5!=. | d1!=. | d2!=. | d3!=. | d4!=. | d5!=. | d6!=. | d7!=. | d8!=. | d9!=.  | d10!=. | bottom5!=. | top5!=.)
	gen 	index = 1 if (gini!=. & aux==0)
	replace index = 2 if (gini==. & aux==1)
	replace index = 3 if (gini!=. & aux==1)
	replace index = 4 if (gini==. & aux==0)
	lab var index "Available measures"
	lab def index 1 Gini 2 Shares 3 "Gini&Shares" 4 None, replace
	lab val index index
	
	cap drop deciles
	cap drop quintiles
	cap drop b5
	cap drop t5
	
	gen deciles 	= (d1 !=. & d2 !=. & d3 !=. & d4 !=. & d5 !=. & d6 !=. & d7 !=. & d8 !=. & d9 !=. & d10 !=.)
	gen quintiles 	= (q1 !=. & q2 !=. & q3 !=. & q4 !=. & q5 !=.) & q5>0
	gen b5			= (bottom5 !=.)
	gen t5			= (top5 !=.)

	gen 	indexd = 10
	replace indexd = 0 if deciles==0 & quintiles==0
	replace indexd = 1 if deciles==1 & b5==1 & t5==1
	replace indexd = 2 if deciles==1 & b5==0 & t5==1
	replace indexd = 3 if deciles==1 & b5==1 & t5==0
	replace indexd = 4 if deciles==1 & b5==0 & t5==0
	replace indexd = 5 if deciles==0 & quintiles==1 & b5==1 & t5==1
	replace indexd = 6 if deciles==0 & quintiles==1 & b5==0 & t5==1
	replace indexd = 7 if deciles==0 & quintiles==1 & b5==1 & t5==0
	replace indexd = 8 if deciles==0 & quintiles==1 & b5==0 & t5==0
	replace indexd = 9 if deciles==0 & quintiles==1 & (d1!=. | d2!=. | d9!=. | d10!=.) 
		
	label def indexd 0 "none/incomplete" 1 full 2 "deciles+t" 3 "deciles+b" 4 deciles 5 "fullquin" 6 "quintiles+t" 7 "quintiles+b" 8 quintiles 9 "quintiles+" 10 other , replace
	label val indexd indexd
	lab var indexd "Available combination of shares"
	
	* This obs. has inconsistent income shares with a large impact, we will only use Gini
	replace indexd = 0 if source_d =="Deininger and Squire, World Bank 2004" & country=="India" & year==1999

	******************************************** reformulating some variables
	
	* We construct clones of the main variables that will be adjusted accordingly with the needs to construct a longer consistent series, while keeping the original values (_****)
	foreach var in resource resource_detailed scale sharing_unit reference_unit areacovr areacovr_detailed source source_comment source_detailed survey {
		gen _`var'			= `var'
		cap lab val _`var' `var'
	}	
	
	* Linking some series based on source, with no adjustment since there is no overlapping
	
		* UN--> UNICEF Poland 1975 (1 obs)
	replace source_d = "UNICEF" if source_d == "United Nations 1981" & country == "Poland" & year == 1976
		* Peru, Szekely and Hilgert 2002, link all and almost all
	replace areacovr_detailed = 101 if country == "Peru" &  areacovr_detailed == 103 & resource == 1 & scale == 1 & source_d == "Szekely and Hilgert 2002"
		* Sri Lanka
	replace areacovr_detailed = 101 if country == "Sri Lanka" & areacovr_detailed == 109 & source == 5
	
   	* Numeric codes for country (idcountry), source_d (idsourced), and new variable combining source and source_d (idsource)

	gen one = 1
	
		* Creating numeric id for Country
	preserve
	sort c3 , stable
	collapse (sum) one (mean) region* incomeg, by(country c2 c3)
	gen idcountry=_n
	lab val region_wb 		region_wb
	lab val region	 		region_wb
	lab val region_un 		region_un
	lab val region_un_sub 	region_un_sub
	lab val incomegroup 	incomegroup

	ren one nobs
	save  countrynames , replace
	restore

	cap drop _merge
	merge m:1 country using countrynames
	cap drop _merge
	lab var idcountry 	"Country numerical identifier"
	lab var nobs 		"N observation in country"
	
		* Source_d for Research Studies within country (idsource8)
	preserve
	collapse (min) idcountry if source==8 , by(country source_d)
	sort country source_d , stable
	bys country: gen idsource8 = idcountry*100 + _n
	lab def idsource 0 "0" , replace
	global N=_N
	forvalues i=1/$N {
		local w_`i'= idsource8[`i']
		local q_`i'=  source_d[`i']
		lab def idsource `w_`i'' "`q_`i''" , modify
	}
	lab val idsource8 idsource
	save `aux1' , replace
	restore
	
	merge m:1 country source_d using `aux1'
	cap drop _merge

		* Creating numerical id for main sources (Idsource)
	gen 	idsource = source*10
	replace idsource = 11 if source_c == "Own construction based on ERF/LIS Database through LISSY"

	replace idsource = 21 if source_d == "Eurostat microdata"
	replace idsource = 22 if source_d == "Eurostat microdata" & survey=="European Union Statistics on Income and Living Conditions (EU-SILC)"

	replace idsource = 41 if source_d == "Altimir 1986"
	replace idsource = 42 if source_d == "ECLAC" 
	replace idsource = 43 if source_d == "ECLAC 2016"
	replace idsource = 44 if source_d == "ECLAC 1984" | source_d == "ECLAC 1986"
	replace idsource = 45 if source_d == "UNICEF" | source_d == "UNICEF 1999" | source_d == "UNICEF 2004" | source_d == "UNICEF 2005" | source_d == "UNICEF 2007" | source_d == "UNICEF 2008" | source_d == "UNICEF 2011"
	replace idsource = 71 if source_d == "Deininger and Squire, World Bank 2004"
	replace idsource = 72 if source_d == "Jain 1975"
	replace idsource = 73 if idsource==70 & source_d != "Poverty and Inequality Platform (PIP)"	
	replace idsource = idsource8 if source==8
	replace idsource = 91 if source_d=="Dowling and Soo 1983"
	replace idsource = 92 if source_d=="Asian Development Bank 2007"
	replace idsource = 93 if source_d=="ILO 1984" | source_d== "International Labour Organization"
	
	lab var idsource "Source (specific source used for series)"	
	lab def idsource 10 LIS 11 "ERF/LIS" 20 Eurostat 21 ECHP 22 EUSILC 30 "SEDLAC" 40 UN 41 "Altimir 1986" 42 "ECLAC" 43 "ECLAC 2016" 44 "ECLAC other" 45 UNICEF 50 NSA 60 OECD 70 "PIP" 71 "Deininger and Squire 2004" 72 "Jain 1975" 73 "WB other" 80 Research 90 "IADB" 91 "Dowling and Soo 1983" 92 "ADB, other" 93 ILO 94 IMF , modify
	lab val idsource idsource 

	gen 	idsourced = idsource
	lab var idsourced "Source (specific source, more detailed)"	
	lab val idsourced idsource 
	
	* Creating particular cases with overlapping series (more than one observation per case and year)
				* Based on variables not used to construct series: source_c, source_d, or survey  
	
		* Serbia and Montenegro, UNICEF, source_d
	replace idsourced = 451 if country=="Serbia and Montenegro" & source_d=="UNICEF 1999" & survey=="Household Budget Survey"
	replace idsourced = 452 if country=="Serbia and Montenegro" & source_d=="UNICEF 2004"
	lab def idsource 451 "UNICEF 1999: Household Budget Survey" 452 "UNICEF 2004", modify

	replace idsourced = 62 if source_c=="Old series" & source==6
	lab def idsource 62 "OECD old series", modify

	***************** 	2. Identifying series by:
	***************** 		idsource + _resource + _scale + _sharing_u + _reference_u + areacovr_d
	***************** 		source, source_d, source_c, and survey were used to construct idsource

	* We will generally assume missing scale/ref/sharing is total household aggregates (that was the most common among old observations)

	recode  scale   (.=3)
	recode  scale_d (.=301)
	recode  sharing (.=1)
	recode 	reference_u (.=1)

	* Storing values of idsourced, areacovr_d and a combination of the other variables in matrices
	
	tab 	idsourced if source !=8,  matrow(xx) 
	local 	xx = rowsof(xx)
	di 		"N rows=`xx'"

	gen 	yy = resource*100000 + reference_unit*10000 + scale_d*10 + sharing_unit
	tab 	yy ,  matrow(yy) 
	local 	yy = rowsof(yy)
	di 		"N rows=`yy'"

	tab 	areacovr_d ,  matrow(zz) 
	local 	zz = rowsof(zz)
	di 		"N rows=`zz'"
	
	gen  	series  = .
	lab var series "Series unique identifier"
	
	* Constructing identifier for series: General case
	
	forvalues 	a = 1/`xx' {
		forvalues 	b = 1/`yy' {
			forvalues 	c = 1/`zz' {
				local x = xx[`a',1]
				local y = yy[`b',1]
				local z = zz[`c',1]
			*	di "series = `x'`y'`z' "
				qui replace  series = `x'`y'`z' if idsourced==`x' & yy == `y' & areacovr_d==`z'
			}
		}
	}

	* Constructing identifier for series: Research Studies
	
	gen qq = idsourced - idcountry*100 if source==8
	su qq
	local 		xx = r(max)
	di "xx=`xx'"
	forvalues 	a = 1/`xx' {
		forvalues 	b = 1/`yy' {
			forvalues 	c = 1/`zz' {
				local y = yy[`b',1]
				local z = zz[`c',1]
			*	di "series = `a'`y'`z' "
				qui replace  series = 80`a'`y'`z' if idsourced==idcountry*100 + `a' & yy == `y' & areacovr_d==`z'
			}
		}
	}
	
	count if series==.

	************ Computing N Obs in each country, by series, source, idsourced
		
	preserve
	collapse (sum) one, by(country source)
	sort country source, stable
	bys country: 	gen nsource   = _N
	save `aux3' , replace
	restore
	preserve
	collapse (sum) one, by(country source_d)
	sort country source_d, stable
	bys country: 	gen nsourced  = _N
	save `aux4', replace
	restore
	preserve
	collapse (sum) one, by(country idsourced)
	sort country idsourced, stable
	bys country: 	gen nidsourced = _N
	save `aux5' , replace
	restore
	preserve
	gen  year0 = year
	gen  year1 = year
	collapse (sum) one (min) year0 (max) year1 wiidcompanion giniseries shareseries, by(country series)
	sort country series, stable
	bys  country: 	gen nseries  = _N
	bys  country: 	gen idseries = _n
	ren wiidcompanion 	wiidcseries 
	ren giniseries 		giniseriesc 
	ren shareseries		shareseriesc
	save `aux6' , replace
	restore
	preserve

	collapse (sum) one , by(country series year)
	sort country series, stable
	bys  country series: gen nyears = _N
	bys  country series: gen idyear = _n
	save `aux7' , replace
	
	collapse (sum) one , by(country year)
	sort country year, stable
	bys  country 	: 	gen nyearsc= _N
	save `aux8' , replace
	
	restore

	cap drop _merge
	merge m:1 country source 		using `aux3'
	cap drop _merge
	merge m:1 country source_d 		using `aux4'
	cap drop _merge
	merge m:1 country idsourced		using `aux5'
	cap drop _merge
	merge m:1 country series 		using `aux6'
	cap drop _merge
	merge m:1 country series year 	using `aux7'
	cap drop _merge
	merge m:1 country year 			using `aux8'
	cap drop _merge
	
	label var nobs       "N of observations in country"
	label var nsource    "N of sources in country"
	label var nsourced   "N of sources_d in country"
	label var nidsource  "N of idsources in country"
	label var nseries    "N of series in country"
	label var nyears     "N of years in country and series"
	label var nyearsc    "N of years in country"
	label var idyear     "N of order of year within country and series"
	label var idseries   "N of order of series within country"
	label var year0		 "Initial year, series"
	label var year1		 "Final year, series"	
	label var wiidcs	 "Series is used in WIID companion"	
	label var giniseriesc  "Series is used in WIID companion"	
	label var shareseriesc "Series is used in WIID companion"	
	
	sort country nseries year , stable
	order id idcountry country c3 c2 nobs nyearsc nseries idseries wiidcs shareseriesc giniseriesc series nyears year0 year1 idyear nsource nsourced nidsource idsource* source_d resource scale survey  

	
	*****************************************************************************************
	
	preserve
	gen year0c = year
	gen year1c = year
	collapse (min) nobs idcountry region_wb incomegroup year0c nyearsc nseries nsource nsourced nidsource (max) year1c , by(country c3 c2 )
	order id idcountry country c3 c2 region_wb incomegroup nobs nseries year0 year1 nyearsc nyears nsource* nidsource
	lab val region 	region_wb
	lab val incomeg incomegroup
	label var year0c	"Initial year, country"
	label var year1c	"Final year, country"	
	label data 			"Summary by country"
	save `countrysum' , replace
	
	tab nseries
	tab year0
	tab year1
	tab nsource
	tab nsourced
	tab nidsource
	restore

	***************** 	3. Creating Dataset seriessum and resorucesum, summarizing series and resources 
	*****************	by country
	
	* series
	preserve
	collapse (min) nobs idcountry  region_wb incomegroup former histent year0 nyears nseries idseries index idsource* resource scale  nsource nsourced nidsource (max) year1 (mean) wiidcseries shareseriesc giniseriesc , by(country c3 c2 source_d series)
	order idcountry country c3 c2 region_wb incomegroup nobs nseries idseries wiidcseries shareseriesc giniseriesc index year0 year1 nyears idsource* resource scale source_d nsource nsourced nidsource year0 year1
	
	lab val region 		region
	lab val region 		region_wb
	lab val incomeg 	incomegroup
	lab val former 		former
	lab val histent		histent
	lab val resource 	resource
	lab val scale 		scale
	lab val idsource 	idsource
	lab val idsourced 	idsource
	lab val index 		index
	
	label data "Summary by series"
	save `seriessum' , replace

	restore

	* Resources
	preserve
	
	gen 	resourceb = 1
	replace resourceb = 2 if resource==4
	lab var resourceb "Resource (simplified)"
	lab def resourceb 1 income 2 consumption
	lab val resourceb resourceb
	
	collapse (sum) one  (min) nobs idcountry region_wb incomegroup year0 nyears nseries nsource nsourced nidsource (max) year1 , by(country c3 c2 resourceb)
	order id idcountry country c3 c2 resourceb region_wb incomegroup nobs nseries year0 year1 nyears nsource* nidsource
	lab val region region_wb
	lab val incomeg incomegroup

	tab resourceb
	
	label data "Summary by country & resource"
	save `resourcesum' , replace
	restore

	***************** 	4. Creating New reduced WIID
 
	drop aux one idsource8 yy qq 
	label data "World Income Inequality Database (WIID), with series identifier, long format"
	
	drop if year==.

	gen _series 	= series
	gen _idseries 	= idseries
	gen _idsourced 	= idsourced
	
	save wiid0 , replace
	


/*	

	* Checking match with percentiles
	use wiid0, clear
	drop if indexd==0
	merge 1:1 id using percentiles, 

	* Checking with wiid1
	
	use wiid0, clear
	ren gini _gini
	ren ge*  _ge*
	ren bottom* _bottom*
	ren top* _top*
	
*	merge 1:1 id using "C:\Users\Lago\Dropbox\WIID\Simplified WIID\Standardization\Data\wiidfinal1", update	
	merge 1:1 id using wiid1, update	
	
	su gini _gini ge* _ge* bot* _bot* top* _top*
	gen d=gini-_gini
	
	list id country year wiidcompanion _source_d source_d _resource resource gini _gini if d!=0 & d!=.