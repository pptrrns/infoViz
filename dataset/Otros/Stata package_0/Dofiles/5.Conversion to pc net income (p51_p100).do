	
	* Code that executes Phase 2 (Standardization), Part 2 (p51-p100)

	* Main path
	cd 	"...\Data"
	global data  "WIID_31MAY2021.dta"
	
	global shares	p51 p52 p53 p54 p55 p56 p57 p58 p59 p60 p61 p62 p63 p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100
		
	tempfile lisineqconceptsb lisineqconceptsc lisineqconceptsri lisineqconceptsr lisineqconceptsi lisineqconceptsri lis_net ineqnet
	
	use 	"$data" , clear
	
	* Bringing country numerical codes
	merge m:1 country using countrynames, update
	drop _m

	drop 	d1-d10
	drop 	q1-q5
	drop 	bottom*
	drop 	top*
	
	* Only LIS obs.
	keep if source==1
	
	* Bringing percentiles and estimated indices

	merge 1:1 id using percentiles
	keep if _merge==3

	* Saving, net per capita as ineqnet.dta
	
	preserve 
	keep if resource==1 & scale==1
	foreach i in $shares gini {
	ren 	`i' `i'net
	}
	save `ineqnet' , replace
	restore
	
	* Merging gininet and actual gini
	cap drop _m
	merge 	m:m country year using `ineqnet', keepus(*net)
	drop if _m!=3

	lab val region_wb region_wb
		
	estimates clear	
	
	* Checking N informative cases (other than pc net) cases
		
	cap drop informative
	gen informative = (resource!=1 | scale!=1)

	* Conversions from any resource and scale to --> net pc income
		* Based only on country/region/incomegroup, LIS sample
		* We do not include other possible explanatory variables, but they could be added here
		
	* Weights so that they add up to 1 by country, so each country has same weight regardless of years in the dataset
	bys 	country source resource scale_d: gen weightnet = 1/_N
	tabstat weightnet if resource==1 & scale_d==101 , by(country) stat(sum)

	* Regressions conditional on country, income group and region, region, income group:

	foreach i in $shares {
	di "net_`i'"

qui	reg 	`i'net `i' i.resource#i.scale_d#i.idcountry [pw=weightnet] if info==1
	estimates store regnetc_`i'
qui	predict gnetpc_`i'

qui	reg 	`i'net `i' i.resource#i.scale_d#i.incomegroup#i.region [pw=weightnet] if info==1
	estimates store regnetri_`i'
qui	predict gnetpri_`i'

qui	reg 	`i'net `i' i.resource#i.scale_d#i.region [pw=weightnet] if info==1
	estimates store regnetr_`i'
qui	predict gnetpr_`i'

qui	reg 	`i'net `i' i.resource#i.scale_d#i.incomegroup [pw=weightnet] if info==1
	estimates store regneti_`i'
qui	predict gnetpi_`i'

	}
	
	****************************************************************
	
	* Files for associating countries with specific coutnry grouping

	cap drop _merge
	save 	`lisineqconceptsb' , replace
	collapse region* incomegroup , by(country c3 resource)
	save 	`lisineqconceptsc' , replace
	gen one=1
	preserve
	collapse one , by(country resource)
	save 	 `lisineqconceptsc' , replace
	restore
	preserve
	collapse one , by(region incomeg resource)
	save 	 `lisineqconceptsri' , replace
	restore
	preserve
	collapse one , by(region resource)
	save 	 `lisineqconceptsr' , replace
	restore
	preserve
	collapse one , by(incomeg resource)
	save 	 `lisineqconceptsi' , replace
	restore

	
********************************************************************************************************
********************************************************************************************************
********************************************************************************************************
********************************************************************************************************
********************************************************************************************************

	* Identifying relevant grouping
		* country/income group/region group
	
	use  wiid2 , clear
		* To use MENA countries to make for Turkey
	replace region_wb=4 if country=="Turkey"
	replace region=4 	if country=="Turkey"
	
********************************************************************************************************
*********	Note that scale is 3 for Hong Kong shares and 1 for Gini
*********   China: with Ravallion and Chen or with WB (not both)      ----> giniseries vs shareseries
*********	South Africa with Leibbrandt et al or with WB (not both)  ----> giniseries vs shareseries
********************************************************************************************************

	* No conversion ...
		* gros/net similar to net
	replace resource=1 if resource==2
		* Former socialist countries, gross treated as net
	replace resource=1 if resource==3 & exsocialist == 1 & year<=1993
		* SSA and S Asia gross treated as net 
	replace resource=1 if resource==3 & (region==5 | region==6)

		* Special case, HK
	replace scale    = _scale 	 if country=="Hong Kong"
	replace scale_d  = _scale_d	 if country=="Hong Kong"

	* Some corrections in unknown/missing scale
	
		* In final version, by default, square root (except Europeans or Turkey)
	replace scale_d=207  if scale_d==201 & inlist(country, "Eritrea", "Korea, Republic of", "Qatar", "New Zealand")
		* Dealing with some missing/unknown scale_d
	replace scale_d=206  if scale_d==201 /* & inlist(country, "Andorra" , "Greenland", "Turkey") */

	* Associating countries with LIS sample
	preserve
	keep country year c3 region_wb region incomegroup resource scale_d _resource _source
	collapse region* incomegroup , by(country c3 resource)
	cap drop _merge
	merge 1:1 country resource 			using `lisineqconceptsc'  , keepus(country resource)
	ren _m mc
	merge m:m region incomeg resource 	using `lisineqconceptsri' , keepus(region* incomeg resource)
	ren _m mri
	merge m:m region resource 		 	using `lisineqconceptsr'  , keepus(region* resource)
	ren _m mr
	merge m:m incomegroup resource		using `lisineqconceptsi'  , keepus(incomegroup resource)
	ren _m mi
		
	foreach i in c ri r i {
	gen 	lis`i'_net = 0
	}
	
	replace	lisc_net  = 1 if mc==3
	replace	lisri_net = 1 if lisc_net  == 0 & mri>1
	replace	lisr_net  = 1 if lisc_net  == 0 & lisri_net == 0 & mr >1
	replace	lisi_net  = 1 if lisc_net  == 0 & lisri_net == 0 & lisr_net  == 0 & mi >1
	
	drop if mc<3 & mri<3 & mr<3 & mi<3
	drop if country==""
	save `lis_net' , replace
	restore
	
	**************************************************************************
	
	ren gini _gini
	forvalues i=1/10 {
	ren d`i' _d`i' 
	}
	forvalues i=1/100 {
	ren p`i' _p`i' 
	}
	ren bottom* _bottom*
	ren top* _top*
	ren *_adj *
		

	cap drop _m
	merge m:m country resource using `lis_net'

	su gini lis*
	
	
	* Getting predicted values

	foreach i in $shares {
	foreach g in c ri r i {
qui	estimates restore regnet`g'_`i'
qui	predict gnetp`g'_`i'
qui replace gnetp`g'_`i' = round(gnetp`g'_`i', 0.01)
	}
	}

	* Documenting level of conversion
	
	gen		conversion = 0
	replace	conversion = 1	if (resource>1 | scale!=1) 					& lisc_net==1
	replace	conversion = 2	if (resource>1 | scale!=1) 	& conversion==0	& lisri_net==1
	replace	conversion = 3	if (resource>1 | scale!=1) 	& conversion==0	& lisr_net==1
	replace	conversion = 4	if (resource>1 | scale!=1) 	& conversion==0	& lisi_net==1
	
	lab var conversion "Conversion (phase 2)"
	lab def conversion 0 none 1 country 2 "region & income group" 3 region 4 "income group"
	lab val conversion conversion
	tab conversion resource
	tab conversion adjustment
	
	foreach i in $shares {
	
	cap drop `i'p 
	gen 	 `i'p = `i'
	replace	 `i'p  = gnetpc_`i'   if conversion==1
	replace	 `i'p  = gnetpri_`i'  if conversion==2
	replace	 `i'p  = gnetpr_`i'   if conversion==3
	replace	 `i'p  = gnetpi_`i'   if conversion==4
	}	
	
	cap drop _m
	drop if id==.
	save wiid3b, replace
	
	use  wiid3a, clear
	drop if id==.
	merge 1:1 id using wiid3b , keepus(p51p-p100p) update replace
	drop _merge

	save wiid3 , replace
	
	erase wiid3a.dta
	erase wiid3b.dta
