
	* Code that execute Phase 1 (Integration), Part 1 (gini and p1-p50)

	* Main path
	cd 	"...\Data"

	global percentiles	p1 p2 p3 p4 p5 p6 p7 p8 p9 p10 p11 p12 p13 p14 p15 p16 p17 p18 p19 p20 p21 p22 p23 p24 p25 p26 p27 p28 p29 p30 p31 p32 p33 p34 p35 p36 p37 p38 p39 p40 p41 p42 p43 p44 p45 p46 p47 p48 p49 p50 p51 p52 p53 p54 p55 p56 p57 p58 p59 p60 p61 p62 p63 p64 p65 p66 p67 p68 p69 p70 p71 p72 p73 p74 p75 p76 p77 p78 p79 p80 p81 p82 p83 p84 p85 p86 p87 p88 p89 p90 p91 p92 p93 p94 p95 p96 p97 p98 p99 p100
	global shares 	 	gini $percentiles 
	
	tempfile reverse ginie wiidseries wiidseriesT iq ssfactor wiidtemp
	
	* ADO file that will be used to compute Gini
	do  "C:\Users\lago\Dropbox\WIID\Simplified WIID\Standardization\Public\Dofiles\ineqwiid.ado"

	cap log close
	log using Adjustments , replace
	
	use wiid0 , clear
	
	* Keping original variables before they are changed

	drop _*
	foreach var in resource scale source {
		gen _`var' 			= `var'
		gen _`var'_detailed = `var'_detailed
	}
	
	foreach var in series year idsourced {
		gen _`var' 			= `var'
	}


	***********************************************************************************
	**************************************** Some adjustments:
	***********************************************************************************
	
	* India: integraring D&S into WB series /* only Gini, D&S2004, shares are inconsistent */
	sort source idseries
	by 	 source: replace   series =   series[1] if source_d=="Deininger and Squire, World Bank 2004" & country=="India"
	by 	 source: replace idseries = idseries[1] if source_d=="Deininger and Squire, World Bank 2004" & country=="India"
	replace source_d="PovcalNet"				if source_d=="Deininger and Squire, World Bank 2004" & country=="India"
	replace source_d="Eurostat Data browser" 	if source_detailed=="European Commission 2006" & country=="Cyprus"
	sort country source_d idseries

	replace source_d="UNICEF" 					if country=="North Macedonia" & source==4

		* Argentina
			* we can unify one obs from Altimir 1986 for Argentina with the rest of the series (diff. in resource)
	bys country source_d areacovr_detailed: replace series=series[_N] 		if source_d=="Altimir 1986" & country=="Argentina"
	bys country source_d areacovr_detailed: replace idseries=idseries[_N] 	if source_d=="Altimir 1986" & country=="Argentina"
	bys country source_d areacovr_detailed: replace resource=resource[_N] 	if source_d=="Altimir 1986" & country=="Argentina"

	bys country source_d : replace resource=2 				if source_d=="Fields 1989" & country=="Brazil"
	bys country source_d : replace idseries=idseries[_N] 	if source_d=="Fields 1989" & country=="Brazil"
	bys country source_d : replace series=series[_N] 		if source_d=="Fields 1989" & country=="Brazil"
	
	replace source_d="PovcalNet" if source_d=="PovcalNet 2019" /*& country=="Ecuador"*/

			* Indonesia: We unify Fields' series, ignoring geographical coverage
	sort source_d idseries
	by source_d : replace idseries = idseries[_N] if source_d == "Fields 1989" & country=="Indonesia"
	by source_d : replace  series =   series[_N]  if source_d == "Fields 1989" & country=="Indonesia"
	
		* Malaysia: ignoring slight difference in resource within Bruton and within Jain
	sort source_d idseries
	by source_d: replace   series =   series[_N] if (source_d=="Bruton et al. 1992" | source_d=="Jain 1975") & country=="Malaysia"
	by source_d: replace idseries = idseries[_N] if (source_d=="Bruton et al. 1992" | source_d=="Jain 1975")  & country=="Malaysia"

			* Taiwan: We make Jain one only series, ignoring gross vs gross/net
	sort country source_d year
	by country source_d: replace   series =   series[_N] if source_d=="Jain 1975" & country=="Taiwan"
	by country source_d: replace idseries = idseries[_N] if source_d=="Jain 1975" & country=="Taiwan"
	bys country source_d : replace resource=resource[_N] if source_d=="Jain 1975" & country=="Taiwan"

	replace source_d="UNICEF" if source==4 & country=="Bulgaria"
	
	replace source_d="Hong Kong Census and Statistics Department" if source_d=="Hong Kong Census and Statistics Department 1990"
	
	replace source  = 7				   if source_d=="van Ginneken and Park 1984" & country=="Egypt"
	replace source_d="PovcalNet" 	   if source_d=="van Ginneken and Park 1984" & country=="Egypt"
	sort country source_d series
	by country source_d: replace   series =   series[_N] if source_d=="PovcalNet" & country=="Egypt"
	by country source_d: replace idseries = idseries[_N] if source_d=="PovcalNet" & country=="Egypt"
	
	replace source_d="PovcalNet" if source_d=="World Bank 2002" & country=="Jamaica"
	sort country source_d 
	by country source_d : replace idseries = idseries[_N] if source_d=="PovcalNet" & country=="Jamaica"
	by country source_d : replace   series =   series[_N] if source_d=="PovcalNet" & country=="Jamaica"
	
	* Changing year (to improve connection among series)
	
	replace year = year + 1 if source_d=="Pedersen and Lockwood 2001" & country=="Haiti" & year==2000
	replace year = year + 1 if source_d=="Lin 1985" 			 & country=="Hong Kong"  & year==1980
	replace year = year + 1 if source_d=="Nolan and Maitre 2000" & country=="Ireland"    & year==1987
	replace year = year + 1 if source==7 						 & country=="Romania" 	 & year==2006
	replace year = year + 1 if source_d=="PovcalNet"  			 & country=="Kosovo" 	 & year==2017
	replace year = year + 1 if source==5 						 & country=="Canada" 
	
	***********************************************************************************
	
	sort country year

	save `wiidtemp' , replace

	****************************** Defining reference series when not the last series
	
	gen 	lis=	 (source_d=="Luxembourg Income Study (LIS)" & scale==1)
	replace lis=1 if (source_d=="Sir Arthur Lewis Institute of Social and Economic Studies 2012" & country=="Barbados")
	replace lis=1 if (source_d=="Deininger and Squire, World Bank 2004" & resource<4 & country=="Burkina Faso")
	replace lis=1 if (source_d=="Deininger and Squire, World Bank 2004" & resource<4 & country=="Nigeria")
	replace lis=1 if (source==5 & scale==1 & country=="Jamaica")
	replace lis=1 if (source==5 & resource<4 & country=="Mauritius")
	replace lis=1 if (source_d=="Eurostat Data browser" & country=="Turkey")
	replace lis=0 if (source==1 & resource==4 & country=="West Bank and Gaza")
	replace lis=1 if (source==3 & country=="Guatemala")
	
	gen 	minyear = year
	gen 	maxyear = year

	collapse idseries source lis (min) minyear (max) maxyear , by(country source_d series)
	sort country maxyear minyear
	by country:  gen 		nwiidseries  = _n
	by country:  gen 		maxwiidseries= _N

	gen		maxyearlis0 = 0
	replace maxyearlis = maxyear if lis==1
	by country: egen maxyearlis = max(maxyearlis)
	gen		minyearlis0 = 0
	replace minyearlis = minyear if lis==1
	by country: egen minyearlis = max(minyearlis)
	
	gen reverse = (maxyear > maxyearlis & lis==0 & maxyearlis>0)
	by country: egen		reversec = max(reverse) 		
	by country: egen 		minyearc = min(minyear)
	by country: egen 		maxyearc = max(maxyear)

	sort country nwiids
	by country:  gen 		minyyear = minyear[_n+1]
	by country:  replace	minyyear = min(minyyear, minyear[_n+2]) if lis[_n+1]==1	
	by country:  gen 		maxyyear = maxyear[_n-1]
	drop *lis0
	save `wiidseries' , replace
	
	use  `wiidtemp' , clear

	merge m:1 country source_d series using `wiidseries'
	drop _m

	***********************************************************************
	***********************************************************************
	***********************************************************************
	***********************************************************************
	***********************************************************************
	
	cap drop _m

	merge 1:1 id using percentiles , keepus(p*) update replace
	tab indexd _m , miss
	drop if _m==2
	drop _m
	* India, D&S 2004, we only use gini
	qui replace indexd = 0 if _source_d =="Deininger and Squire, World Bank 2004" & country=="India"
	forvalues i=1/100 {
	qui replace p`i'=. if _source_d =="Deininger and Squire, World Bank 2004" & country=="India"
	}
	
	* When Gini is missing, use the estimated value
	
	preserve
	qui keep if gini==.
	qui keep id p1-p100
	global k = _N
	qui xpose , clear varname
	gen gini = .
	gen id = .
	forvalues i=1/$k {
	qui ineqwiid v`i' if _n>1 , gini
	replace id=v`i'[1] 	 in `i'
	replace gini=round(r(gini)*100, 0.01) in `i'
	}
	keep id gini
	drop if id==.
	su gini
	save `ginie'
	restore
	merge 1:1 id using `ginie' , update
	drop _m

	foreach d in gini $percentiles { 
	di "`d'"
	gen 	 factor`d'   =  0
	gen 	ffactor`d'   =  0
	gen 	`d'_adj = `d'
	lab var `d'_adj "adjusted `d'"
	}

	********************************* Integrating LIS with most recent series if different from LIS

	preserve
	keep if reversec==1
	keep if lis==1 | reverse==1
	keep if (year>=maxyearlis & lis==0) | lis==1 | (source==5 & country=="China") | (source==4 & country=="Panama")

	sort country nwiidseries
	by  country: replace nwiidseries = nwiidseries[_N] +1 if lis==1

	foreach d in $shares { 
	gsort country  year nwiidseries 
	by  country year: replace factor`d'= `d'_adj[2] - `d'_adj[1] 	 if year == maxyearlis & lis==0
	by  country year: replace factor`d'= `d'_adj[1] - `d'_adj[2] 	 if _n==2 			  & lis==0
*	by  country year: replace factor`d'= `d'_adj[2] - `d'_adj[1] 	if factor`d'==. 	  & lis==0 & _n==2
	by  country year: replace factor`d'= . 						 	if year != maxyearlis & lis==0 & _n==1
	gsort country nwiidseries year 
	by  country nwiidseries: egen 	 mfactor`d' = mean(factor`d') 	if lis==0
	by  country nwiidseries: replace factor`d'= mfactor`d' 			if lis==0
	replace	`d'_adj = `d'_adj 	 + factor`d'						if lis==0
	drop mfactor`d'
	replace ffactor`d' = factor`d'
	replace factor`d'  = 0
	}
	
	* For countries with double overlapping after LIS (e.g. Iceland, Serbia)
	
	foreach d in $shares { 
	gsort country  year nwiidseries 
	by  country year: replace factor`d'= factor`d' + ffactor`d'[1] 	if year != maxyearlis & lis==0 & _n==2
	by  country year: replace factor`d'= . 						 	if year != maxyearlis & lis==0 & _n==1
	gsort country nwiidseries year 
	by  country nwiidseries: egen 	 mfactor`d' = mean(factor`d') 	if lis==0
	by  country nwiidseries: replace factor`d'= mfactor`d' 			if lis==0
	replace	`d'_adj = `d'_adj 	 + factor`d'						if lis==0
	drop mfactor`d'
	drop ffactor`d'
	}	
*	edit year nwiidseries lis _idsource gini* *factorgini if country=="Iceland"
*	edit year nwiidseries lis _idsource gini* *factorgini if country=="Serbia"

	* Special cases
	gen redundant = (year==maxyearlis & lis==0)
	by country: replace nwiidseries = nwiidseries[1] if country!="Dominican Republic"
	
	sort country nwiids
	foreach var in lis reverse series idseries idsourced source source_d resource resource_d scale scale_d {
	by country: replace `var' = `var'[_N] if country!="Dominican Republic"	/* LIS, one only year*/
	}


	* Reset factor to 0 in lis and integrated observations
	foreach d in $shares { 
	replace factor`d'= 0 if country!="Dominican Republic"
	}	
	
	save `reverse', replace
	restore

	***************************************************************************************************

	cap drop _m
*	drop if source==1 & (resource==1 | scale==1) & reverse==1
	merge 1:1 country year id using `reverse', keepus(factor* *_adj nwiids lis reverse series idseries idsourced source source_d resource resource_d scale scale_d redundant) update replace 
	drop _m

	* Identifying years (q>1) and series (qq>1) with overlapping series
	
	sort country year nwiidseries maxyear
	by   country year 		: gen  q = _N

	gen n = _n
	
	sort country series q
	by  country series		: gen qq = q[_N]
	sort n

		* Minimum year of overlapping, qyear (years beyond qyear0, will be removed in general)
		
	sort country series year
	by   country series: egen 	qyear0  = min(year) if q>=2 & year>=minyyear
	by   country series: egen 	qyear1  = max(year) if q>=2 & year>=minyyear
	sort country series  qyear0 year
	by   country series: replace qyear0  = qyear0[1]		
	sort country series  qyear1 year
	by   country series: replace qyear1  = qyear1[1]		
	
	preserve
	drop minyear maxyear
	gen minyear = year
	gen maxyear = year
	collapse idseries source lis (min) minyear (max) maxyear , by(country series)
	sort country lis maxyear minyear
	bys country:  gen nwiidseries = _n
	bys country: egen maxnwiidseries = max(nwiidseries)

	sort country nwiids
	bys country:  gen maxyearlis = maxyear[_N]

	bys country:  gen minyyear = minyear[_n+1]
	bys country:  gen maxyyear = maxyear[_n-1]

	* Overall overlapping backwards or forwards
	gen overlapf = (maxyear >= minyyear) & nwiidseries<maxnwiidseries
	gen overlapb = (minyear <= maxyyear) & nwiidseries>1 

	save `wiidseriesT' , replace		
	
	restore
	
	merge m:1 country series using `wiidseriesT' , keepus(nw minyyear maxyyear overl*) update replace
	drop _m


	* Info about overlapping in eacn country
	sort country year nwiidseries maxyear
	by   country year 		: gen  _q = _N
	sort country series _q
	by  country  series		: gen _qq = _q[_N]
	by  country  series 	: gen  xq =(_q>=2)/_q
	by  country 			: egen nq = sum(xq)
	replace nq=round(nq)
	sort country _qq
	by  country 			: gen _cq = _qq[_N]
	sort n
	lab var  nq "Number of overlapping years"
	lab def _cq 1 "no overlapping" 2 "simple overlapping" 3 "complex overlapping"
	lab val _cq _cq

	preserve
	keep if _q>=2
	collapse _q, by(country year)
	bys country: gen iq = _n
	lab var iq "Order of overlapping year"
	save `iq' , replace
	restore

	merge m:1 country year using `iq'
	drop _m
	
	* Countries with no overlapping, 
	tab country if _cq==1
		*** We can automatically set the Gini and shares as the original, no need to run the linking code
		
	* Countries with only simple overlapping years (no q>2)
	tab country _q 	if _q==2
	
	* Countries with multiple overlapping in at least one year	
	tab country _q 	if _q> 2

	* Countries with one only overlapping, 
	tab country if nq==1
	* Countries with multiple overlapping years, 
	tab country if nq>1
	
	* Years with overlapping, ex. Brazil:
	tab year q if q>=2 & country=="Brazil"	

	replace nwiids=1   if source_d=="United Nations 1981" & country=="Turkey"
	replace nwiids=2   if source_d=="Jain 1975"   		  & country=="Turkey"
	replace overlapf=0 if source_d=="Jain 1975"   		  & country=="Turkey"
	replace overlapb=0 if source_d=="United Nations 1981" & country=="Turkey"
	replace overlapf=1 if source_d=="United Nations 1981" & country=="Turkey"
	
	gen 	zz=(source_d=="United Nations 1981" & country=="Turkey")
	replace zz=2 if (source_d=="Shirahase 2001" & country=="Japan") | (source==5 & country=="Pakistan")
	replace zz=3 if (source_d=="Lachman and Bercuson 1992" & country=="South Africa")
	replace zz=4 if (inlist(source_d, "Leibbrandt et al. 2010", "Leibbrandt et al. 2009", "Whiteford and Van Seventer 2000") | (source_d=="PovcalNet" & year<2010) ) & country=="South Africa"

	sort country year nwiidseries
	foreach d in $shares { 
	by  country year: replace factor`d'= `d'_adj[_n+1]-`d'_adj[_n] if _n>=1 & _n<_q &_q>=2 & nwiids<maxwiids & lis==0 
	by  country year: replace factor`d'= 0 if zz==4	
	by  country year: replace factor`d'= 0 if factor`d'==.
	}
	sort year

	sort country nwiidseries _qq year 
	foreach d in $shares { 
	by  country nwiidseries _qq: replace factor`d'= factor`d'[1] 	if nwiids<maxwiids & _qq>=2 & lis==0 & zz==1
	by  country nwiidseries _qq: replace factor`d'= factor`d'[_N-1]	if nwiids<maxwiids & _qq>=2 & lis==0 & zz==2
	by  country nwiidseries _qq: replace factor`d'= factor`d'[_N-3]	if nwiids<maxwiids & _qq>=2 & lis==0 & zz==3
	by  country nwiidseries _qq: replace factor`d'= factor`d'[_N] 	if nwiids<maxwiids & _qq>=2 & lis==0
	}
	
	* Applying the correction factor

	preserve
*	keep if (nq>=2 & _cq<=3) | _cq==3
	cap drop n
	drop if source==7 & country=="South Africa"
	gen n=1
		* check min and max give same result ...
	collapse (max) source resource (mean) nq _cq scale scale_d _q _qq factor* nwiids overlapf overlapb maxyear minyear idseries (sum) n, by(country source_d series)	
	gsort country -nwiids
	foreach d in $shares {
	gen ss`d' = 0
	bys country : replace ss`d' = factor`d' 
	bys country : replace ss`d' = factor`d' + ss`d'[_n-1] if overlapf==1 & _n>1
	}
	forvalues i=2/10 {
	bys country : replace resource  = resource[_n-1] if overlapf==1 & _n==`i'
	bys country : replace scale  	= scale[_n-1] 	 if overlapf==1 & _n==`i'
	bys country : replace scale_d  	= scale_d[_n-1]  if overlapf==1 & _n==`i'
	}
	
	save `ssfactor' , replace
	restore
	
	cap drop _m
	merge m:1 country source_d series using `ssfactor'  , keepus(ss* resource scale scale_d) update replace
	
	* Applying the correction factor for Gini and shares

	foreach d in $shares { 
	replace ss`d' = factor`d' if ss`d'==.
	replace	`d'_adj = `d'_adj + ss`d'   
	}

	* We cannot adjust shares (Bulgaria: UNICEF to Eurostat; HK NSA) (overlapping years do not have shares)
	forvalues i=1/100 {
	replace	p`i'_adj = p`i' if country=="Bulgaria"  & source==4
*	replace	p`i'_adj = p`i' if country=="Hong Kong" & source==5 & _resource==1 & _scale==3 /* no change */
	}

	sort country year
*	edit _source_d year gini* factorgi ssgini _q _qq nq overl* if country=="Panama"  & year>2010

	* Checking overlapping adjustments worked
	sort country iq nwiids
	by country iq : gen xx = gini_adj[2] - gini_adj[1] if _q==2
	replace xx=(abs(xx)>.1 & _q==2)
	
	by country iq : gen yy1 = gini_adj[3] - gini_adj[1] if _q==3
	by country iq : gen yy2 = gini_adj[2] - gini_adj[1] if _q==3
	by country iq : gen yy3 = gini_adj[3] - gini_adj[2] if _q==3
	gen yy= ((abs(yy1)>.1 | abs(yy2)>.1 | abs(yy3)>.1) & _q==3)

	recode xx yy* (.=0)
	
	tab country if xx==1 
	tab country if yy==1
	
	sort country year nwiids

	foreach var in series idseries idsourced source source_d {
	by country:  replace `var' = `var'[_N]	
	}
	
	replace resource  	= 1 if country=="South Africa" | country=="China"	
		/* SA, with overlapping; China, no adj for gross Dowling and Soo */
		

	lab val _source source
	lab val _resource resource
	lab val _scale scale
	lab val _idsourced idsourced
	
	cap drop _m
	
	preserve
	keep id wiidcompanion shareseries giniseries
	lab data "Identifying different series types"
	save seriestype, replace
	restore
	lab data "WIID C: Adjusted in phase 1, with overlapping observations"
	save  wiid1, replace

********************************************************************************************************
*********	Note that scale is 3 for Hong Kong shares and 1 for Gini (--> adj. to obs. with wiidc==2)
*********   China: with Ravallion and Chen or with WB (not both)      ----> giniseries vs shareseries
*********	South Africa with Leibbrandt et al or with WB (not both)  ----> giniseries vs shareseries
********************************************************************************************************

	* Reomoving overlapping years (redundant)
	
	drop if wiidcompanion==2
	cap drop _m
	
	gen 	d 		= gini_adj - gini
	
	su d if d==0
	su d if d!=0	
	
	gen adjustment  = (d!=0)
	lab var adjustment "Adjusted in phase 1"

	lab data "WIID C: Adjusted in phase 1, without overlapping observations"
	save wiid2, replace
	
	
