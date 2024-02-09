
	cd 	"...\Data"

	* ADO file that will be used to compute ienquality measures
	do  "...\Dofiles\ineqwiid.ado"
	
	* Country fractiles
	global cd 	 = 100
	* Global fractiles
	global gd 	 = 100

	* Period of time	
	global t0 1950
	global t1 2019
	global period $t0 (1) $t1
	
	
	tempfile former former2 impute newidc cpercentile0 cpercentile00 countrydistribution countrydistribution2a population0 population1 population _gdp0 _gdp1 globaldistribution0 globaldistribution0_wide atkinsonw atkinsonb atkinson atkinsonww ineqindices globaldistribution1_wide globaldistribution2 globaldistribution3 ineqwsc ineqwsreg ineqwsig ineqwsglobal globaldistribution4 shapley globaldistribution5 globaldistribution idwiid 

	************************************************************* Bringing WIID with final income distributions
	
	cap label drop all
	use "final\wiidcountry", clear		
	drop if p50==.		/* using only observations with income shares */
	keep id c2 c3 country year p1-p100 incomeg region* eu former histent

	gen sample=1

	**********************************************************************************	
	*********************************	A. Preparation of data ***********************
	**********************************************************************************	

	******************************* 1. Population (UN)
	
	**** Bringing population from WIID metadata first
	merge m:m c3 year using "metadata\metadata" 			, keepus(c2 c3 population year) update 
	drop _merge
	su population
	
	* Bringing UN Population estimates (x 1,000) for missing countries
	merge m:m c3 year using "metadata\populationun.dta" 	, keepus(c3 population year countryun) update 
	drop _merge
	drop if c3==""
	su population

	drop if year>$t1
	drop if year<$t0-5

	lab var population "Country population, thousands"
		
	tab country if countryun == "" & year>=1950
*	tab c3		if country   == "" & year>=1950
	
	cap drop _merge
	gen unsample=(countryun!="" & c3!="   " )

	* Check all countries have population in same units (especially historical units, former Yugoslavia, etc.
	

	*************************** 2.Integrated series GDPpc (WB, Maddison, PWT)

	********************* Created with gdp.do

	merge m:m c3 year using "metadata\gdp.dta" , keepus(c3 year gdp country* pop*) update

	keep if _m!=2 | inlist(countrywb, "Aruba", "Bermuda", "Cayman Islands", "Curacao", "Korea, Dem. People’s Rep.", "Kosovo", "Liechtenstein", "Monaco") | inlist(countrymad ,"Czechoslovakia" , "Former USSR", "Former Yugoslavia" ) | inlist(countrypwt, "Anguilla", "Aruba", "Bermuda")
	
	***************************** 3. Income group and region, updating some missing info (WB)

	cap drop _merge	
	merge m:m c3 using "metadata\incomeg", keepus(_region_wb _incomeg Economy) update
	drop 	if _m==2
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
	
	drop _income Economy _region_wb _m	

	sort c3  country
	by   c3: replace country=country[_N] if country[_N]!=""
	replace country=countrywb  			if country==""
	sort c3  country
	by   c3: replace country=country[_N] if country[_N]!=""
	replace country=countrymad 			if country==""
	sort c3  country
	by   c3: replace country=country[_N] if country[_N]!=""
	replace country=countrypwt 			if country==""
	sort c3  country
	by   c3: replace country=country[_N] if country[_N]!=""
	tab country if year>=1950, miss
	*	Korea, Dem. People’s Rep. , Cayman Islands, Curacao, ... since 1960

	replace country="Yugoslavia" 	if c3=="YUG"
	replace country="Soviet Union" 	if c3=="SUN"
		
	sort c3  region_wb
	by   c3: replace region_wb=region_wb[1]
	sort c3  region_un
	by   c3: replace region_un=region_un[1] 
	sort c3  region_un_s
	by   c3: replace region_un_s=region_un_s[1] 
	sort c3  eu
	by   c3: replace eu=eu[1] 
	sort c3  incomeg
	by   c3: replace incomeg=incomeg[1] 
	
	replace region_wb=2 if country=="Anguilla"
	replace region_wb=2 if country=="Montserrat"
	replace region_wb=5 if country=="Réunion"

	replace incomeg=1 if inlist(country, "Anguilla", "Montserrat", "Réunion") & incomeg==.

	replace incomeg=5 if incomeg==.
*	lab def incomegroup 5 Missing , add


	su year if p50!=. & country=="Eritrea"
	su year if p50!=. & country=="Ethiopia"
	su year if p50!=. & country=="Sudan"
	su year if p50!=. & country=="South Sudan"
	su year if p50!=. & country=="Bangladesh"
	su year if p50!=. & country=="Pakistan"
	
	* Note:
		* Ethiopia (1996->), Eritrea (1997), no need to inpute
		* South sudan (2009), Sudan (1969->)
		* Bangladesh and Pakistan (both from 1963), no need to inpute
		
		
	***************************** 4. Historical entities
	
	* Note: Overlapping Soviet Union, Czechoslovakia, Yugoslavia, Serbia and Montenegro, Ethiopia/Eritrea, Sudan/South Sudan,  

	* Historical entities 
		
		* Czechoslovakia until 1991, even if independence was in 1993, for the consistency of the LIS series
		* For Yugoslavia, no information on distribution, we need to consider the independent countries from the beginning (only conflict would be Croatia and Solvenia in 1987-88, while Yugoslavia is in 1988 and 1991)
		* No information on GDP for "Andorra" (taken from France), "Eritrea" (taken from Ethiopia), "Greenland" (from Denmark), "South Sudan" (taken from Sudan)
		
	gen 	former2 = former
	recode  former2 (.=0)
	replace former2=6 if inlist(country, "Denmark", "Greenland")
	replace former2=7 if inlist(country, "Andorra", "France")
	lab def former 5 "Ethiopia" 6 "Denmark(aux)" 7 "France(aux)" , modify
	lab val former2 former
	lab var former "Former entity"
	sort country former2
	by   country: replace former2 = former2[_N]

	* For distribution and GDP
	preserve
	keep if histent==1 | country=="Sudan"
	drop if country=="Sudan" 					& year>=2009
	drop if country=="Yugoslavia" 				& year>=1997
	drop if country=="Serbia and Montenegro" 	& year< 1997
	keep if former2!=.
	save `former', replace
*	save  former, replace
	restore

	* For only GDP
	preserve
	keep if country=="Sudan" | country=="Ethiopia" | country=="Denmark" | country=="France"
	keep if gdp!=.
	save `former2', replace
*	save  former2, replace
	restore

	merge m:1 former2 year using `former'  , update keepus(gdp p*)
	drop _m
	merge m:1 former2 year using `former2' , update keepus(gdp)
	drop _m

	* Unifying the name for N Korea and Macao 
	replace country="Korea, DPR" if c3=="PRK"
	replace country="Macao"		 if c3=="MAC"
	
	cap drop region
	gen 	region = region_wb
	replace region = 30 if former==3 & eu==0 | (country=="Soviet Union")
	lab def region_wb 30 "non EU former Soviet Union" , modify
	lab val region region_wb

	replace incomeg=2 if country=="Soviet Union"

	tab country if region ==.
	tab country if incomeg==.

	drop if inlist(country, "Czechoslovakia", "Serbia and Montenegro", "Soviet Union", "Yugoslavia")
							/* dropping Czechoslovakia, Serbia and Montenegro , and Yugoslavia*/

	***************************** 5. Treating missing information (interpolation, extrapolation)

	* Populating some missing population numbers with WDI, maddison or PWT, make sure they are expressed in same units

	merge m:1 c3 year using "metadata\population_wb"
	drop _merge
	
	replace population = population_wb 		if population==.
	replace population = popmaddison*1000	if population==.
	replace population = poppwt*1000000		if population==.

	* Keeping most recent GDP and population if last ones are missing
	sort country year
	by  country: replace gdp = gdp[_n-1] if gdp==. & gdp[_n-1]!=.
	by  country: replace population = population[_n-1] if population==. & population[_n-1]!=.
	* Extrapolate (Curacao)
	su population if country=="Curacao" & year==2005
	replace population = r(mean) if country=="Curacao" & year>=2000 & year<=2004 & population==.
	
*	drop miny* maxy*
	sort country year
	gen nonmiss = .
	gen yeargap = .
	qui by 	 country : egen _minyear = min(year) if p50!=.
	qui by 	 country : egen  minyear = mean(_minyear)
	qui by 	 country : egen _maxyear = max(year) if p50!=.
	qui by 	 country : egen  maxyear = mean(_maxyear)
	forvalues i==1/100 {
		di "Interpolation: fractile `i' / $cd "
		qui gen  minp`i' = p`i' if year==minyear
		qui sort country minp`i'
		qui by 	 country:  replace minp`i' = minp`i'[1]
		qui gen  maxp`i' = p`i' if year==maxyear
		qui sort country maxp`i'
		qui by 	 country : replace maxp`i' = maxp`i'[1]
		qui by 	 country : replace p`i' = minp`i' if year<minyear 
		qui by 	 country : replace p`i' = maxp`i' if year>maxyear
		sort country year
		qui by country: ipolate p`i' year if year>=minyear & year<=maxyear, gen(ip`i')
		qui replace ip`i' = p`i' if (year<=minyear | year>=maxyear)
		lab var ip`i' "percentile `i'"
	}

	gen 	interpolated = (id==.)
	replace interpolated = 2 if (year<minyear | year>maxyear)
	lab def interpolated 0 No 1 Interpolated 2 Extrapolated 3 Imputed 4 Mean
	lab val interpolated interpolated
	lab var interpolated "Obs. has been interpolated?"
	tab inter
	
	cap drop p1-p100
	ren ip* p*
	
	***************************** 6. Country fractiles, wide format	--> cpercentile0.dta
		* pi  (share, percentile) , y_i (income),  
		* ddi (share, new, any level of aggregation $cd)
		* Interpolation
	
	global cdd = 100/$cd
	
	* If N country fractiles is 100, take p1...p100 for ddi
		* Setting Minimum value of p> to 0.01, re-normalziaign to add up to 1

	gen sdd = 0
	if $cd==100 {

		forvalues i=1/100 {
			qui gen  	dd`i'=p`i'
			qui replace dd`i'=.01 if dd`i'<.01
			qui replace sdd=sdd + dd`i'	
		}
	}
	
	* If N country fractiles is < 100, aggregate
	else {
	forvalues i=1/$cd {
		qui gen dd`i' = 0
		local r = (`i'-1)*$cdd+1
		local g = `i'*$cdd
		di "`r'_`g'"
		forvalues s=`r'/`g' {
			qui replace dd`i'=dd`i' + p`s'
		}
		qui replace dd`i'=.01 if dd`i'<.01
		qui replace sdd=sdd + dd`i'	
	}
	}
	
	forvalues i=1/$cd {
		qui replace dd`i'=(dd`i'/sdd)*100
	}

	
	sort country year
	gen missy = (dd50==.)
	bys c3: egen minmissy=min(missy)
	
	* Cases with at least gdp
	su dd* gdp if minmissy==1 & gdp!=.
	
	* Imputing by region and income group to those cases with gdp, missing distribution
		** We impute all except North Korea --> all percentile with country's mean income

	drop if country==""
		
	gen missdd  = (dd5 ==.)
	gen missgdp = (gdp ==.)
	gen nyear	= (year!=.)
	sort country year
	* No gdp observation
	by country: egen smissgdp = sum(missgdp) 
	by country: egen syear    = sum(nyear) 
	tab country if   smissgdp == 70	/* Somalia and Réunion */
	
	preserve 
	keep year region_wb incomegroup gdp dd* p* population
	collapse dd* p* gdp [iw=population] , by(year region_wb incomegroup)
	ren gdp gdpri
	save `impute' , replace
*	save  impute  , replace
	restore
	cap drop _m
	merge m:1 year region_wb incomeg using `impute' , keepus(dd* p* gdpri ) update 
*	replace interpolate = 3 if _merge==4 & missdd==1
	tab country if smissgdp == syear
	* 	gdp from region and income group (only listed countries, not other territories)
*	replace interpolate = 3 if smissgdp == syear & inlist(country, "Liechtenstein", "Monaco", "Somalia")
	replace gdp = gdpri 	if smissgdp == syear & inlist(country, "Liechtenstein", "Monaco", "Somalia")
	
	forvalues i=1/$cd {
		qui replace interpolate = 4 if dd`i'==. & gdp!=.
		qui replace	p`i'  = 1/$cd	if dd`i'==. & gdp!=.
		qui replace	dd`i' = 1/$cd 	if dd`i'==. & gdp!=.
	}

	* Extending missing GDP backwards with region and income group
	
		* Minimum and maximum year for each gdp series

	cap drop miny _miny
	bys  c3 : egen _miny = min(year) if gdp!=.
	bys  c3 : egen  miny = mean(_miny)
	su miny

	cap drop maxy _maxy
	bys  c3 : egen _maxy = max(year) if gdp!=.
	bys  c3 : egen  maxy = mean(_maxy)
	su maxy

	gen 	ratiori = gdp/gdpri if year==miny
	sort c3 ratiori
	bys  c3: replace ratiori=ratiori[1]
	replace	gdp = gdpri*ratiori if gdp==. & year<miny

	drop ratiori
	
	gen 	ratiori = gdp/gdpri if year==maxy
	sort c3 ratiori
	bys  c3: replace ratiori=ratiori[1]
	replace	gdp = gdpri*ratiori if gdp==. & year>maxy

	* Maldives, only country in region/income group, we keep gdp constant earlier years
	
	sort country year
	by country : gen  gdpmin =  gdp 	   if year==miny 
	by country : gen  gdpmax =  gdp 	   if year==maxy 
	by country : egen gdpmean = mean(gdpmin) 
	by country : egen gdpmean2= mean(gdpmax) 
	by country : replace gdp = gdpmean  if gdp==. & year<miny	/* Maldives*/
	by country : replace gdp = gdpmean2 if gdp==. & year>maxy	/* none */
	
	drop gdpmin gdpmax gdpmean*
	
	* Extending forwards if necessary
	
	cap drop maxy
	sort country year
	by country: gen maxy = year[_N]
	tab country maxy if maxy<$t1

	su year if maxy<$t1
	if r(N)>0 {
	local j = $t1 - r(max)
	forvalues i=1/`j' {
	cap drop dup*
	expand 2 if maxy<$t1 & year==maxy, gen(dupindicator)	
	tab country year if dup==1
	replace year = maxy	+ 1 if dup==1
	sort country year
	by country: replace maxy = year[_N]
	}
	}

	* Extending backwards
	
	cap drop miny
	sort country year
	by country: gen miny = year[1]
	tab country miny if miny>$t0

	su year if miny<$t0
	if r(N)>0 {
	local j = r(max)-$t0
	forvalues i=1/`j' {
	cap drop dup*
	expand 2 if miny>$t0 & year==miny, gen(dupindicator)	
	tab country year if dup==1
	replace year = miny	- 1 if dup==1
	sort country year
	by country: replace miny = year[1]
	}
	}
	
	* Still some missing (they are not going to be included)
	
	tab country if gdp==.
	tab country if population==.

	* From relative income to real incomes (x GDPpc, PPP): y1...100
	
	forvalues i=1/$cd {
		cap drop y_`i' 
		qui gen	 y_`i' = dd`i'*gdp
	}

	tab country 							  if minmissy==1 & p50==. & population!=. & gdp!=.
*	edit country population gdp p50p y_50 gdp if minmissy==1 & p50==. & population!=. & gdp!=. & year==2010
	
	* We do not do anything if still missing population and/or gdp
	tab country 								if minmissy==1 & p50==. & (population==. | gdp==.)
*	edit country population gdp p50p gdp if minmissy==1 & p50==. & (population==. | gdp==.) & year==2010

	* Remaining (partial) missing countries, generally small, we do nothing?
	tab      country if y_50==. & year>=1950 & year<=2018 & minmissy==0
	tab year country if y_50==. & year>=1950 & year<=2018 & minmissy==0
*	edit country year population gdp p50p y_50 gdp if  y_50==.
	
	cap drop weight
	gen weight = population/$cd

	su dd* y_* gdp [aw=weight]
	
	* Countries with still some missing information

	tab country year if (gdp==. | population==.) & year>=1980
	
	* Aruba (<1989), Bermuda, Caiman, Curacao (<1999), Korea Dem. People’s R, Kosovo (<1999), Liechtenstein, Monaco, ...

	cap label drop pp

	
	* Deleting historical entities
	drop if country=="Yugoslavia" 				& year>=1997
	drop if country=="Serbia and Montenegro" 	& year< 1997

	
	* New idcountry, numerical country code
	preserve
	keep if y_50!=. & country!=""
	gen idcountry = 1
	collapse idc , by(country c3)
	sort c3
	replace idc=_n
	save `newidc', replace
	restore
	cap drop _merge
	merge m:1 c3 country using `newidc'
	drop _merge
	
	sort c3 c2
	by c3: replace c2=c2[_N] 
	
	* Excluded territories
	tab country if gdp==.
	drop if gdp==.
	* Changing name
	replace country = "Saint Kitts and Nevis" if country == "St. Kitts and Nevis"
	replace country = "Saint Vincent and the Grenadines" if country == "St. Vincent and the Grenadines"
	
	recode former (.=0)
	keep id country idcountry c2 c3 year region* incomeg former gdp population interpolated p* dd* y_* weight
	lab var id "WIID identifier for survey-year obs."
	lab data "Country-level percentile distribution, wide format"
	save `cpercentile0' , replace
*	save  cpercentile0  , replace
	
	**********************************************************************************	
	*********************************	B. Estimating and reporting ******************
	**********************************************************************************	

	***************************** 7. Estimating global and country inequality indices, matrices ineq*
		
		* Estimating inequality using ineqwiid.ado	
		* Using long format with country -> year -> cgroup ------> cpercentile00.dta
		* cgroup (country fractiles)
	
	* Checking list of economies
	use `cpercentile0' , clear
	tab country
	
	drop if y_50==.
	
	* Reshaping from wide to long (cgroup) format for estimation
	drop if country==""
	keep country c3 idc region* incomeg former y_* dd* gdp population weight year
	ren dd* p*
	reshape long y_ p  , i(country year) j(cgroup)
	cap label drop pp
	ren y_ y


	gen 		ggdp = .
	forvalues j=$period {
	qui su  y [aw=weight] 						if year==`j'
	qui replace	ggdp = r(mean)				 	if year==`j'
	}
	
	qui gen	yw 	 = y*ggdp/gdp
	qui gen	ywa  = y + ggdp - gdp
	
	su y yw ywa gdp ggdp [aw=weight] if year==$t1

	lab val cgroup	
	lab var cgroup 		"Country fractile"
	lab var p 			"Country fractile's income share"
	lab var y  			"Fractile´s per capita income"
	lab var yw 			"Within-country distribution (Ib=0)"
	lab var ywa			"Within-country distribution (Ib=0, abs.)"
	lab var gdp			"Country GDP 2017US (thousands)"
	lab var ggdp		"Global GDP 2017US (thousands)"
	lab var weight  	"Fractile's population"
	lab var population  "Country's population"
	lab data "Country-level percentile distribution, long format"
	save `cpercentile00' , replace
*	save  cpercentile00  , replace
	

	use `cpercentile00', clear
		
	mat mean = J(1,1,.)
	mat pop  = J(1,1,.)
	mat percentile = J($cd , 1, .)
	mat percentileb= J($cd , 1, .)
	* Just to save matrix rownames
	qui ineqwiid y [aw=weight] if year==$t0 , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineq 	= r(ineq)*100
	mat ineqreg	=   ineq
	mat ineqig	=   ineq
	mat ineqb	=   ineq
	mat ineqws 	=   ineq
	mat ineqw 	=   ineq
	mat ineqa	=   J(3,1,.)
	mat ineqareg=   J(1,1,.)
	mat ineqaig	=   J(1,1,.)

	gen group  		= .
	lab var group  		"Global fratile"
	gen groupb 	 	= .
	lab var groupb 		"Global between-country fratile"
	gen groupw 	 	= .
	lab var groupw 		"Global within-country fratile"
	gen groupwa 	 	= .
	lab var groupwa		"Global within-country fratile, abs."
	gen groupreg  	= .
	lab var groupreg  	"Region fratile"
	gen groupig  	= .
	lab var groupig   	"Income group fratile"

	qui su idcountry,
	global nc = r(max)
	
		* Global measures and distribution (runs faster if separately by year)
	
	forvalues j=$period   {
	di as result "_______________________________ Global inequality measures, Year=`j'"
	preserve
	qui keep if year==`j'
	
	qui su ggdp
	scalar ggdp0 = r(mean)
	mat mean = mean, ggdp0

	qui su population  if cgroup==1, d
	scalar pop0=r(sum)
	mat pop  = pop , pop0
	
	di as result "_______________________________ All"
	ineqwiid y 	 		 [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineq=ineq  , r(ineq)*100
	scalar ineqa1=r(gini)*100*ggdp0

	
	di as result "_______________________________ By region"
	forvalues i=1/7 {
	di "region `i'"
	qui su  gdp 	  [aw=weight] if cgroup==1 & region_wb==`i'
	scalar  rgdp0 = r(mean)
	ineqwiid y if region_wb==`i' [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineqreg=ineqreg  , r(ineq)*100
	scalar ineqa1`i'=r(gini)*100*rgdp0
	}
	
	di as result "_______________________________ By income group"
	forvalues i=1/4 {
	di "income group `i'"
	qui su  gdp 	  [aw=weight] if cgroup==1 & incomegroup==`i'
	scalar  igdp0 = r(mean)
	ineqwiid y if incomeg==`i' [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineqig=ineqig  , r(ineq)*100
	scalar ineqa2`i'=r(gini)*100*igdp0
	}

	di as result "_______________________________ Between countries"
	ineqwiid gdp  		 [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineqb=ineqb, r(ineq)*100
	scalar ineqa2=r(gini)*100*ggdp0
	
	di as result "_______________________________ Within countries"
	ineqwiid yw  		 [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineqw0  = r(ineq)*100
	mat ineqc`j' = (0 , r(ineq)'*100 )
	ineqwiid ywa  		 [aw=weight] , gini variance
	mat ineqw0[2,1] = r(variance)*100
	mat ineqw=ineqw, ineqw0
	scalar gini = r(gini)
	scalar ineqa3=r(gini)*100*ggdp0

	di as result "_______________________________ Country inequality"
*	mat ineqac`j' = J(1,1,. )
	forvalues i=1/$nc {
	di "`j'_`i'"
	qui su y if idc==`i' [aw=weight] 
	if r(N)>0.5  {
*	tab country year   if idc==`i'
	qui ineqwiid y if idc==`i' [aw=weight] , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
	mat ineqc`j'= ineqc`j' \ (`i', r(ineq)'*100 )
	}
	}
		
	mat ineqc`j' = ineqc`j'[2...,1...]
	mat ineqa 	 = ineqa 	, (ineqa1  \ ineqa2  \ ineqa3)
	mat ineqareg = ineqareg , ineqa11 , ineqa12 , ineqa13 , ineqa14 , ineqa15 , ineqa16 , ineqa17
	mat ineqaig  = ineqaig  , ineqa21 , ineqa22 , ineqa23 , ineqa24
	
	***************************** 8. Estimating global fractiles group* ---> countrydistribution
		* cgroup	Country fractile
		* group		Global fratile
		* groupb	Global between-country fratile (Iw=0)
		* groupw	Global within-country fratile (Ib=0)
		* groupw	Global within-country fratile (Ib=0, abs)
		* groupreg	Region fratile
		* groupig	Income group fratile

	di as result "_______________________________ Global distribution, Year=`j'"
	sort y , stable
	di as result "_______________________________ All"
	qui bys  year:  gen  sweight = sum(weight)
	qui bys  year: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	group = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	qui	tabstat y	[aw=weight]  , by(group)
	tab  group		[aw=weight]  

	sort gdp , stable
	di as result "_______________________________ Between countries (Iw=0)"
	drop sweight ssweight
	qui bys  year:  gen  sweight = sum(weight)
	qui bys  year: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	groupb = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	qui	tabstat	gdp	[aw=weight]  , by(groupb)
	tab  groupb	[aw=weight]  

	sort yw , stable
	di as result "_______________________________ Within countries (Ib=0)"
	drop sweight ssweight
	qui bys  year:  gen  sweight = sum(weight)
	qui bys  year: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	groupw = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	sort ywa , stable
	di as result "_______________________________ Within countries (Ib=0, abs)"
	drop sweight ssweight
	qui bys  year:  gen  sweight = sum(weight)
	qui bys  year: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	groupwa = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	tab  groupw [aw=weight]  

	sort y , stable
	
	di as result "_______________________________ Region"
	drop sweight ssweight
	sort year region_wb y , stable
	qui by  year region_wb:  gen  sweight = sum(weight)
	qui by  year region_wb: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	groupreg = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	tab  groupreg [aw=weight]  
	
	di as result "_______________________________ Income group"
	drop sweight ssweight
	sort year incomeg y , stable
	qui by  year incomeg:  gen  sweight = sum(weight)
	qui by  year incomeg: egen ssweight = sum(weight)
	qui replace sweight = (sweight / ssweight)*100
	local i = 1
	forvalues g=1/$gd {
		qui replace	groupig = `i' if sweight>(100/$gd)*(`i'-1) & sweight<=(100/$gd)*`i'
		local i = `i'+1
	}

	tab  groupig [aw=weight]  

	lab data "Country-level percentile distribution, long format year ´j'"
	tempname cpercentile`j'
	save `cpercentile`j'' , replace
	restore
	}	

	cap drop _m
	forvalues j=$period   {
	merge 1:1 c3 year cgroup using `cpercentile`j'' , keepus(group*) update replace
	drop _m
	}
	su 		group

	* In case N Country Groups ($cd) <100
	replace group =group*100/r(max)
	replace groupb=groupb*100/r(max)
	replace groupw=groupw*100/r(max)
	replace groupwa=groupwa*100/r(max)
	
	cap lab drop pp
	lab data "Country distributions, fractile groups, long format"
	save `countrydistribution', replace
*	save  countrydistribution , replace

	***************************** 9. Country indices to annual files ---> countryindices`j'.dta

	use `countrydistribution', clear
	forvalues j=$period   {
	clear
	svmat	ineqc`j' , names(xx)
	ren 	xx1  idcountry
	ren 	xx2  gini
	ren 	xx3  variance
	ren 	xx4	 theilm1
	ren 	xx5	 theil0
	ren 	xx6  theil1
	ren 	xx7  theil2
	ren 	xx8  atkinson025
	ren 	xx9  atkinson050
	ren 	xx10 atkinson075
	ren 	xx11 atkinson1
	ren 	xx12 atkinson2
	gen 	year=`j'
	tempfil  countryindices`j'
	save 	`countryindices`j'' , replace
	}

	***************************** 10. Putting together country indices and distribution ----> countrydistribution2a

	use `countrydistribution', clear
	drop if idc==. /* dropping territories we are not going to use */
	reshape wide y yw* p group*  , i(c3 year) j(cgroup)
	ren year _year
	cap drop _m
	ren _year year
	forvalues j=$period   {
	merge 1:1 idcountry year using `countryindices`j'' , update
	cap drop _m
	}
	
	gen  ginia = gini*gdp
	gen  sd	  = variance^.5
	drop weight 
	drop variance
	
*	tabstat gdp gini* theil* atk* [aw=population] if year>=1988, by(year)
	
	cap drop _merge
	merge 1:1 c3 year using `cpercentile0' , update keepus(interpo)
	keep if _merge==3
	drop _merge
	lab var idc "Country numerical code"
	order idcountry country c3 year region* incomeg* former interp gdp population gini theil* atk* ginia sd y* p* group*

	* Rounding to exactly zero when there is no country inequality
	foreach i in gini theilm1 theil0 theil1 theil2 atkinson025 atkinson050 atkinson075 atkinson1 atkinson2 ginia sd {
	qui replace `i'=0 if `i'<.00001	
	}

	lab data "Country indices"
	save `countrydistribution2a', replace	
*	save  countrydistribution2a , replace	
	
		
	**********************************************************************************	
	*********************************	C. Global distribution dataset ***************
	**********************************************************************************	

	***************************** 11. Population and GDP by area, subarea ---> population*, _gdp*
																		* ---> globaldistribution0
	use `countrydistribution', clear
	drop if group==.	/* (Italy and US <1950; Kosovo) */
			
			* Population by area
			
	preserve
	collapse (sum) weight  , by(year)
	ren weight population
	gen area	= 0 
	gen decomposition = 0
	save `population0', replace
	restore
	
	preserve
	collapse (sum) weight  , by(year region_wb)
	ren weight population
	gen subarea = region_wb + 10
	gen area = 1
	gen decomposition = 0
	save `population1', replace
	restore
	
	preserve
	collapse (sum) weight  , by(year incomeg)
	ren weight population
	gen subarea = incomeg + 20
	gen area = 2
	lab var area 	"area"
	lab var subarea	"subarea"
	gen decomposition = 0
	append using `population1' 
	save `population1', replace
	restore
	
			* Population by global fractiles
			
	preserve 
	collapse (sum) weight  , by(year group)
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 0
	save `population', replace
	restore

	preserve
	collapse (sum) weight  , by(year groupb)
	ren groupb group
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 1
	append using `population'	
	save `population', replace
	restore

	preserve
	collapse (sum) weight  , by(year groupw)
	ren groupw group
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 2
	append using `population'	
	save `population', replace
	restore

	preserve
	collapse (sum) weight  , by(year groupwa)
	ren groupwa group
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 8
	append using `population'	
	save `population', replace
	restore
	
	preserve 
	collapse (sum) weight  , by(year region_wb groupreg)
	ren groupreg group
	ren region_wb subarea
	replace subarea = subarea + 10
	gen area = 1
	gen decomposition = 0
	append using `population'	
	save `population', replace
	restore

	preserve 
	collapse (sum) weight  , by(year incomeg groupig)
	ren groupig group
	ren incomeg subarea
	replace subarea = subarea + 20
	gen area = 2
	gen decomposition = 0
	append using `population'
	save `population', replace
	restore
	
			* GDP by area
			
	preserve
	collapse (mean) gdp [aw=weight] , by(year)
	gen area	= 0 
	gen decomposition = 0
	save `_gdp0', replace
	restore

	preserve
	collapse (mean) gdp [aw=weight] , by(year region_wb)
	ren region_wb subarea
	replace subarea = subarea + 10
	gen area = 1
	gen decomposition = 0
	save `_gdp1', replace
	restore
	
	preserve
	collapse (mean) gdp [aw=weight] , by(year incomeg)
	ren incomeg subarea
	replace subarea = subarea + 20
	gen area = 2
	gen decomposition = 0
	append using `_gdp1'
	lab var area 	"area"
	save `_gdp1', replace
	lab var subarea	"subarea"
	restore

		* Mean income by fractile and area
	
	preserve 
	collapse (mean) y [aw=weight]   , by(year group)
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 0
	save `globaldistribution0', replace
	restore

	preserve
	drop if groupb==.
	collapse (mean) gdp [aw=weight]  , by(year groupb)
	ren groupb group
	ren gdp y
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 1
	append using `globaldistribution0'
	save `globaldistribution0', replace
	restore

	preserve
	drop if groupw==.
	collapse (mean) yw [aw=weight]  , by(year groupw)
	ren groupw group
	ren yw y
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 2
	append using `globaldistribution0'
	save `globaldistribution0', replace
	restore
	
	preserve
	drop if groupwa==.
	collapse (mean) ywa [aw=weight]  , by(year groupwa)
	ren groupwa group
	ren ywa y
	gen area 	= 0
	gen subarea = 0
	gen decomposition = 8
	append using `globaldistribution0'
	save `globaldistribution0', replace
	restore
	
	preserve 
	collapse (mean) y [aw=weight]  , by(year region_wb groupreg)
	ren groupreg group
	ren region_wb subarea
	replace subarea = subarea + 10
	gen area = 1
	gen decomposition = 0
	append using `globaldistribution0'
	save `globaldistribution0', replace
	restore

	preserve 
	collapse (mean) y [aw=weight]  , by(year incomeg groupig)
	ren groupig group
	ren incomeg subarea
	replace subarea = subarea + 20
	gen area = 2
	gen decomposition = 0
	append using `globaldistribution0'
	lab def area 0 World 1 Region 2 "Income group" 3 "Country"
	lab val area area

	lab def subarea 0 "World" 11 "North America" 12 "Latin America and the Caribbean" 13 "Europe and Central Asia" 14 "Middle East and North Africa" 15 "Sub-Saharan Africa" 16 "South Asia" 17 "East Asia and the Pacific" 21 "High income" 22 "Upper middle income" 23 "Lower middle income" 24 "Low income" 25 "Missing income group" 300 "Country", replace
	
	lab val subarea subarea
	lab def decomposition 0 none 1 between 2 within 3 iwithin 4 pwwithin 5 sshbetween 6 sbetween 7 swithin 8 withinabs, replace
	lab val decomposition decomposition
	
	lab var area 	"Country grouping"
	lab var subarea "Country subgrouping"
	lab var y "income"
	save `globaldistribution0', replace
	restore
	
	use `globaldistribution0' , clear
	merge m:1 year area using `population0'
	drop _m
	merge m:1 year area subarea using `population1'  , update
	drop _m
	merge 1:1 year area subarea decomposition group using `population'
	drop _m
	merge m:1 year area using `_gdp0'
	drop _m
	merge m:1 year area subarea decomposition using `_gdp1' , update
	drop _m
	
	gen 	p = (y*weight/(gdp*population))*100
	lab var p "fractile"
	lab var weight "weight"
	lab var population "population"
	lab var gdp "gdp"

	save `globaldistribution0' , replace
*	save  globaldistribution0 , replace
	

	***************************** 12. Copying indices to variables
	*****************	---> globaldistribution0_wide (distribution only)
	*****************	---> globaldistribution1_wide (distribution + indices)

	* Matrices
	
	mat 		 ineqa=ineqa[1...,2...]
	mat colnames ineqa=$years
	mat rownames ineqa=ginia

	mat 		 ineqareg=ineqareg[1...,2...]
	mat colnames ineqareg=$yearsreg
	mat rownames ineqareg=ginia

	mat 		 ineqaig =ineqaig[1...,2...]
	mat colnames ineqaig =$yearsig
	mat rownames ineqaig =ginia

	mat 		 ineq 	 =ineq[1...,2...]
	mat colnames ineq 	 =$years
	mat 		 ineq 	 =ineq  \ ineqa[1,1...]
	
	mat 		 ineqb	 =ineqb[1...,2...]
	mat colnames ineqb	 =$years
	mat 		 ineqb	 =ineqb \ ineqa[2,1...]

	mat 		 ineqw	 =ineqw[1...,2...]
	mat colnames ineqw	 =$years
	mat 		 ineqw	 =ineqw \ ineqa[3,1...]

	mat 		 ineqreg =ineqreg[1...,2...]
	mat colnames ineqreg =$yearsreg
	mat 		 ineqreg =ineqreg  \ ineqareg

	mat 		 ineqig  =ineqig[1...,2...]
	mat colnames ineqig  =$yearsig
	mat 		 ineqig  =ineqig  \ ineqaig

	mat 		 mean=mean[1...,2...]
	mat colnames mean=$years
	mat rownames mean=mean 
	
	mat 		 pop=pop[1...,2...]
	mat colnames pop=$years
	mat rownames pop=pop
	
	mat list ineq
	mat list ineqb
	mat list ineqw
	mat list ineqreg
	mat list ineqig
	mat list ineqc2018

	mat list mean , format(%9.3f)
	mat list pop  , format(%9.0f)

	use `globaldistribution0', clear
	keep if year>=$t0 & year<=$t1
	
	drop if group==.

	reshape wide y p weight , i(area subarea decomposition year ) j(group)
		
	order area subarea year population gdp y* p* weight* 
	lab data "Global distribution, wide format (fractiles)"
	save `globaldistribution0_wide', replace
*	save  globaldistribution0_wide , replace
	
	* Indices 
	
	use `globaldistribution0_wide', replace

	global matnames year gini variance theilm1 theil0 theil1 theil2 atkinson025 atkinson050 atkinson075 atkinson1 atkinson2 ginia
	
	mat a = 0
	forvalues i = $t0/$t1 {
	mat a = a \ `i'
	}
	mat a = a[2...,1]
	foreach i in ineq ineqb ineqw  {
	clear
	mat 	t`i' = a , `i''
	mat colnames t`i' = $matnames
	svmat 	t`i' , names(col)
	tempfile `i'
	save 	``i'' , replace
	}
		
	foreach i in ineqreg ineqig {
	clear
	mat 	t`i' = `i''
	mat colnames t`i' = gini variance theilm1 theil0 theil1 theil2 atkinson025 atkinson050 atkinson075 atkinson1 atkinson2 ginia
	svmat 	t`i' , names(col)
	gen year = .
	local k = 0
	forvalues j=$period {
		replace year = $t0+`k' if _n>=(_N/($t1-$t0+1))*`k'+1 & _n<=(_N/($t1-$t0+1))*(`k'+1)
		local k = `k'+1
	}	
	bys year: gen subarea=_n
	tempfile `i'
	save 	``i'' , replace
	}
		
		* For Atkinson indices
		
			* within - index
	
	use  `countrydistribution', clear
	keep idc country c3 year cgroup population weight y gdp yw*
	merge m:1 c3 year using `countrydistribution2a' , keepus(atkinson*) replace update
	* Within-country terms (index --> weighted by income share)
	collapse (mean) atkinson* [aw=weight*gdp] , by(year)
	gen area 	= 0
	gen subarea = 3
	save `atkinsonw' , replace
	ren atk* watk*
	merge 1:1 year using `ineq' , keepus(atkinson*) replace update
	drop _m
	foreach a in 025 050 075 1 2 {
	gen batkinson`a'=(1 - (1-atkinson`a'/100)/(1-watkinson`a'/100))*100
	}
	keep year batk* area suba
	ren batk* atk*
	replace subarea = 1
	save `atkinsonb' , replace
	append using `atkinsonw'
	save `atkinson' , replace
	
					* within - pure, Ib=0

	use  `countrydistribution', clear
	keep idc country c3 year cgroup population weight y *gdp yw
	merge m:1 c3 year using `countrydistribution2a' , keepus(atkinson*) replace update
	drop _merge
	ren atk* catk*
	merge m:1 year using `ineq' , keepus(atk*) replace update
	ren  atk* gatk*
	ren catk*  atk*
	foreach a in 025 050 075 1 2 {
		* Between (raw) - I(xe)
	gen  ybe`a' =  gdp*(1- atkinson`a'/100)
	gen gybe`a' = ggdp*(1-gatkinson`a'/100)
		* Within (pure) - I(
	gen ywe`a' = y*gybe`a'/ybe`a'
	}	
	
	mat ineqatkw = J(1,5,.)
	forvalues j=$period {
	di as result "_______________________________ Atkinson within (Ib=0), Year=`j'"
	global years $years `j'
	
	qui ineqwiid ywe025 	if year==`j'  [aw=weight], a(.25)
	mat a = r(a_1)
	qui ineqwiid ywe050 	if year==`j'  [aw=weight], a(.50)
	mat a = a , r(a_1)
	qui ineqwiid ywe075 	if year==`j'  [aw=weight], a(.75)
	mat a = a , r(a_1)
	qui ineqwiid ywe1 	if year==`j'  [aw=weight], a(1)
	mat a = a , r(a_1)
	qui ineqwiid ywe2 	if year==`j'  [aw=weight], a(2)
	mat a = a , r(a_1)
	mat ineqatkw =  ineqatkw \ a
	}
	
	mat ineqatkw =  ineqatkw[2...,1...]
	mat ineqatkw=ineqatkw*100	
	clear
	
	svmat 	ineqatkw , names(col)
	gen year = $t0+_n-1

	ren c1 atkinson025
	ren c2 atkinson050
	ren c3 atkinson075
	ren c4 atkinson1
	ren c5 atkinson2
	
	gen area 	= 0
	gen subarea = 2
	save `atkinsonww' , replace
	
				* Appending with other indices

	use `ineqb', clear
	drop atk*
	merge 1:1 year using `atkinsonb' , keepus(atk*) replace update
	drop _m
	save `ineqb', replace
	
	use `ineqw', clear
	drop atk*
	merge 1:1 year using `atkinsonww' , keepus(atk*) replace update
	drop _m
	save `ineqw', replace	

	use 	`ineq' , clear
	gen 	subarea = 0
	gen 	decomposition = 0
	append 	using `ineqb'
	replace	decomposition = 1	if subarea == .
	replace subarea = 0 		if subarea == .
	append 	using `ineqw'
	replace decomposition = 2 	if subarea == .
	replace subarea = 0 		if subarea == .
	gen 	area = 0
	append 	using `ineqreg'
	replace decomposition = 0 		if area == .
	replace subarea  = subarea + 10 if area == .
	replace	area = 1 if area == .
	append 	using `ineqig'
	replace decomposition = 0 		if area == .
	replace subarea  = subarea + 20 if area == .
	replace	area = 2 if area == .
	gen sd = variance^.5
	drop variance
	save	`ineqindices' , replace
*	save	 ineqindices  , replace
	
	use `globaldistribution0_wide', clear
	merge 1:1 year area subarea decomposition using `ineqindices'
	drop _m
	lab data "Global distribution and indices, wide format (fractiles)"
	save `globaldistribution1_wide' , replace
*	save  globaldistribution1_wide  , replace

	***************************** 13. Merging country and global distributions and indices
	
	use `globaldistribution1_wide', clear
	append using `countrydistribution2a'
	drop yw*
	
	replace area=3 		if area==.
	replace subarea=300 if subarea==.	
	
	replace country = "_aggregate" if country==""

	order area subarea decomposition country c3 year 
	
	lab data "Country and global distribution and indices, wide (fractiles)"
	save `globaldistribution2' , replace
*	save  globaldistribution2  , replace
	
	***************************** 14. Estimating specific country income shares and ratios (from percentile)
	
	use  `globaldistribution2' , clear	
	
	cap drop vp* vy*
	cap drop top*
	cap drop bottom*
	cap drop middle*
	cap drop palma*
	cap drop s80*

	* Vintiles
	local 	  j =    1
	forvalues i =  0(5)95{
		local a = `i'+1
		local b = `i'+2
		local c = `i'+3
		local d = `i'+4
		local e = `i'+5
		
		qui gen vp`j'=  p`a'+p`b'+p`c'+p`d'+p`e'
		qui gen vy`j'= (y`a'+y`b'+y`c'+y`d'+y`e')/5
		local j = `j'+1
	}
	
	* Deciles
	local 	  j =    1
	forvalues i =  0(2)19 {
		local a = `i'+1
		local b = `i'+2
		qui gen dp`j'=  vp`a'+vp`b'
		qui gen dy`j'= (vy`a'+vy`b')*5/10
		local j = `j'+1
	}
	
	* Labelling percentiles and vintiles
	
	forvalues i=1/100 {
	lab var  y`i'      "country percentile `i''s income (2017PPPUSD)"    
	lab var  p`i'      "country percentile `i''s income share (%)"    
	lab var  group`i'  "global percentile `i'"    
	lab var  groupb`i' "global between-country percentile `i'"    
	}
	
	forvalues i=1/20 {
	lab var vy`i' "vintile `i''s income (2017PPPUSD), within country, "    
	lab var vp`i' "vintile `i''s income share (%), within country"    
	}
	
	forvalues i=1/10 {
	lab var dy`i' "decile `i''s income (2017PPPUSD), within country, "    
	lab var dp`i' "decile `i''s income share (%), within country"    
	}

	gen 	top1  	 = p100
	gen 	top5 	 = vp20
	gen 	top10 	 = 0
	gen 	top20 	 = 0
	gen 	bottom40 = 0
	gen 	bottom20 = 0
	gen 	bottom5  = vp1
	
	forvalues i=91/100 {
	replace top10 	 = top10 	+ p`i'
	}
	
	forvalues i=1/40 {
	replace bottom40 = bottom40 + p`i'
	}	
	
	forvalues i=1/20 {
	local 	  j=100-`i'-1
	replace top20 	 = top20 	+ p`j'
	replace bottom20 = bottom20 + p`i'
	}	

	gen middle50 	= 100 - bottom40 - top10
	gen palma		= top10 / bottom40
	gen s80s20		= top20 / bottom20


	lab var gini 		"Gini"
	lab var ginia  		"Absolute Gini"
	lab var theilm1 	"GE(-1)"
	lab var theil0 		"GE(0), MLD, M-Theil"
	lab var theil1 		"GE(1), T-Theil"
	lab var theil2 		"GE(2), 1/2CV2"
	lab var atkinson025	"Atkinson (0.25)"
	lab var atkinson050	"Atkinson (0.50)"
	lab var atkinson075	"Atkinson (0.75)"
	lab var atkinson1	"Atkinson (1)"
	lab var atkinson2	"Atkinson (2)"
	lab var sd 			"Standard Deviation"
	lab var top1		"Top  1%, income share, %"
	lab var top5		"Top  5%, income share, %"
	lab var top10		"Top 10%, income share, %"
	lab var top20		"Top 20%, income share, %"
	lab var bottom5		"Bottom  5%, income share, %"
	lab var bottom20	"Bottom 20%, income share, %"
	lab var bottom40	"Bottom 40%, income share, %"
	lab var middle50	"Middle 50% (41-90), income share, %"
	lab var palma		"Top10/Bottom40 Ratio"
	lab var s80s20		"Top20/Bottom20 Ratio"
			
	lab data "Country and global distribution, indices and shares"
	save `globaldistribution3' , replace
*	save  globaldistribution3  , replace
	
	
	***************************** 15. Population weighted average values by area
	
	* World
	
	use  `globaldistribution3', clear	
	keep if area==3
	drop if gini==.
	collapse gdp gini* theil* atk* sd palma* bot* top* mid* y1-y$cd p1-p$cd vp1-vy20  [aw=population] , by(year)
	gen subarea = 0
	gen area 	= 0
	gen decomposition = 4
	lab data "Population weighted mean indices by year (world)"
	save `ineqwsc' , replace
	
	* Region
	use  `globaldistribution3', clear	
	keep if area==3
	drop if gini==.	
	collapse gdp gini* theil* atk* sd palma* bot* top* mid* y1-y$cd p1-p$cd vp1-vy20 [aw=population] , by(region_wb year)
	gen subarea = 10 +  reg
	gen area 	= 1
	gen decomposition = 4
	lab data "Population weighted mean indices by year (region)"
	save `ineqwsreg' , replace
	
	* Income group
	use  `globaldistribution3', clear	
	keep if area==3
	drop if gini==.
	collapse gdp gini* theil* atk* sd palma* bot* top* mid* y1-y$cd p1-p$cd vp1-vy20 [aw=population] , by(incomeg year)
	gen subarea = 20 +  incomeg
	gen area 	= 2
	gen decomposition = 4
	drop incomeg
	lab data "Population weighted mean indices by year (income group)"
	save `ineqwsig' , replace

	use 			`ineqwsc', clear
	append using 	`ineqwsreg'
	append using 	`ineqwsig'
	order area year
	lab data "Population weighted mean indices by year"
	save `ineqwsglobal' , replace
	
		
	use  	`globaldistribution3', clear
	append  using `ineqwsglobal'
	replace country="North America" 					if subarea==11
	replace country="Latin America and the Caribbean" 	if subarea==12
	replace country="Europe and Central Asia" 			if subarea==13
	replace country="Middle East and North Africa" 		if subarea==14
	replace country="Sub-Saharan Africa" 				if subarea==15
	replace country="South Asia" 						if subarea==16
	replace country="East Asia and the Pacific" 		if subarea==17
	replace country="High income" 						if subarea==21
	replace country="Upper-middle income" 				if subarea==22
	replace country="Lower-middle income" 				if subarea==23
	replace country="Low income" 						if subarea==24
	replace country="Missing"	 						if subarea==25
	replace country="World" 							if area==0

	sort year area subarea
	by   year area: 			replace population = population[1] if area==0 & population==. 
	sort year area country subarea
	by   year area country: 	replace population = population[1] if area==1 & population==. 
	
	lab data "Country and global distribution, indices, shares. pop weighted means"
	save `globaldistribution4', replace
*	save  globaldistribution4 , replace
	
	* Checking same  value:
	tabstat gini theil0 bottom40 p50 if decomposition==4 &	area==0 & year==$t1 
	tabstat gini theil0 bottom40 p50 if 					area==3 & year==$t1 [aw=population] , by(subarea)
	
***************************** 16. Shapley decomposition
		
*	tempfile shapley globaldistribution5 globaldistribution idwiid
	
	use `globaldistribution4' , clear
*	use  globaldistribution4 , clear
	keep year area subarea decomp gini theil* at* palma s80* sd* ginia bottom* top* middle*
	keep if subarea==0
	drop area subarea
	
	global T = $t1 -$t0 + 1
	drop if year > $t1
	drop if year < $t0
	
	sort decomposition year
	xpose , clear varname
	
	gen 	q = 0
	replace	q = 5  if _varname=="bottom5"
	replace	q = 20 if _varname=="bottom20"
	replace	q = 40 if _varname=="bottom40"
	replace	q = 50 if _varname=="middle50"
	replace	q = 1  if _varname=="top1"
	replace	q = 5  if _varname=="top5"
	replace	q = 10 if _varname=="top10"
	replace	q = 20 if _varname=="top20"
	replace	q =.25 if _varname=="palma"
	replace	q = 1  if _varname=="s80s20"
	forvalues d=1/10 {
	replace	q = 10 if _varname=="dp`d'"
	}


	
	forvalues j=1/$T {
		local b = `j' + $T
		local w = `b' + $T

*		gen 	b_`j' = (v`j' - v`w' + v`b'	    )/2		if _varname!="year"  & _varname!="decomposition" 
		gen 	b_`j' = (abs (v`j'-q) - abs(v`w'-q) + abs(v`b'-q) )/2 if _varname!="year"  & _varname!="decomposition" 
		replace b_`j' =  v`j'							if _varname=="year" 
		replace b_`j' =  6								if _varname=="decomposition"

*		gen 	w_`j' = (v`j' - v`b' + v`w')/2 			if _varname!="year"  & _varname!="decomposition" 
		gen 	w_`j' = (abs(v`j'-q) - abs(v`b'-q) + abs(v`w'-q) )/2 if _varname!="year"  & _varname!="decomposition" 

		replace w_`j' =  v`j'							if _varname=="year" 
		replace w_`j' =  7								if _varname=="decomposition"

		gen 	w2_`j' =  v`j' - v`b' 					if _varname!="year"  & _varname!="decomposition" 
		replace w2_`j' =  (1-(1-v`j'/100)/(1-v`b'/100))*100 if inlist(_varname , "atkinson025", "atkinson050", "atkinson075" , "atkinson1" , "atkinson2")
		replace w2_`j' =  v`j'							if _varname=="year" 
		replace w2_`j' = 3								if _varname=="decomposition"

*		gen 	sb_`j' = (b_`j'/v`j')*100 				if _varname!="year"  & _varname!="decomposition"
		gen 	sb_`j' = (b_`j'/abs(v`j'-q))*100 		if _varname!="year"  & _varname!="decomposition"
		replace sb_`j' =  v`j'							if _varname=="year" 
		replace sb_`j' =  5								if _varname=="decomposition"
	}
	xpose , clear
	keep if inlist(decomposition,3,5,6,7)
	gen area	= 0
	gen subarea	= 0
	save `shapley' , replace
*	save  shapley  , replace
	
*	use	`globaldistribution4' , clear
	use	 globaldistribution4  , clear
	replace decomposition = 0 if decomposition == .
	lab def decomposition 0 none 1 between 2 within 3 iwithin 4 pwwithin 5 sshbetween 6 sbetween 7 swithin 8 withinabs, replace
	lab val decomposition decomposition
	
	append using `shapley' 
	

	lab data "Country and global distribution, indices, shares. pop weighted means, and Shapley"
	save `globaldistribution5' , replace
*	save  globaldistribution5  , replace

	************************ 17. Final file
	
	use `globaldistribution5' , clear
	order area subarea decomposition country c3 year pop gdp gini the* atk* ginia sd palma s80* bot* midd* top*

	* Indetifying countries with interp==3
	
	gen xx = (interp==2)
	bys country: egen mxx = sum(xx)
	tab country if mxx==140
	tab xx
	tab mxx
	tab country if mxx==70	
	
	replace interpolate = 3 if mxx==70
	drop mxx xx
	
	foreach i in gini ginia theilm1 theil0 theil1 theil2 atkinson025 atkinson050 atkinson075 atkinson1 atkinson2 ginia sd palma s80s20 bottom5 bottom20 bottom40 middle50 top20 top10 top5 top1 {
	qui replace `i'  = round(`i', .001)	
	}
	forvalues i=1/100 {
*	qui replace p`i'  = round(p`i' 		, 0.01)
	qui replace y`i'  = round(y`i' , 0.01)
	qui label var weight`i' "Population in global percentile `i'"
	qui label var weight`i' "Population in global percentile `i'"
	qui label var p`i' 		"Percentile `i', income share (%)"
	qui label var y`i' 		"Percentile `i', mean income (2017 PPP USD)"
	}
	forvalues i=1/20 {
*	qui replace vp`i' = round(vp`i'	, 0.01)
	qui replace vy`i' = round(vy`i' , 0.01)
	}
	forvalues i=1/10 {
	qui replace dp`i' = round(dp`i'	,  0.01)
	qui replace dy`i' = round(dy`i' ,  0.01)
	qui label var dp`i' 	"Decile `i', income share (%)"
	qui label var dy`i' 	"Decile `i', mean income (2017 PPP USD)"
	}
	replace  gdp 	  = round( gdp	, 0.01)
	replace ggdp	  = round(ggdp	, 0.01)
*	replace population= round(population)	

	*************************************************************
	* renormalizing sd, ginia
	
	replace ginia = ginia/1000
	replace sd 	  = sd/1000
	*************************************************************
	
	sort year area subarea
	replace country="World" if area==0
	lab def incomegroup 1 "High income" 2 "Upper middle income" 3 "Lower middle income" 4 "Low income" 5 "Missing", replace
	lab data "WIID Companion, global dataset"
	* Note: Italy 1948 is used to interpolation in 1950, but not kept
*	keep if (year>=$t0 & year <=$t1) | (country == "Italy" & year==1948)
	keep if (year>=$t0 & year <=$t1)
	save `globaldistribution' , replace
*	save  globaldistribution  , replace
	
***************************************************************************************************************	
	* Note: dec=8 (within abs) is only to see the distribution, the decomposition is included with the same dec values like the others
***************************************************************************************************************	
	
	use `globaldistribution', clear
	
	replace subarea = dec 	 	if inlist(dec, 1,2)
	replace subarea = dec-1 	if inlist(dec, 4,5) & subarea==0
	
	lab def subarea 1 "World (between-countries)" 2 "World (within-countries)" 3 "World (population weighted sum)" 4 "World (Shapley between-country share)" , add
	
	drop if inlist(dec, 3, 6, 7, 8)
	drop if subarea>3 & dec==4
	drop dec
	tab subarea
	
	* Assigning population when missing in World (pop. weig. and shapley)
	bys area year : egen _population = min(population)  if area==0
	replace population = _population 				    if area==0 & subarea==3
	
	qui replace   ggdp		 = gdp if area<=2

	forvalues i=1/100 {
	qui replace   y`i' = . if subarea >0 & subarea<5
	qui replace   p`i' = . if subarea >0 & subarea<5
	}
	forvalues i=1/10 {
	qui replace  dy`i' = . if subarea >0 & subarea<5
	qui replace  dp`i' = . if subarea >0 & subarea<5
	}	
	
	* Removing vintiles, ...
	drop vp* vy* _population group* region_un* region former top1 idcountry ggdp
	
	* Bringing id
	preserve
	use "final\wiidcountry", clear		
	keep if shareseries==1
	keep c3 year id
	gen area=3
	gen subarea=300
	save `idwiid', replace
	restore
	merge m:1 area subarea c3 year using `idwiid', keepus(id)
*	merge m:1 area subarea c3 year using  idwiid , keepus(id)
	drop if _merge==2
	drop _m
	
	* Renaming theil to ge
	ren theil* 	  ge*
	ren atkinson* a*
	
	cap lab def region_wb 0 "All regions" , add
	replace region_wb   = 0 if area==0
	cap lab def incomegroup 0 "All income groups" , add
	replace incomegroup = 0 if area==0
	
	lab var gdp "Gross Domestic Product, per capita" 
	lab var id  "WIID identifier of country-year observations"
	lab var interpolate "Country-year observation has been interpolated?"
	sort area subarea c3 year
	
	drop weight*
	
	order area subarea country c3 interpolated id region_wb incomeg year population gdp gini ge* a* palma s80 bottom5 bottom20 bottom40 top5 top10 top20 middle* ginia sd dp* dy* p1-p100 y1-y100
	
	save "final\wiidglobal", replace
