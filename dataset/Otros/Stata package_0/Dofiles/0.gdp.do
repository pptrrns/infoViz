
		* Code to integrate a series for GDP per capita, 2017 PPP: WDI + Maddison + PWT
		
	* Main path
	cd 	"...\Data\Metadata"

	******** 		1. Bringing data from all sources
		
						* WDI, World Bank (NY.GDP.PCAP.PP.KD series,
						** Downloaded from: https://databank.worldbank.org/source/world-development-indicators)
	
	tempfile wdigdp wdigdp2 wdigni wdi gdp0 aux1 aux2
	clear
	import excel "World Development Indicators\World_Development_Indicators_full.xlsx", sheet("Data") firstrow clear	
	tab SeriesC
	keep if SeriesC=="NY.GDP.PCAP.PP.KD" 
	missings dropobs , force
	missings dropvars , force
	reshape long YR, i(CountryC) j(year)  
	ren YR gdp
	replace gdp="" if gdp==".."
	destring gdp , replace
	
	ren CountryCode countrycode
	ren CountryName countrywb
	
		* Main variables: gdp (used) and gni (not used)
	ren gdp gdpwb
	lab var gdpwb "GDP pc 2017PPP, WDI"
	save `wdigdp' , replace
	
						* maddison Project (Downloaded from:
						** https://www.rug.nl/ggdc/historicaldevelopment/maddison )
		
	merge 1:1 countrycode year using "Maddison project\mpd2020.dta" , update
	ren _m _maddison
	ren country countrymaddison
	tab countrymad if _m==2 & year>=1990, miss	/* not in WDI */
	* Czechoslovakia CSK, Former USSR SUN, Former Yugoslavia YUG, Taiwan, Province of China TWN
	tab countrywb if _m==1 & year<=2016, miss	/* not in WDI */
	* many small states, Eritrea, Fiji, Greenland, Kosovo, Somalia, Timor Leste ...
	
		* Main variables: gdp gni
*	ren cgdppc 	 gdpmaddison
	ren gdppc	 gdpmaddison
*	ren rgdpnapc gdpmaddison2
	lab var 	 gdpmaddison  "GDP pc 2011US$, multiple benchmarks, maddison"
*	lab var 	 gdpmaddison2 "GDP pc 2011US$, 2011 benchmark, maddison"
	
	ren pop popmaddison
	
						* Penn World Tables, downloaded from
						** https://www.rug.nl/ggdc/productivity/pwt
						
	merge 1:1 countrycode year using "Penn World Tables\pwt91.dta" , update
	ren countrycode c3
	ren _merge _pwt
	ren pop poppwt
	ren country countrypwt
	
	gen gdppwt  = rgdpe/poppwt 
	lab var gdppwt  "GDP pc expenditure-side real GDP at chained PPPs, 2011US$"

	tab countrypwt _m if _pwt==2 & year>=1990 , miss	/* not in WDI or maddison */
	* Anguilla, Montserrat, Taiwan
	
	tab countrywb  _m if _pwt==1 & year>=1990 & year<=2017, miss	/* in WDI, not in PWT */

	tab countrymad _m if _pwt==1 & year>=1990 & year<=2017, miss	/* in maddison, not in PWT */
	* Afghanistan, Cuba, Czechoslovakia, D.P.R. of Korea, Former USSR, Former Yugoslavia, Libya, Puerto Rico

	save `gdp0' , replace

	*******************************************************

					* 2. Assigning GDP: WB --> maddison --> PWT

	gen 	gdp = gdpwb	

	foreach v in wb maddison pwt {
	gen one`v' = 1 if gdp`v'!=.
	bys c3: egen `v'=mean(one`v')
	recode `v' (.=0)
	}
	
	* For cases not in WB, ref. USA 1990
			
	su 		gdp			  		if c3=="USA" & 	year==1990
	gen 	usa = r(mean) 		if 				year==1990

	su 		gdpmaddison 			if c3=="USA" & 	year==1990
	gen	ratiomad = usa/r(mean) 	if 				year==1990
	sort c3 ratiomad
	bys  c3: replace ratiomad=ratiomad[1]
	replace	gdp = gdpmaddison*ratiomad if gdp==. & wb==0

	*		... not in maddison either
	
	su 		gdppwt 				if c3=="USA" & 	year==1990
	gen	ratiopwt = usa/r(mean) 	if 				year==1990
	sort c3 ratiopwt
	bys  c3: replace ratiopwt=ratiopwt[1]
	replace	gdp = gdppwt*ratiopwt if gdp==. & wb==0 & maddison==0 

	drop ratio*

	* To extend series backwards
	
		* Minimum and maximum year for each gdp series

	cap drop miny _miny
	bys  c3 : egen _miny = min(year) if gdp!=.
	bys  c3 : egen  miny = mean(_miny)
	su miny

	cap drop maxy _maxy
	bys  c3 : egen _maxy = max(year) if gdp!=.
	bys  c3 : egen  maxy = mean(_maxy)
	su maxy

	gen 	ratiomad = gdp/gdpmaddison if year==miny
	sort c3 ratiomad
	bys  c3: replace ratiomad=ratiomad[1]
	replace	gdp = gdpmaddison*ratiomad if gdp==. & year<miny
	
	cap drop miny _miny
	bys  c3 : egen _miny = min(year) if gdp!=.
	bys  c3 : egen  miny = mean(_miny)
	su miny
	
	gen 	ratiopwt = gdp/gdppwt if year==miny
	sort c3 ratiopwt
	bys  c3: replace ratiopwt=ratiopwt[1]
	replace	gdp = gdppwt*ratiopwt if gdp==. & year<miny

	cap drop miny _miny
	bys  c3 : egen _miny = min(year) if gdp!=.
	bys  c3 : egen  miny = mean(_miny)
	su miny
	
	drop ratio*

	* Special case USSR
			***** Russia, 1960->, Ukraine ..., 1973-> 
			***** USSR long-term trend with maddison
	
	gen 	ussr = 0
	replace ussr = 1 if inlist(c3, "ARM", "AZE", "BLR", "EST", "GEO", "KAZ", "KGZ", "LVA", "LTU")
	replace ussr = 1 if inlist(c3, "MDA", "RUS", "TJK", "TKM", "UKR", "UZB", "SUN") 
		
	cap drop miny _miny
	by c3: egen _miny = min(year) 	if gdp!=. & ussr==1
	by c3: egen  miny = mean(_miny) if ussr==1
	su miny
	tab c3 miny /* 1973 except for Russia, but blank between 1974-1980, earlier for Estonia, ... */
	replace miny = 1980 if miny<=1973 & c3!="RUS"

	gen	gdpussr = gdp if c3=="SUN"
	sort ussr year gdpussr
	bys ussr year : replace gdpussr = gdpussr[1] 
	
	gen	ratiomad = gdp/gdpussr if year==miny & ussr==1
	sort c3 ratiomad
	by c3: replace ratiomad=ratiomad[1] if ussr==1
	by c3: replace	gdp = gdpussr*ratiomad if gdp==. & year<miny  & ussr==1
	
	sort c3 year
*	graph twoway (line gdp year if c3=="SUN" & year>=1950) (line gdp year if c3=="RUS" & year>=1950) (line gdp year if c3=="UKR" & year>=1950) , name(ussr, replace)

	drop ratio*
	
	* Special case: Yugoslavia
	
	gen 	yug = 0
	replace yug = 1 if inlist(c3, "BIH", "HRV", "MKD", "MNE", "SCG", "SRB", "SVN", "XKX", "YUG")
	cap drop miny _miny
	by c3: egen _miny = min(year) 	if gdp!=. & yug==1
	by c3: egen  miny = mean(_miny) if yug==1
	su miny
	tab c3 miny

	gen	gdpyug = gdp if c3=="YUG"
	sort yug year gdpyug
	bys yug year : replace gdpyug = gdpyug[1] 
	
	gen	ratiomad = gdp/gdpyug if year==miny & yug==1
	sort c3 ratiomad
	by c3: replace ratiomad=ratiomad[1] if yug==1
	by c3: replace	gdp = gdpyug*ratiomad if gdp==. & year<miny  & yug==1
	
	sort c3 year
*	graph twoway (line gdp year if c3=="YUG" & year>=1950) (line gdp year if c3=="SRB" & year>=1950) (line gdp year if c3=="MNE" & year>=1950), name(yug, replace)

	drop ratio*
	
	* Special case: Czechoslovakia
	
	gen 	csk = 0
	replace csk = 1 if inlist(c3, "CZE", "SVK", "CSK")
	cap drop miny _miny
	by c3: egen _miny = min(year) 	if gdp!=. & csk==1
	by c3: egen  miny = mean(_miny) if csk==1
	su miny
	tab c3 miny

	gen	gdpcsk = gdp if c3=="CSK"
	sort csk year gdpcsk
	bys csk year : replace gdpcsk = gdpcsk[1] 
	
	gen	ratiomad = gdp/gdpcsk if year==miny & csk==1
	sort c3 ratiomad
	by c3: replace ratiomad=ratiomad[1] if csk==1
	by c3: replace	gdp = gdpcsk*ratiomad if gdp==. & year<miny  & csk==1
	
	sort c3 year
*	graph twoway (line gdp year if c3=="CSK" & year>=1950) (line gdp year if c3=="CZE" & year>=1950) (line gdp year if c3=="SVK" & year>=1950)

	drop ratio*

	* To extend series forward

	gen 	ratiomad = gdp/gdpmaddison if year==maxy
	sort c3 ratiomad
	bys  c3: replace ratiomad=ratiomad[1]
	replace	gdp = gdpmaddison*ratiomad if gdp==. & year>maxy
	
	cap drop maxy _maxy
	bys  c3 : egen _maxy = max(year) if gdp!=.
	bys  c3 : egen  maxy = mean(_maxy)
	su maxy
	
	gen 	ratiopwt = gdp/gdppwt if year==maxy
	sort c3 ratiopwt
	bys  c3: replace ratiopwt=ratiopwt[1]
	replace	gdp = gdppwt*ratiopwt if gdp==. & year>maxy

	cap drop maxy _maxy
	bys  c3 : egen _maxy = max(year) if gdp!=.
	bys  c3 : egen  maxy = mean(_maxy)
	su maxy

	drop ratio*
	
	su gdp* year
	su gdp* year if gdp==.
* We are using all possible values, except very early gdpmaddison2

	replace 	gdp = gdp/1000
	lab var gdp "GDP 2017US$PPP (thousands), integrated series"
	sort c3 year
	
		* Special case: Serbia and Montenegro
	
	preserve 
	collapse (mean) gdp if c3=="SRB"	,  by(year)
	ren gdp gdp1
	gen c3="SCG"
	save `aux1' , replace
	restore
	preserve
	collapse (mean) gdp if c3=="MNE" ,  by(year)
	ren gdp gdp2
	gen c3="SCG"
	save `aux2' , replace
	restore
	
	cap drop _merge
	merge 1:1 c3 year using `aux1' , update
	cap drop _merge
	merge 1:1 c3 year using `aux2' , update
	replace gdp = gdp1+gdp2 if c3=="SCG"
	drop gdp1 gdp2
	cap drop _merge
	
	
		* Special case, N Korea (lienar interpolation to fill in the gap between 1943 and 1990, both with similar incomes)
		
	ipolate gdp year 	if c3=="PRK" , gen(_gdp)
	replace gdp = _gdp 	if c3=="PRK"
	
	keep country* c3 year pop* gdp*
	
	drop gdpussr gdpyug gdpcsk 
	
	order country* c3 year pop* gdp*
	
	replace gdp = gdp*1000
	
	lab data "GDP based, integrated series based on WDI, Maddison, and PWT"
	save gdp , replace
	

