
	* Code that creates wiidcountry.dta
	
	cd 	"...\Data"

	* ADO file that will be used to compute inequality measures
	do  "...\Dofiles\ineqwiid.ado"

	tempfile aux1
	
	use		wiid3, clear
	keep id idcountry country c2 c3 year gdp population ginip p1p-p100p region* incomeg adjustment conversion indexd shareseries giniseries former eu histent
	
	* Bringing population and gdp
	merge m:1 c3 year using metadata\gdp , keepus(gdp) update replace	
	drop if _m==2
	drop _m
		
	* Sorting percentiles
	preserve
	keep id country year p*p
	ren *p *
	reshape long p , i(id country year) j(percentile)
	lab val percentile
	sort country year id p 
	by country year id: replace percentile = _n
	reshape wide p, i(id country year) j(percentile)
	ren p* p*p
	save `aux1' , replace
	restore 
	drop p*p
	cap drop _m
	merge 1:1 id using `aux1' 
	cap drop _m

	* Setting minimum value of p>=0.01, re-normalziaign to add up to 1
	gen sp = 0
	forvalues i=1/100 {
			qui gen  	p`i'=p`i'p
			qui replace p`i'=.01 if p`i'<.01
			qui replace sp=sp + p`i'	
	}
	
	forvalues i=1/100 {
		qui replace  p`i'=(p`i'/sp)*100
		qui gen	 	 y`i' = p`i'*gdp
		qui gen	 	 x`i' = y`i'
		qui replace	 x`i' = p`i' if gdp==.	
	}

	* Estimating indices using ineqwiid
	
	qui gen gini = .
	qui gen gem1 = .
	qui gen ge0	 = .
	qui gen ge1	 = .
	qui gen ge2	 = .
	qui gen a025 = .
	qui gen a050 = .
	qui gen a075 = .
	qui gen a1 	 = .
	qui gen a2 	 = .
	qui gen sd	 = .

	gsort -indexd
	su year if indexd>0
	global k = r(N)
	
	* Estimating indices

	forvalues i=1/$k {
		di "`i'_$k"
		
		preserve

		qui keep x*
		
		qui keep if _n==`i'
		qui xpose , varname clear
		ren  v1 x
		sort x
		order _varname x*
		qui	  ineqwiid x , gini ge(-1 0 1 2)  a(.25 .5 .75 1 2) variance
		local gini  = r(gini)*100
		local gem1  = r(ge_1)*100
		local ge0  	= r(ge_2)*100
		local ge1  	= r(ge_3)*100
		local ge2  	= r(ge_4)*100
		local a025	= r(a_1)*100
		local a050	= r(a_2)*100
		local a075	= r(a_3)*100
		local a1  	= r(a_4)*100
		local a2  	= r(a_5)*100
		local sd  	= (r(variance)*100)^.5
		
		restore

		qui replace gini  	= `gini' 	in `i'
		qui replace gem1  	= `gem1'  	in `i'
		qui replace ge0 	= `ge0'		in `i'
		qui replace ge1  	= `ge1'		in `i'
		qui replace ge2  	= `ge2'		in `i'
		qui replace a025	= `a025' 	in `i'
		qui replace a050	= `a050' 	in `i'
		qui replace a075	= `a075' 	in `i'
		qui replace a1  	= `a1' 		in `i'
		qui replace a2  	= `a2' 		in `i'
		qui replace sd  	= `sd'		in `i'

	}	
	
	drop x*
	
	gen 	ginia 	= gini*gdp
	gen 	cv 		= (2*ge2)^.5	/* = sd/gdp, if gdp!=. */
	replace sd 		= . if gdp == .
	
	***************************** Estimating income shares and ratios (from percentiles)
		
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
	lab var  y`i'  "Percentile `i', mean income (2017 PPP USD)"    
	lab var  p`i'  "Percentile `i', income share (%)"    
	}
	
	forvalues i=1/20 {
	lab var vy`i' "Vintile `i', income (2017PPPUSD)"    
	lab var vp`i' "Vintile `i', income share (%)"    
	}
	
	forvalues i=1/10 {
	lab var dy`i' "Decile `i', mean income (2017 PPP USD)"    
	lab var dp`i' "Decile `i', income share (%)"    
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

	gen 	middle50 	= 100 - bottom40 - top10
	gen 	palma		= top10 / bottom40
	gen 	s80s20		= top20 / bottom20
	
	replace ginia = ginia/1000
	replace sd 	  = sd/1000
	
	
	lab var id 			"WIID identifier"

	lab var gini 		"Gini (estimated from percentiles)"
	lab var ginia  		"Absolute Gini"
	lab var gem1 		"GE(-1)"
	lab var ge0 		"GE(0), MLD, M-Theil"
	lab var ge1 		"GE(1), T-Theil"
	lab var ge2 		"GE(2), 1/2CV2"
	lab var cv	 		"Coefficient of Variation"
	lab var a025		"Atkinson (0.25)"
	lab var a050		"Atkinson (0.50)"
	lab var a075		"Atkinson (0.75)"
	lab var a1			"Atkinson (1)"
	lab var a2			"Atkinson (2)"
	lab var sd 			"Standard Deviation"
	lab var top1		"Top 1%, income share, % (for internal use)"
	lab var top5		"Top 5%, income share, %"
	lab var top10		"Top 10%, income share, %"
	lab var top20		"Top 20%, income share, %"
	lab var bottom5		"Bottom 5%, income share, %"
	lab var bottom20	"Bottom 20%, income share, %"
	lab var bottom40	"Bottom 40%, income share, %"
	lab var middle50	"Middle 50% (p41-p90), income share, %"
	lab var palma		"Top10/Bottom40 Ratio"
	lab var s80s20		"Top20/Bottom20 Ratio"
			
	lab data "Country and global distribution, indices and shares"
	
	drop top1* v* sp idcountry
	
	ren indexd sharetype
	lab var sharetype "Type of original income shares"
	
	ren ginip gini_std
	lab var gini_std "Standardized Gini"
	
	ren giniseries  _giniseries
	ren shareseries _shareseries
	
	tab country if gini_std==. & gini!=.
	replace gini_std = gini if gini_std==. & gini!=.
		
	gen 	giniseries 	= (gini_std!=.)
	replace giniseries  = 0 if country=="China"			& _giniseries==0
	replace giniseries  = 0 if country=="South Africa" 	& _giniseries==0
		
	gen 	shareseries  = (gini!=.)
	replace shareseries  = 0 if country=="China"		& _shareseries==0
	replace shareseries  = 0 if country=="South Africa" & _shareseries==0

	lab var shareseries	"Series based on standardized income percentiles"             
	lab var giniseries	"Series based on standardized reported Gini index"
	
	lab def adjustment 0 No 1 yes
	lab val adjustment adjustment 

	su year gini_std gini if giniseries ==1
	su year gini	 	  if shareseries==1
	
	sort c3 year

	order id country c2 c3 year region_wb region_un* eu incomeg population shares ginis gdp gini_std gini ge* cv a0* a1 a2 palma s80 bottom5 bottom20 bottom40 top5 top20 middle* ginia sd dp* dy* p1-p100 y1-y100 sharet adjustment conversion former histent
	
	
	lab var gdp "GDP per capita 2017 PPP US, integrated series"
	lab var adjust  "Adjusted during integration (phase 1)"
	lab var convers "Converted during standardization (phase 2)"
	lab var former "Former entity"
	lab var histent "Historical entity"
	lab def histent 0 No 1 Yes
	lab val histent histent
	
	lab data "WIID Companion, country-level income distribution dataset

	drop _* p*p
	drop region
	
	* Stata file
	save "final\wiidcountry", replace

