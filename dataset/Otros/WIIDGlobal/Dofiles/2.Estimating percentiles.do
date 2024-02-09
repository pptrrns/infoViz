	
	* Code that estimates percentile distributions from reported income shares, log-normal distribution
		* It requires to have DASP version 2.3 module installed ('ungroup' command), 
		* http://dasp.ecn.ulaval.ca/index.html
		
	* Main path
	cd "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\WIID Companion\Standardization\Public\Data"
	
	* File containing the necessary WIID information
	global data  "wiid0.dta"

	tempfile inequality lorenzfile
	
	global shares   d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 q1 q2 q3 q4 q5 top5 bottom5
		
	use "$data" , clear
	keep if wiidcompanion>0 | source==1


	list country year d* q*
	drop if indexd==0 | q5==0
	keep id country year $shares source idsourced gini wiidc*
	
	* Defining 'lorenzfile' command
	************************************************************************************************************
	cap program drop lorenzfile
	program def lorenzfile ,  rclass  byable(recall) sortpreserve
	syntax [if] [in] , OUT(string) [OUT2(string) Percentile(string) nobs(string) dist(string) adjust(string)]

	global shares d1 d2 d3 d4 d5 d6 d7 d8 d9 d10 q1 q2 q3 q4 q5 top5 bottom5
	global lorenz l0 l5 l10 l20 l30 l40 l50 l60 l70 l80 l90 l95 l100

	tempfile dec quin
	marksample touse , strok
	
	* Checking consistency among shares

	cap drop incondec
	qui gen  incondec = 0
	forvalues i   = 1/10 {
		local im  = `i'+1
		forvalues j = `im'/10 {
					di "`i'_`j'
					qui replace incondec = 1 if d`i'>d`j' & (d`i'!=. & d`j'!=.)
*					list country year d1-d10 if  d`i'>d`j' & (d`i'!=. & d`j'!=.)
		 }	
	}

	cap drop inconquin
	qui gen  inconquin = 0
	forvalues i   = 1/5 {
		local im  = `i'+1
		forvalues j = `im'/5 {
*					di "`i'_`j'
					qui replace inconquin = 1 if q`i'>q`j' & (q`i'!=. & q`j'!=.)
					list country year q1-q5 if q`i'>q`j' & (q`i'!=. & q`j'!=.)
		 }	
	}

	cap drop 	inconbot
	qui gen  	inconbot = 0
	qui replace inconbot = 1 if (bottom5/5) > (d1/10) & bottom5 !=. & d1 != .
	qui replace inconbot = 1 if (bottom5/5) > (q1/20) & bottom5 !=. & q1 != .
	qui replace inconbot = 1 if ((d1-bottom5)/5) >(d2/10) & bottom5 !=. & d1 != . & d2 != .
	qui replace inconbot = 1 if ((q1-bottom5)/15)>(q2/20) & bottom5 !=. & q1 != . & q2 != .
	
	cap drop 	incontop
	qui gen  	incontop = 0
	qui replace incontop = 1 if (top5/5) < (d10/10) & top5 !=. & d10 != .
	qui replace incontop = 1 if (top5/5) < ( q5/20) & top5 !=. & q5  != .
	qui replace incontop = 1 if ((d10-top5)/5)<(d9/10) & top5 !=. & d9 != . & d10 != .
	qui replace incontop = 1 if ((q5-top5)/15)<(q4/20) & top5 !=. & q4 != . &  q5 != .
	
*	list id country year idsourced* incon* if (incondec==1 | inconquin==1 | inconbot==1 | incontop==1)

	* Discard top5 and bottom5 when inconsistent
	
	qui replace top5=. 		if incontop==1
	qui replace bottom5=. 	if inconbot==1
	
	* We re-order deciles and quintiles, if necessary
	preserve
	keep id d1-d10
	reshape long d, i(id) j(var)
	drop if d==.
	sort id d
	by id: gen 		nv=_n
	by id: replace 	nv=var if _N<10
	drop var
cap	reshape wide d, i(id) j(nv)
	save `dec' , replace
	restore
	preserve
	keep id q1-q5
cap	reshape long q, i(id) j(var)
	drop if q==.
	sort id q
	by id: gen 		nv=_n
	by id: replace 	nv=var if _N<5
	drop var
cap	reshape wide q, i(id) j(nv)	
	save `quin' , replace
	restore
	drop d1-d10
	merge 1:1 id using `dec'
	drop _m
	drop q1-q5
	merge 1:1 id using `quin'
	drop _m

cap	replace q1 = d1 + d2  if d1!=. & d2  != .
cap	replace q2 = d3 + d4  if d3!=. & d4  != .
cap	replace q3 = d5 + d6  if d5!=. & d6  != .
cap	replace q4 = d7 + d8  if d7!=. & d8  != .
cap	replace q5 = d9 + d10 if d9!=. & d10 != .

	preserve
	
	foreach i in 0 5 10 20 30 40 50 60 70 80 90 95 100 {
		qui gen p`i' =  .
		qui gen l`i' =  .
	}
	
	qui replace l0  = 0
	cap	qui replace l5  = bottom5 	if bottom5!=.
	cap	qui replace l10 = d1
	cap	qui replace l20 = l10+d2		
	cap	qui replace l20 = q1		if l20==. & q1!=.
	cap	qui replace l30 = l20	+d3	
	cap	qui replace l40 = l30+d4
	cap	qui replace l40 = l20+q2	if l40==. & q2!=.
	cap	qui replace l50 = l40+d5	
	cap	qui replace l60 = l50+d6
	cap	qui replace l60 = l40+q3	if l60==. & q3!=.
	cap	qui replace l70 = l60+d7
	cap	qui replace l80 = l70+d8
	cap	qui replace l80 = l60+q4	if l80==. & q4!=.
	cap	qui replace l90 = l80+d9
	cap	qui replace l100= l90+d10
	cap	qui replace l100= l80+q5	if l100==. & q5!=.
	cap	qui replace l95 = l100-top5	if l95==.

	* re-scaling, to add up to exactly 100
	
	local j = 5
	foreach i in 5 10 20 30 40 50 60 70 80 90 95 100 {
cap		qui replace l`i' = l`i' * 100/l100 
	}
	
	keep  $lorenz gini* id country year
	sort id
*	su
	global N = _N
	
	* Check Lorenz-consistency
	
	cap drop inconsistent*
	qui gen  inconsistent = 0
	foreach i in 0 5 10 20 30 40 50 60 70 80 90 95 100 {
		foreach j in 0 10 20 30 40 50 60 70 80 90 95 100 {
			foreach s in 0 10 20 30 40 50 60 70 80 90 95 100 {
				foreach t in 0 10 20 30 40 50 60 70 80 90 95 100 {
*					di "`i'_`j'_`s'_`t'"
					qui replace inconsistent = 1 if (l`t'-l`s')/(`t'-`s') < (l`j'-l`i')/(`j'-`i') & l`i'!=. & l`j'!=. & l`s'!=. & l`t'!=. & `t'>`s' & `s'>=`j' & `j'>`i'
				 }	
			}
		}
	}

	* Inconsistencies: 
	list id country year inconsistent if incon==1	
	
	drop country year

	order $lorenz gini incon id
	qui xpose , clear varname
	qui gen 	p = .
	qui replace p = 0   in 1
	qui replace p = 5   in 2
	qui replace p = 10  in 3
	qui replace p = 20  in 4
	qui replace p = 30  in 5	
	qui replace p = 40  in 6
	qui replace p = 50  in 7
	qui replace p = 60  in 8
	qui replace p = 70  in 9	
	qui replace p = 80  in 10
	qui replace p = 90  in 11
	qui replace p = 95  in 12
	qui replace p = 100 in 13

	qui replace p = p/100

	save `out' , replace
	restore
	
	* Disagregating percentiles

if "`percentile'" != "" {

	preserve
	use `out', clear
	forvalues i = 1/$N {
		di "v`i'"
		local j = v`i'[_N]
*		di "`percentile'_`j'"
		tempfile `percentile'_`j'
		qui ungroup p v`i' , fname(``percentile'_`j'') nobs(`nobs') dist(`dist') adjust(`adjust')
	}	
	restore
}

		*************** Estimating inequality from detailed percentiles
	
if "`out2'" != "" {

	sort 	id
	mkmat 	id
	global 	r=rowsof(id)
	
	qui gen ginie0		= .
	qui gen ginie		= .
	qui gen theilm1		= .
	qui gen theil0		= .
	qui gen theil1		= .
	qui gen theil2		= .
	qui gen atkinson025	= .
	qui gen atkinson050	= .
	qui gen atkinson075	= .
	qui gen atkinson1 	= .
	qui gen atkinson2 	= .
	qui gen sd0			= .
	qui gen sd			= .
	forvalues t=1/100 {
		qui gen	p`t'		= .
	}

	
	forvalues i = 1/$r {
	
		preserve
		local j = id[`i']
		qui use ``percentile'_`j'' , clear
	
		sort _y
		qui su _y
		scalar ss = r(sum)
		
		forvalues t=1/100 {
			qui egen p`t'		= sum(_y) if _n>(`t'-1)*_N/100 & _n<=`t'*_N/100
			qui su   p`t'
			scalar  _p`t'  		= r(mean)/ss*100
		}
		
		restore
		sleep 1000
		forvalues t=1/100 {
			qui replace p`t'		= _p`t'  	if id==`j'
		}
		qui save `out2', replace
		sleep 1000
	}
}

		
	end
	************************************************************************************************************

	* Running command
	
	lorenzfile , out(`lorenzfile') out2(`inequality') percentile(percentile) nobs(10000) adjust(1)
	
	keep  id country year source* gini p1-p100
	order id country year source* gini p1-p100
	
	lab data "Estimated percentile distributions"
	notes drop _dta
	save percentiles , replace
	
	*save percentiles_BWA , replace

	
	
	
	
	
	
	
	
	
	
	
	