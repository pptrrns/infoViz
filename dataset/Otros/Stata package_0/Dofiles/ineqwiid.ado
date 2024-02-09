cap program drop ineqwiid
program def ineqwiid, rclass byable(recall) sortpreserve
	syntax varlist(min=1 max=1) [aweight iweight fweight] [if] [in] [, GIni GE(string) Variance Logvariance Rmd Atkinson(string) Format(string) BY(string) ]
	marksample touse
	di ""
	preserve
	tempvar y w index value ineq
	local  x:  word 1 of `varlist'
	qui gen `y' = `x' 				if `touse'
	gsort   `y'		
	set more off
	if "`weight'" == "" {
		qui gen double `w'=1		if `touse'
	}
	else {
		qui gen double `w' `exp'	if `touse'
	}
	if "`format'" == "" {
		loc format "%9.4f"
	}
	if "`ge'" 		~= "" {
		local alpha 	`ge'
	}
	if "`atkinson'" ~= "" {
		local epsilon 	`atkinson'
	}
	if "`gini'" =="" & "`ge'" =="" & "`atkinson'" =="" & "`variance'" =="" & "`logvariance'" =="" & "`rmd'" ==""	{
		local gini 			gini
		local ge			ge
		local atkinson		atkinson
		local variance		variance
		local logvariance	logvariance
		local rmd			rmd
		local alpha 	-2 -1 0 1 2
		local epsilon 	.25 .50 .75 1 2
	}
	local neg = 0
	qui sum `y' [`weight' `exp'] 				if `y' <=0
	if r(N) >0 {
		local neg = 1
		di r(N) " nonpositive observations used in calculations (to avoid this, add 'if `1' > 0' )"
			di ""
		if "`ge'" 			~= "" {
			di "=> GE(a<=0 or a==1) not computed"
			di ""
		}
		if "`atkinson'" 	~= "" {
			di "=> A(e>=1) not computed"
			di ""
		}
		if "`logvariance'" 	~= "" {
			local logvariance 
			di "=> logvariance not computed"
			di ""
		}		
	}
	
	tempname id wc yc _gini ssw _ge _atk
	tempname M MM I

	qui sum `y' [`weight' `exp']
	local mean 		= r(mean)
	if `mean' == 0 {
		di as result "Warning: zero mean, some indices not computed, Gini is absolute"
	}
	if `mean' < 0 {
		di as result "Warning: negative mean, some indices not computed"
	}
	if `mean' == . {
		di as result "Warning: no valid observations"
	}
	
	local N   		= r(N)
	local sw  		= r(sum_w)
	local sy  		= r(sum)
	local _variance	= r(Var)*(`N'-1)/`N'		/* population variance */
	local _sd  		= r(sd)

	qui range 		`value' 	. . 100
	qui range 		`index' 	. . 100
	
	label var 	`index' 	Index
	label var 	`value' 	Value
	label def 	`index'		0 ""
	label value `index' 	`index'
	mat 		`ineq' 		= 0
	local		names		
	
	local p = 1
	if "`gini'" 		~= "" & `mean'!=. {
		 qui gen double `wc' = sum(`w')		/`sw'
		 qui gen double `yc' = sum(`w'*`y') /`sy' 
		 qui egen `_gini' = sum( `wc'[_n]*`yc'[_n+1]-`wc'[_n+1]*`yc'[_n] )											
		* SUM (F)*(y_i-y_i-1)
*		 dis "Gini="`_gini'
		 scalar 	 `_gini' 	= `_gini'[1]
		 mat 		 `ineq' 	= `ineq' \  `_gini'
		 local		 names `names' Gini
		 ret  scalar `gini' 	= `_gini'
		 qui replace `value' 	= `_gini' 	   in `p'
		 qui replace `index'   	= `p'		   in `p'
		 label def   `index'	  `p' Gini , add
		 local p = `p'+1
	}	 
	
	if "`variance'" 	~= "" {
*		 dis "Variance="`_variance'
		 mat 		 `ineq' 	= `ineq' \  `_variance'
		 local		 names `names' Variance
		 ret scalar `variance'  = `_variance'
		 qui replace `value' 	= `_variance' 	in `p'
		 qui replace `index'   	= `p'	 		in `p'
		 label def   `index'      `p' Variance , add
		 local p = `p'+1
	}
	if "`logvariance'" 	~= "" {
		 tempname vv1 vv2 v1 v2 _logvariance
		 qui ameans `y' [`weight' `exp'] 
		 local gmean = r(mean_g)
		 qui gen `vv1'   =  ln(`y')^2						*`w'/`sw'
		 qui gen `vv2' 	 =  ln(`y')							*`w'/`sw'
		 qui egen `v1'   =  sum( `vv1' )
		 qui egen `v2'   =  sum( `vv2' )
		 qui gen `_logvariance'  =  `v1' - 2*`v2'*ln(`gmean') + ln(`gmean')^2
*		 dis "logvariance="`_logvariance'
		 scalar 	 `_logvariance' 	= `_logvariance'[1]
		 mat 		 `ineq' 	= `ineq' \  `_logvariance'
		 local		 names `names' LogVar
		 ret  scalar `logvariance'	= `_logvariance'
		 qui replace `value' 	= `_logvariance'  	in `p'
		 qui replace `index'   	= `p' 				in `p'
		 label def   `index'	  `p' LogVar , add
		 local p = `p'+1
	} 	
	if "`rmd'" 			~= "" {
*		 di `mean'
		 qui sum `y' [`weight' `exp'] 					if `y'<=`mean'
		 local sw1 				= r(sum_w) / `sw'
		 local sy1 				= r(sum)   / `sy'
		 local _rmd				=  `sw1' - `sy1' 
*		 dis "rmd="`_rmd'
		 mat 		 `ineq' 	= `ineq' \  `_rmd'
		 local		 names `names' RMD
		 ret  scalar `rmd'		= `_rmd'
		 qui replace `value' 	= `_rmd' 	in `p'
		 qui replace `index'   	= `p'		in `p'		 
		 label def   `index'	  `p' "RMD" , add
		 local p = `p'+1
	} 	
	
		
	* GE, v = sum((y/m)^a*ln y/m)) a=0,1 ; v= sum(y^a)
	if "`ge'" 		~= "" & `mean'!=0 & `mean'!=. {	
		local j = 1
		foreach a in `alpha' {
			tempname vv`j' v`j' ge_`j'
			local yes = 0
			if (`a' == 0 | `a' == 1) & `neg'== 0 {
					local yes = 1
					qui  gen `vv`j''  	=  (`y'/`mean')^`a'* ln(`y'/`mean')*`w'/`sw'
					qui egen `v`j''   	=  sum( `vv`j'' )
				if `a'==0 {
					qui gen `ge_`j'' 	=   - `v`j''	
				}
				if `a'==1 {
					qui gen `ge_`j''  	=     `v`j'' 
				}
			}
			if (`a' < 0 & `neg'== 0 ) | (`a' >0 & `a' ~= 1) 	{
					local yes = 1
					qui  gen `vv`j''  	=  ((`y'/`mean')^`a')*`w'/`sw'
					qui egen `v`j''   	=  sum( `vv`j'' ) 
					qui gen `ge_`j''	=  (`v`j'' - 1 ) / (`a'*(`a'-1)) 
			}
			if `yes' == 1  {
*				dis "GE(`a')="`ge_`j''
				scalar 	 	`ge_`j'' 	= `ge_`j''[1]
*				di 	 	"`ge_`j''=" `ge_`j''
				mat 		 `ineq' 	= `ineq' \  `ge_`j''
				local		 names `names' "GE(`a')"
				ret  scalar ge_`j'= `ge_`j''
		        qui replace `value' = `ge_`j'' 	in `p'
			    qui replace `index' = `p'		in `p'	
			    label def   `index'	  `p' "GE(`a')" , add
				local p = `p'+1
			}
*			sum `v`j''
			local j = `j'+1
		}
	}

	* Ak, v= sum(ln y) if e=1; v = sum( y^(1-e) )
	if "`atkinson'" 		~= "" & `mean'>=0 & `mean'!=. {	
		local j = 1
		foreach e in `epsilon' {
			tempname vv`j' v`j' a_`j'
			cap drop 	 `rifn'_a_`j'
			local yes = 0
			if `e' ==  1 & `neg'== 0 	{
				local yes = 1
				qui  gen `vv`j''  =  ln( `y' )			*`w'/`sw'
				qui egen `v`j''   =  sum( `vv`j'' )	
				qui gen `a_`j''   = 1 - ( exp(`v`j'' )					/`mean' )  
			}
			if `e' <  1 | (`e' > 1 & `neg'== 0) 	{
				local yes = 1
				qui  gen `vv`j''  =  (   `y'^(1-`e') )	*`w'/`sw'
				qui egen `v`j''   =  sum( `vv`j'' )	
				qui gen `a_`j''	  = 1 - ( (    `v`j''^( 1/ (1-`e')) ) 	/ `mean' )	
			}
			if `yes' == 1  {				
*				dis "A(`e')="`a_`j''
				scalar 	 	`a_`j'' 	= `a_`j''[1]
				mat 		 `ineq' 	= `ineq' \  `a_`j''
				local		 names `names' "A(`e')"
				ret  scalar a_`j'= `a_`j''
		        qui replace `value' = `a_`j'' 	in `p'
			    qui replace `index' = `p'		in `p'	
			    label def   `index'	  `p' "A(`e')" , add
			    local p = `p'+1
			}
			local j = `j'+1
		}
	}
	
	mat 	`ineq'	= `ineq'[2...,1]
	mat colnames `ineq' = Values
	mat rownames `ineq' = `names'
	ret mat  ineq 	= `ineq'
	
	* Reporting results
	
	tabdisp `index' 		if `value' ~=. , c(`value') f(`format') concise stubwidth(15) csepwidth(1) cellwidth(10) left
	restore
end


