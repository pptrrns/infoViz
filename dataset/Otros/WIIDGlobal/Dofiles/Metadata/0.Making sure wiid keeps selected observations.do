		
	cd 		  			  "C:\Users\Lago\Dropbox\WIID\Simplified WIID\Standardization\Public\Data"

	global wiidold 		  "C:\Users\lago\Dropbox\WIID\Simplified WIID\Standardization\Data\WIID_31MAY2021.dta"
	global wiidnew 		  "C:\Users\lago\Dropbox\WIID\Simplified WIID\Standardization\Data\WIID_16MAY2022.dta"
	
	use "$wiidold", clear
	keep c2 country year source source_d source_c survey resource resource_d scale scale_d areacovr_d popcovr_d ref* sharing* wiidc* rev
	recode  scale_d (.=301)
	save auxwiid, replace

	
	use "$wiidnew", clear
*	use wiid0, clear
	tab wiidcompanion
	merge 1:1 c2 year source source_d source_c survey resource resource_d scale scale_d areacovr_d popcovr_d ref* sharing* rev using auxwiid
		
	tab source wiidcompanion if _m==1 , miss
	tab source wiidcompanion if _m==2 , miss
	
	* Gains in LIS
	tab country if    _m==1 & wiidcompanion==1
	
	* Losses in LIS
	tab  country if   _m==2 & wiidcompanion==1
	gen _country2 = ( _m==2 & wiidcompanion==1)
	bys country: egen country2=max(_country2)
		* Are involved in overlapping?
	tab country source if country2==1 & wiidcompanion==2	
	
	
	* Spain
	tab year source_d if country=="Spain" & wiidcompanion==2
	tab year source_d if country=="Spain" & wiidcompanion>0
	
	lab def _m 1 "gains" 2 "losses"
	lab val _m _m
	foreach c in "Belgium" "Canada" "China" "Estonia" "Finland" "France" "Germany" "Ireland" "Israel" "Korea, Republic of" "Netherlands" "Panama" "Spain"  "Switzerland" "United Kingdom" "United States" {
	di "`c'"
	tab year _m if _m<3 & wiidcompanion==1 & country=="`c'"
	
	}
	
	
	
	
	