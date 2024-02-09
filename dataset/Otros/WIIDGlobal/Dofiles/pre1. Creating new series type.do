
	* Code that transforms almost final WIID into official WIID
	
		* Creates new wiidcompanion, updated only for last source
		* Creates new giniseries and shareseries
		
	
	* Main path
	cd 		  	 "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\WIID Companion\Standardization\Public\Data"
	* Official WIID
	global wiidcountryold "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2022\WIID Companion\Data\final\wiidcountry.dta"
	global wiidold 		  "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2022\Official new\WIID_30JUN2022_0.dta"
	global wiidnew 		  "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\Official new\WIID_17NOV2023.dta"
	
	cap log close
	log using logfile.log, replace
	
	use 	"$wiidold" , clear
	merge 1:1 id using "$wiidcountryold" , keepus(giniseries shareseries)
	drop _m
	keep id country c2 year wiidc giniseries shareseries source* resource* scale* survey area* q1
	ren id id_old
	keep if wiidc>0
	keep if country=="China" | country=="South Africa"
	save seriestype_old , replace
	
	
	* We harmonize survey info in old and new LIS and PovcalNet

	use 	"$wiidold" , clear
	keep if source==1 | source == 7 | source == 2 | source == 3 | source == 6 | source_d == "ECLAC" | source_d == "ECLAC 2016"
	
	replace source_c = "Old series" if country == "Japan" & source == 6 & year < 2018
			* Duplicates in the original that need to be removed
	bys country year resource_d scale_d source_d source_c : drop if country=="Japan" & source==6 & _n==2
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Sri Lanka" & source==7 & _n==2 & year==1970
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Argentina" & source==7 & _n==2 & year==1961
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Barbados" & source==7 & _n==2 & year==1970
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Honduras" & source==7 & _n==2 & year==1968
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Puerto Rico" & source==7 & _n==2 & year==1963
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="Tanzania" & source==7 & _n==2 & year==1967
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="India" & source==7 & _n==2 & year==1954
	bys country year resource_d scale_d source_d source_c areacovr_d: drop if country=="India" & year==1952 & areacovr_d!=101




	preserve
	gen n=1
	collapse (sum) n , by(country c2 year source source_c resource_d scale_d areacovr_d)
	keep if n>1
	gen duplicate = 1
	tab  year country if duplicate==1 & source==1 /* Austria 1995*/
	tab  year country if duplicate==1 & source==7 /* None */
	tab  year country if duplicate==1 & source==3 /* CR, Ecuador, Peru */
	tab  year country if duplicate==1 & source==2
	save duplicate , replace
	restore

	gen 	duplicate = 0
	replace duplicate = 1 if (country=="Austria" & year==1995 & survey=="Microcensus") & source == 1
	replace duplicate = 1 if (country=="Germany" & inlist(year,1995,1996,1997) & survey=="European Community Household Panel (ECHP)" & source==2 & source_c!="")
	replace duplicate = 1 if (country=="Luxembourg" & inlist(year,1996,1997) & source==2 & survey=="ECHP based on national survey -PSELL")
	replace duplicate = 1 if (country=="United Kingdom" & inlist(year,1995,1996,1997) & source==2 & survey=="ECHP based on national survey -BHPS")
	replace duplicate = 1 if (country=="Brazil" & inrange(year,2012,2015) & source==3 & survey=="Pesquisa Nacional por Amostra de Domicilios Continua (PNADC)")
	replace duplicate = 1 if (country=="Peru" & inlist(year,2003) & source==3 & survey=="Encuesta Nacional de Hogares (ENAHO) 2")
	replace duplicate = 1 if (country=="Ecuador" & inlist(year,2006) & source==3 & survey=="Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)")
	replace duplicate = 1 if (country=="Costa Rica" & inlist(year,2010) & source==3 & survey=="Encuesta Nacional de Hogares-Encuesta de Hogares de Propositos Multiples (ENAHO-EHPM)")
	replace duplicate = 1 if (country=="India" & year==1952 & survey=="National Sample Survey Round 4 (rural prices)")
	replace duplicate = 1 if (country=="India" & year==1953 & survey=="National Sample Survey Round 6 (rural prices)")
	replace duplicate = 1 if (country=="India" & year==1954 & survey=="")
	replace duplicate = 1 if (country=="India" & year==1955 & survey=="National Sample Survey Round 8")
	replace duplicate = 1 if (country=="India" & year==1956 & survey=="National Sample Survey Round 10 (rural prices)")
	replace duplicate = 1 if (country=="India" & year==1963 & survey=="" & areacovr_d==301)
	replace duplicate = 1 if (country=="Israel" & year==1963 & survey=="Saving Survey")
	replace duplicate = 1 if (country=="Korea, Republic of" & year==1970 & survey=="City Household Income and Expenditure Survey and Farm Household Economic Survey")
	replace duplicate = 1 if (country=="Turkiye" & year==1968 & source_d=="Fields 1989")

	
		
	preserve
	gen n=1
	collapse (sum) n , by(country c2 year source source_c source_d resource_d scale_d areacovr_d duplicate)
	tab source if n>2
	restore
	
	* Based on missmatch detected below:
	**** TO include if any


	preserve
	gen n=1
	collapse (sum) n , by(country c2 year source source_c source_d resource_d scale_d areacovr_d)
	keep if n>1
	gen duplicate = 1
	tab	year country if duplicate==1 & source==1 	/* Austria 1995 and Canada 1998*/
	tab	year country if duplicate==1 & source==7	
	restore
	
	preserve
	keep if source==3
	save oldsedlac , replace
	restore
	
	*** Separate file for WB (PIP) data for testing
	preserve
	keep if source==7
	save oldpip , replace
	restore
	
	*** Non-PIP, non-SEDLAC sources
	drop if inlist(source,3)
	save oldall, replace

	
	*******************************************************************************************************
	use 	"$wiidnew" , clear
	
	gen 	duplicate = 0
			
	preserve
	keep 				  if source == 1 | source_d=="Poverty and Inequality Platform (PIP)" 

	gen n=1
	collapse (sum) n , by(country c2 year source source_c source_d resource_d scale_d areacovr_d)
	keep if n>1
	gen duplicate = 1
	tab	year country if duplicate==1 & source==1 	
	tab	year country if duplicate==1 & source==7	/* None */	
	restore

	replace duplicate = 1 if (country=="Austria" & year==1995 & survey=="Microcensus") & source==1
	merge m:1 c2 year source source_c source_d resource_d scale_d areacovr_d duplicate using oldall , update
	
	
	* To remove
	tab year country if _m==2 & source==1						/* France and Canada */
	tab year country if _m==2 & wiidcompanion==1 & source==1	/* None */

/* 
	Austria 1987 not present in new LIS survey (was also in companion)
	China 2014 - Income, Gross not present in new LIS 
	
*/

	tab      country if _m==2 & source_d=="Poverty and Inequality Platform (PIP)"	/* none after corrections */
*	tab year country if _m==2 & source_d=="Poverty and Inequality Platform (PIP)"
*	tab year country if _m==2 & source_d=="Poverty and Inequality Platform (PIP)" & wiidcompanion==1

	* Cases added (new country, Mali; several years in others, especially European ones))
	
	tab country 	 if _m==1 & resource==1 & scale==1 & source==1 
	tab country 	 if _m==1 				& scale==1 & source_d=="Poverty and Inequality Platform (PIP)"
	
	* Cases added (none)
	
	tab country 	 if _m==1 & resource==1 & scale==1 & source==3 

	* To remove (none)
	tab year country if _m==2 					 & source_d=="ECLAC"
	tab year country if _m==2 & wiidcompanion==1 & source_d=="ECLAC"

	drop if _m ==2

	* Cases added (none)
	
	tab country 	 if _m==1 & resource==1 & scale==1 & source_d=="ECLAC"

	drop _merge
	
	merge m:1 c2 year source source_c source_d resource_d scale_d areacovr_d duplicate survey using oldsedlac, update
	
	
	* To remove
	tab year country if _m==2 					 & source==3
	tab year country if _m==2 & wiidcompanion==1 & source==3

	drop if _m ==2

	drop _merge


	replace wiidcompanion = wiidcompanion_old if wiidcompanion == .

	tab wiidcompanion source , miss
	
	save aux1 , replace
/*	
		* Using old id
	use "$wiidnew", clear	
	drop if id_old==.
	merge 1:1 id_old usitabng seriestype
			* We identify only 821 obs
	tab source _merge
	keep if _m==3
	keep id wiidcompanion *series
	save aux2 , replace
	
	use aux1, clear
	merge 1:1 id using aux2 , keepus(*series) update
	replace wiidcompanion = wiidcompanion_old if wiidcompanion==.
*/	

	* Replacing old PovcalNet 2019 (only Lesotho remains)
	replace wiidcompanion=1 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Ecuador" & (year==1987)
	replace wiidcompanion=1 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Rwanda"  & (year==1985)
	replace wiidcompanion=1 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Uruguay" & (year==1981)
	replace wiidcompanion=2 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Uruguay" & (year==1989)
	replace wiidcompanion=1 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Vietnam" & (year==1998)	/* new year number */
	
	replace wiidcompanion=1 if source==5 & country=="United States" & source_c=="Series 1967-2013" & resource==3 & scale==2 & year<=1974
	replace wiidcompanion=2 if source==5 & country=="United States" & source_c=="Series 1967-2013" & resource==3 & scale==2 & year==1975
	replace wiidcompanion=1 if source_d=="Poverty and Inequality Platform (PIP)" & country=="Burundi" & (year>1998)
	
	drop wiidcompanion_old
	replace wiidcompanion = 0 if missing(wiidcompanion)
	*replace wiidc = 0 if missing(wiidc)

	* Compare with same in wiidold:
	tab source   wiidcompanion , miss
	tab source_d wiidcompanion if source==4, miss
		
	
	* Summary, WIID Companion:
	
*	- We lose: 

	******************* LIS:
	********* None
	********* Some cases, with change in year

	******************* Eurostat:
	********* None

	******************* SEDLAC:
	********* None
	********* Some cases, with change in source_c or area (Brazil, Argentina, Costa Rica)
	
	******************* ECLAC:
	********* Peru 2000
	
	******************* NSA:
	********* None
	
	******************* OECD:
	********* Japan 2009 is replaced by 2018 needed to connect old and new series
	replace wiidc = 2 if country == "Japan" & source == 6 & year == 2018 & source_c == "Old series" & resource == 1
	
	******************* PIP
	********* Central African Republic 2003 and South Africa 1996 
	********* Vietnam 1999 (PovcalNet 1999) no longer needed since it's 1998 in PIP
	********* In ohter cases, changes in year, area (Uruguay, Bolivia an Suriname), resource (Kyrgyzstan)
	
	* Correcting wiidc for Mozambique:

	sort country year
	
	* Bringing giniseries and shareseries, those that preserve id_old (China and South Afirca)
	preserve
	keep if id_old!=. & (wiidc==1 | wiidc==2)
	merge 1:1 id_old using seriestype_old, keepus(country year source_d giniseries shareseries)
	tab source_d country if _m==2
	drop if _m==2
	save aux3, replace
	restore
	
	gen 	giniseries  = 1 if (wiidc==1 | wiidc==2)	/* if gini !=., it will be estimated */
	gen 	shareseries = 0 if (wiidc==1 | wiidc==2) & q1 ==.
	replace shareseries = 1 if (wiidc==1 | wiidc==2) & q1 !=.
	
	merge m:1 id_old using aux3, keepus(giniseries shareseries) update replace
	drop if _m==2
	drop _m

	* Adjusting the rest (PIP and LIS)
	
	replace giniseries = 0 if country=="China"  & (wiidc==1 | wiidc==2) &  scale==1 & areacovr==1 & /*
*/	((source_d=="Poverty and Inequality Platform (PIP)" & (year>=1981 & year<=1987) ) | /*
*/   (source==1 & resource==1 & year==2003) )

	replace giniseries = 0 if country=="South Africa" & source_d=="Poverty and Inequality Platform (PIP)" & ((year>=1993 & year<=2005) | year==2009)

	replace shareseries = 0 if country=="China"  & (wiidc==1 | wiidc==2) & areacovr==1 & source==5 & year>=2018
	
*	edit country source_d ginis   year if ginis==0 & country=="China" & (wiidc==1 | wiidc==2)		
*	edit country source_d ginis   year if ginis==0 & country=="South Africa" & (wiidc==1 | wiidc==2)		
*	edit country source_d shares  year if share==0 & country=="China" & (wiidc==1 | wiidc==2) & q1!=.		

	tab ginis  wiidc , miss
	tab shares wiidc if q1!=. , miss

	* Saving new id with wiidc, giniseries and share series
	
	
	foreach var in wiidcompanion giniseries shareseries {
		gen `var'_old  = `var'
	}
	
	* Updating for new wiid for new LIS and others beyond latest LIS year
	
		* LIS
	replace wiidcompanion = 1 if source == 1 & scale == 1 & resource == 1 & wiidcompanion == 0
	replace wiidcompanion = 0 if source == 1 & inlist(country, "Guatemala", "Romania", "Dominican Republic")
	replace wiidcompanion = 0 if source == 1 & country == "Egypt"   & survey == "Egypt Labor Market Panel Survey (ELMPS)"
	replace wiidcompanion = 0 if source == 1 & country == "Asutria" & survey == "Microcensus" & year == 1998
	replace wiidcompanion = 0 if source == 1 & country == "Canada"  & survey == "Survey of Consumer Finances (SCF)" & year == 1995

	bysort country year : egen _lis = sum(wiidcompanion) if source == 1
	bysort country year : egen  lis = sum(_lis) 

	sort  country year
	by    country  	: egen  _ylis = max(year) if lis > 0
	by    country  	: egen   maxyearylis = mean(_ylis)
	
	drop _ylis _lis

	* Most recent source that may need to be udpated:
	
	gen aux = 0
	foreach var in source_d source_c resource_d scale_d areacovr_d year {
		sort country wiidcompanion year 
		by   country wiidcompanion 	:	gen last`var'   = `var'[_N] 	if wiidcompanion==1
		by   country wiidcompanion 	:	replace aux =  10 				if wiidcompanion==1
		sort country aux
		by   country  	:	replace last`var'   = last`var'[_N]
	}

	sort country year 
	
	replace wiidcompanion = 1 if source_d == lastsource_d & source_c == lastsource_c & resource_d == lastresource & scale_d == lastscale & areacovr_d  == lastarea & year >= lastyear
*	edit year source_d resource_d scale_d last* wiidcompanion if country=="Spain"	
*	edit year source_d resource_d scale_d last* wiidcompanion if country=="Argentina"		

		save seriestype0 , replace
		
	
	
	
	