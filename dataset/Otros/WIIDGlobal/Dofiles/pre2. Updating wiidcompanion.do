
	* Code that transforms almost final WIID into official WIID
	
		* Updates new wiidcompanion with changes in series		
	
	* Main path
	cd 		  	 "C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\WIID Companion\Standardization\Public\Data"

	cap log close
	log using logfile.log, replace
	
	use		seriestype0 , clear
	sort 	country year source_d wiidcompanion
	
	
	***************************************************************************
	
	* Check each country (compare with latest graphs - merged series)
	global c "North Macedonia"
	
	* Checking what we already have with previous ref source
	edit year gini source_d resource scale_d wiidcompanion gini if country=="$c" & (wiidcompanion == 1 | wiidcompanion == 2)
	* Checking if we have anything more recent (might have been already added if same source)
	edit year source_d resource scale_d wiidcompanion lastsource* gini q1 if country=="$c" & year > lastyear & resource <5

	edit year source_d resource scale_d wiidcompanion lastsource* survey gini q1 if country=="$c"
	
*	edit year source_d resource scale_d wiidcompanion lastsource gini if country=="$c" & source==1 & scale==1 & resource==1
	tab  year source_d if country=="$c"
	tab  year source_d if country=="$c" & (wiidcompanion == 1 | wiidcompanion == 2)
	
	***************************************************************************

	************** Making the necessary adjustments on wiidcompanion
	
	* Albania: replacing PovcalNet by Eurostat (web browser) since 2017
		* There is a series on income net (but we use Eurostat, which is longer, even if OECD modified)

	replace wiidcompanion = 1 if source==2 & country=="Albania"
	replace wiidcompanion = 2 if source==7 & country=="Albania" & wiidcompanion_old == 1 & year == 2017
	replace wiidcompanion = 0 if source==7 & country=="Albania" & source_d == "Poverty and Inequality Platform (PIP)" & year >  2017

	tab year source_d if country=="Albania" & wiidcompanion == 1
	tab year source_d if country=="Albania" & wiidcompanion == 2
	
	* Armenia
	replace wiidcompanion = 1 if source==7 & country=="Armenia" & year>2018
	
	* Austria:  remove duplictae (they are 94 and 95 in LIS, but both 94 in WIID)
	replace wiidcompanion = 1 if source_d== "Eurostat microdata" & country=="Austria" & year==2022 & scale_d==101 & resource==1
	replace wiidcompanion = 2 if survey  == "Microcensus" 	     & country=="Austria" & year == 1995 & scale == 1 & resource == 1
	replace wiidcompanion = 0 if source  == 1 & survey == "Microcensus" & country=="Austria" & year == 1995
	
	* Bangladesh
	replace wiidcompanion = 1 if source_d=="Bangladesh Bureau of Statistics" & country=="Bangladesh" & year>2010 & resource==2 & scale_d==301 & areacovr_d==101
	
	* Bhutan
	replace wiidcompanion = 1 if source==7 & country=="Bhutan" 
	
	* North Macedonia
	replace wiidcompanion = 1 if inlist(year,2010,2011) & country=="North Macedonia"
	
	* Botswana
	replace wiidcompanion = 1 if source_detailed=="Poverty and Inequality Platform (PIP)" & country=="Botswana" 
	
	* Burkina Faso (For longer series markign D&S as No and chosing the PIP one)
	replace wiidcompanion = 0 if country=="Burkina Faso" & inlist(year,1995,1998) & source_d=="Deininger and Squire, World Bank 2004"
	replace wiidcompanion = 1 if country=="Burkina Faso" & year==1995 & source_d=="Poverty and Inequality Platform (PIP)"
	
	* Cambodia (adding new 2013,2014 data from new series)
	replace wiidcompanion = 1 if country=="Cambodia" & inlist(year,2013,2014) & source_d=="Cambodia National Institute of Statistics"
	replace wiidcompanion = 2 if country=="Cambodia" & inlist(year,2012) & source_d=="Cambodia National Institute of Statistics"
	
	* Croatia
	replace wiidcompanion = 0 if country=="Croatia" & source_d == "European Commission 2006" & year==2003
	
	* China (chosing longer series from NBS)
	replace wiidcompanion = 2 if country=="China" & source_d=="National Bureau of Statistics of China" & inlist(year,2019) & gini!=.
	
	
	* Cote d'Ivoire: PIP most recent than LIS --> bacwards adjustment
	
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Cote d'Ivoire" & year==2019
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Cote d'Ivoire" & year==2015
		
	* Czechia: Cornia does not overlap with LIS anymore
	
	replace wiidcompanion = 1 if source_d=="Cornia 1994"  & country=="Czechia" 

	* El Salvador (adding PIP for 2021 and 2022)
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="El Salvador" & inlist(year,2021,2022)

	* Gambia, The
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Gambia, The" & inlist(year,2016,2021)

	* Germany: LIS updated, Eurostat for new data
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Germany" & inlist(year,2021,2022) & resource==1 & scale_d==101
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Germany" & inlist(year,2020) & resource==1 & scale_d==101

	
	* Honduras
	replace wiidcompanion = 0 if country=="Honduras" & year==1968 & gini==61.9 
	
	
	* India - 2016 to 2022 adding data from PIP (CPHS Survey)
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="India" & year>=2016 & areacovr_d==101
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="India" & inlist(year,2005, 2012) & areacovr_d==101
	
	* Indonesia
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Indonesia" & year>=2015 & areacovr_d==101
	
	
	* Ireland: LIS updated, Eurostat kept for last year
	replace wiidcompanion = 0 if source_d=="Eurostat microdata"  & country=="Ireland" & year< 2020 & scale==1 & resource==1
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Ireland" & year> 2020 & scale==1 & resource==1
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Ireland" & year==2020 & scale==1 & resource==1
	
	* Israel (Adding OECD for recent years)
	replace wiidcompanion = 2 if source_d=="OECD.Stat"  & country=="Israel" & year==2018 & resource==1
	replace wiidcompanion = 1 if source_d=="OECD.Stat"  & country=="Israel" & year>2018 & resource==1

	
	* Italy 
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Italy" & year>2019 & resource==1 & scale_d==101
	replace wiidcompanion = 0 if source_d=="Luxembourg Income Study (LIS)"  & country=="Italy" & year>2019 & resource==1 & scale_d==101

	
	* Japan
	*replace wiidcompanion = 0 if source_d=="Japan Bureau of Statistics" & year==2009 & country=="Japan"
	replace wiidcompanion = 1 if source_d=="OECD.Stat" & inlist(year,2015,2018) & country=="Japan" & resource==1 & source_comments=="Old series"
	

	* Kenya 
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Kenya" & year>=2006
	
	* Lithuania: LIS updated, Most recent Eurostat no longer needed (still needed for earlier years)
	
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Lithuania" & year> 2021 & scale==1 & resource==1
	
	* Luxembourg
	replace wiidcompanion = 0 if source_d=="Eurostat microdata"  & country=="Luxembourg" & inrange(year, 2014,2019) & scale==1 & resource==1
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Luxembourg" & year>2020 & scale==1 & resource==1
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Luxembourg" & year==2020 & scale==1 & resource==1

	* Malaysia
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Malaysia" & year>2003 & scale==1 
	
	* Mexico: ECLAC remove as have LIS now
	replace wiidcompanion = 0 if country=="Mexico" & year >=  2018 & source_d=="ECLAC" & areacovr==1
	
	* Moldova
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Moldova" & year>2005 & scale==1 
	
	*Mozambique
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Mozambique" & year==2015 & scale==1 
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Mozambique" & year==2020 & scale==1 
	
	* Norway
	replace wiidcompanion = 0 if source_d=="Eurostat microdata"  & country=="Norway" 

		
	* New Zealand
	replace wiidcompanion = 2 if source_d=="OECD.Stat"  & country=="New Zealand" & year==2018 & resource==1 
	replace wiidcompanion = 1 if source_d=="OECD.Stat"  & country=="New Zealand" & year>2018 & resource==1 
	
	* Paraguay
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Paraguay" & year==2020 
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Paraguay" & year>2020  
	
	*Philippines
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Philippines" & year>2002 & resource==2
	replace wiidcompanion = 0 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Philippines" & year==2000 & resource==4
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Philippines" & year==2000 & resource==4

	* Poland
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Poland" & year>2020 & scale==1 & resource==1
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Poland" & year==2020 & scale==1 & resource==1

	* Sri Lanka
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Sri Lanka" & year>2016
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Sri Lanka" & year==2016 & resource==4

	* Sweden
	replace wiidcompanion = 0 if source_d=="Eurostat microdata"  & country=="Sweden" & year< 2021 & scale==1 & resource==1
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Sweden" & year> 2021 & scale==1 & resource==1
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Sweden" & year==2021 & scale==1 & resource==1
	
	* Switzerland
	replace wiidcompanion = 1 if source_d=="Eurostat microdata"  & country=="Switzerland" & year>2019 & scale==1 & resource==1
	replace wiidcompanion = 2 if source_d=="Eurostat microdata"  & country=="Switzerland" & year==2019 & scale==1 & resource==1
	
	* Thailand
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Thailand" & year>2013 
	
	* Peru 
	replace wiidcompanion = 0 if source_d=="Luxembourg Income Study (LIS)"  & country=="Peru" & year==2019

	
	* Vietnam: LIS udpated, PIP not needed between 2006-14
	
	replace wiidcompanion = 1 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Vietnam" & year>2014
	replace wiidcompanion = 2 if source_d=="Poverty and Inequality Platform (PIP)"  & country=="Vietnam" &  year==2014
	
	* Zambia
	replace wiidcompanion = 1 if source_d=="Zambia Statistics Agency"  & country=="Zambia" & year==2022 & areacovr_detailed==101
	replace wiidcompanion = 2 if source_d=="Zambia Statistics Agency"  & country=="Zambia" &  year==2015 & areacovr_detailed==101
	
	* Updating gini and share series:
	
	********************************************************************************************************
	*********   China: with Ravallion and Chen or with WB (not both)      ----> giniseries vs shareseries
	******************* LIS replaced by NSA for gini
	*********	South Africa with Leibbrandt et al or with WB (not both)  ----> giniseries vs shareseries
	********************************************************************************************************

	cap drop *series
	
	gen 	giniseries   = 0
	gen 	shareseries  = 0
	
	replace	giniseries   = 1 if wiidcompanion>0
	replace	shareseries  = 1 if wiidcompanion>0 & q3 != .

	replace shareseries  = 0  if country == "China" & source_d == "Ravallion and Chen 2007" & wiidcompanion>0
	replace giniseries   = 0  if country == "China" & (source == 7 | (source==1 & year < 2014)) & wiidcompanion>0
	
	replace shareseries  = 0  if country == "South Africa" & (source_d == "Leibbrandt et al. 2009" | source_d == "Leibbrandt et al. 2010") & wiidcompanion>0
	replace giniseries   = 0  if country == "South Africa" & source   == 7 & wiidcompanion>0

*	edit year source_d giniseries shareseries wiidcompanion if country=="South Africa" & wiidcompanion>0

	save 	seriestype.dta , replace
	
	
	* Checking only one observation for each country year (other than the necessary overlapping obs.)
	
	keep  if wiidcompanion>0
	
	duplicates report 	wiidcompanion giniseries country year
	duplicates tag 		wiidcompanion giniseries country year , generate(xx1)
	list country year source source_detailed source_comments areacovr_detailed resource_detailed scale_detailed wiidcompanion wiidcompanion if xx1, nol

	
	duplicates report 	wiidcompanion shareseries country year
	duplicates tag 		wiidcompanion shareseries country year , generate(xx2)
	list country year source source_detailed source_comments areacovr_detailed resource_detailed scale_detailed wiidcompanion wiidcompanion if xx2, nol

	
	* There should be none other than double overlappings: Lithuania 2005 + Japan 2009 + Romania 2007
	tab country year if xx1>0 | xx2>0
	
	edit wiidcompanion country year source_d resource xx* if xx1>0 | xx2>0
	
	
	
	