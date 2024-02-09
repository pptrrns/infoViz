	*****************************************************************************
	******************************* WIID COMPANION DATASETS *********************
	*****************************************************************************
	
	* Set path (Note paths should also be udpated in each dofile):
	global path "...\Dofiles\"


	****************************** 0. GDP Integrated Series
	* To produce integrated GDPpc series from WDI, Maddison project and PWT
	do "$path\0.gdp.do"
	
	****************************** 1. COUNTRY DATASET
	* To create simplified version of WIID, with identification for series
	do "$path\1.Identification.do"
	* Estimates percentile distributions from agrgegated income shares (needs DASP module installed)
	do "$path\2.Estimating percentiles.do"
	* For phase 1 - Integration
	do "$path\3.Adjustments.do"
	* For phase 2 - Standardization (in 2 parts)
	do "$path\4.Conversion to pc net income (Gini and p1_p50).do"
	do "$path\5.Conversion to pc net income (p51_p100).do"
	* For estimating inequality measures, country dataset
	do "$path\6.Estimating indices from percentiles.do"
	
	****************************** 2. GLOBAL DATASET
	do "$path\7.Aggregating global distribution.do"
