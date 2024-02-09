	*****************************************************************************
	******************************* WIID COMPANION DATASETS *********************
	*****************************************************************************
	
	
	* Set path:
	cd "C:/.../WIID Companion/"
	
	****************************** 0. GDP, Population and Income Group
	* To produce integrated GDPpc series from WDI, Maddison project and PWT
	*do "Dofiles/Metadata/0.gdp.do"
	* Population and country income groups
	*do "Dofiles/Metadata/0.population.do"
	*do "Dofiles/Metadata/0.Incomeg.do"
	
	****************************** 1. COUNTRY DATASET
	* To create simplified version of WIID, with identification for series
	do "Dofiles/1.Identification.do"
	cd ..
	* Estimates percentile distributions from agrgegated income shares (needs DASP module installed)
	do "Dofiles/2.Estimating percentiles.do"
	cd ..
	* For phase 1 - Integration
	do "Dofiles/3.Adjustments.do"
	cd ..
	* For phase 2 - Standardization (in 2 parts)
	do "Dofiles/4.Conversion to pc net income (Gini and p1_p50).do"
	cd ..
	do "Dofiles/5.Conversion to pc net income (p51_p100).do"
	cd ..
	* For estimating inequality measures, country dataset
	do "Dofiles/6.Estimating indices from percentiles.do"
	cd ..
	
	****************************** 2. GLOBAL DATASET
	do "Dofiles/7.Aggregating global distribution.do"

