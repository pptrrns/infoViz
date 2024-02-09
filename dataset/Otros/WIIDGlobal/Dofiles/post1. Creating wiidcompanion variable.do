
	* Code that creates wiidcompanion variable for WIID (run after wiidglobal is finalized)

	* Main path
	cd 	"C:\Users\rahul.lahoti\Dropbox\WIID update\WIID 2023\WIID Companion\Standardization\Public\Data"


	use wiid1, clear
	keep id wiidcompanion shareseries giniseries
	lab data "Identifying different series types"
	save _seriestype, replace
	keep id wiidcompanion
	keep if wiidcompanion==1
	save wiidcompanionid , replace

