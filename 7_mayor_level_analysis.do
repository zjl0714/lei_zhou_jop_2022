*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	APPENDIX TABLE A10, TABLE A26 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: estout; plotbeta; coefplot; reghdfe

	ssc install estout, replace 
	ssc install reghdfe, replace

*/
*------------------------------CODE START HERE---------------------------------*



clear

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"

set more off
set matsize 11000

use subway_analysis_use.dta, clear

* DEFINE GLOBAL VARIABLES

global mayor_cont gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
global base_cont lpop_1 lgdp_1 lrev_1 GRP_growth_1	
	

* GENERATE CROSS-SECTIONAL DATA
xtset Mayor_leaderindex Year
by Mayor_leaderindex: egen year_max = max(Year)
by Mayor_leaderindex: egen year_min = min(Year)
by Mayor_leaderindex: keep if Year == year_max
gen tenure = year_max - year_min + 1
 
xtset City_Code Year
set matsize 11000

drop if fsj2 == 1

*** Table A10:  Subway Approval and Mayoral Promotion: Cross-Sectional Analysis ***

	eststo clear
	eststo: qui reghdfe Mayor_Finalpromotion Mayor_plan if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_Finalpromotion Mayor_plan $mayor_cont tenure if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_Finalpromotion Mayor_plan $mayor_cont tenure $base_cont if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_Finalpromotion Mayor_plan later_mayor $mayor_cont tenure $base_cont if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan later_mayor) replace
	esttab using tableA10.rtf, se b(3) t(3) keep(Mayor_plan later_mayor) star(* 0.1 ** 0.05 *** 0.01) replace
	
	
	
*** Table A26:  Subway Approval and Corruption ***
	eststo clear
	eststo: qui reghdfe Mayor_corruption Mayor_plan if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_corruption Mayor_plan $mayor_cont tenure if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_corruption Mayor_plan $mayor_cont tenure $base_cont if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_corruption Mayor_plan later_mayor $mayor_cont tenure $base_cont if Year < 2016, absorb(year_max#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan  later_mayor) replace
	esttab using tableA26.rtf, se b(3) t(3) keep(Mayor_plan later_mayor) star(* 0.1 ** 0.05 *** 0.01) replace
	
