*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	TABLE 1, TABLE A3 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: asdoc
ssc install asdoc, replace
*/
*------------------------------CODE START HERE---------------------------------*



clear

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"

set more off
set matsize 11000

use subway_analysis_use.dta, clear


*** Table 1: summary Statistics ***

	global mayor_cont gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	global base_cont lpop_1 lgdp_1 lrev_1 GRP_growth_1

	asdoc sum Mayor_promotion3y Mayor_connection_work Mayor_age Per_pop gdp rev GRP_growth ///
		Mayor_plan inv1_per GRP_per land_per rev_per, save(Table1.doc)
	 
*** Table A3: summary Statistics Continued ***
		
	asdoc sum $base_cont $mayor_cont Mayor_connection_home Mayor_connection_college ///
		Mayor_connection_prom Mayor_c_2currentgvn if fsj2 == 0, save(Table_cont.doc)
