*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	TABLE 2, FIGURE 2 & 
	APPENDIX TABLE A4, TABLE A5, TABLE A6, TABLE A7, TABLE A8, TABLE A9,
	TABLE A11, TABLE A12, TABLE A13, TABLE A14 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: estout;  coefplot; reghdfe

	ssc install estout, replace 
	ssc install coefplot, replace
	ssc install reghdfe, replace

*/
*------------------------------CODE START HERE---------------------------------*



clear

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"

set more off
set matsize 11000

use subway_analysis_use.dta, clear
 


* SETUP GLOBAL VARIABLES

	global mayor_cont gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	global mayor_cont2 gender2 race6 Mayor_age Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league
	global base_cont lpop_1 lgdp_1 lrev_1 GRP_growth_1
	global PS_cont PS_age PS_gender2 PS_race8 PS_connection_work PS_c_2currentsec2 PS_c_prov_exp PS_c_central_exp PS_c_edu PS_c_soe_exp PS_c_univ_exp PS_c_league 
		


*** FIXED-EFFECTS MODEL *** 
	
	
*** Table 2: Main Analysis ***

	eststo clear
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
	
	
	
*** Figure 2: Dynamic Effects of Subway Approvals on Mayor Promotion ***

	capture drop mpprior*
	capture drop mpconn*
	capture drop mppost*
	capture drop temp_ind*

	*** generate treatment years
	* generate mpconn0
	gen mpconn0 = (L.Mayor_plan == 0 & Mayor_plan == 1)
	replace mpconn0 = 1 if L.Mayor_plan == 1 & Mayor_plan == 1 & L.Mayor_leaderindex != Mayor_leaderindex
	
	* generate mpconn1-13
	forvalues i=1/13{
		gen temp_ind`i' = 1
		forvalues x=0/`i'{
			replace temp_ind`i' = 0 if L`x'.Mayor_plan != 1
			replace temp_ind`i' = 0 if L`x'.Mayor_leaderindex != Mayor_leaderindex
		}
		gen mpconn`i' = (temp_ind`i' == 1 & L`i'.mpconn0 == 1)
		drop temp_ind`i'
	}
	
	* combine mpconn5-13 into mpconn4
	forvalues i=5/13{
		replace mpconn4 = 1 if mpconn`i' == 1
	}
	
	*** Placebo 1: years before the city mayor received subway approval
	* generate lead 1
	gen mpprior1=(Mayor_plan==0 & F.Mayor_plan==1)

	* generate leads 2-13
	forvalues i=2/13 {
		gen temp_ind`i' = 1
		forvalues x=0/`=`i'-1' {
			replace temp_ind`i' = 0 if F`x'.Mayor_plan == 1
		}
		gen mpprior`i' = (temp_ind`i' == 1 & F`i'.Mayor_plan == 1)
		drop temp_ind`i'
	}

	* combine years 6-13 into category 5+
	forvalues i=6/13 {
		replace mpprior5 = 1 if mpprior`i' == 1
	}

	* tests
	list City_Code Year mpprior5 mpprior4 mpprior3 mpprior2 mpprior1 Mayor_plan if City_Code == 1301
	list City_Code Year mpprior5 mpprior4 mpprior3 mpprior2 mpprior1 Mayor_plan if City_Code == 1302
	list City_Code Year mpprior5 mpprior4 mpprior3 mpprior2 mpprior1 Mayor_plan if City_Code == 4101

	
	*** Placebo 2: years after the mayor who receives the subway approval leaves office
	* generate post1
	gen mppost1=(Mayor_plan==0 & L.Mayor_plan==1)

	* generate posts 2-13
	forvalues i=2/13 {
		gen temp_ind`i' = 1
		forvalues x=0/`=`i'-1' {
			replace temp_ind`i' = 0 if L`x'.Mayor_plan == 1
	}
		gen mppost`i' = (temp_ind`i' == 1 & L`i'.Mayor_plan == 1)
		drop temp_ind`i'
	}
	
	* combine years 5-13 into category 4+ 
	forvalues i=5/13 {
		replace mppost4 = 1 if mppost`i' == 1
	}
			
	* Figure 2 updated
	gen mppost_any = (mppost1 == 1 | mppost2 == 1 | mppost3 == 1 | mppost4 == 1)
	xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn0 mpconn1 mpconn2 mpconn3 ///
		mpconn4 mppost_any i.Year if fsj2 == 0, fe cluster(City_Code)
	coefplot, keep(mpprior5 mpprior4 mpprior3 mpprior2 ///
		mpconn0 mpconn1 mpconn2 mpconn3 mpconn4 ) coeflabels(mpprior5 = "=<-5" mpprior4="-4" ///
		mpprior3="-3" mpprior2="-2" mpconn0="0" mpconn1="1" ///
		mpconn2="2" mpconn3="3" mpconn4=">=4") ///
		vertical yline(0, lp(dash)) scheme(s1mono) ///
		ytitle("Effect of Approval on Mayoral Promotion in 3 Years") ///
		levels(95, 90) xline(4.5, lp(dash)) ///
		mfcolor(white) ylabel(-0.4(0.2)1.2) yscale(range(-0.4 1.2) titlegap(0.2))
	graph export figure2.png, replace
	
		* Joint test
	test mpprior5=mpprior4=mpprior3=mpprior2=0
		
	
*** Table A4: Subway Approval and Mayoral Promotion:  Alternative Measure of Promotion***	
	
	* PANEL A
	
	eststo clear
	eststo: qui reghdfe mayor_turnover_3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover_3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
	
	
	* PANEL B
	
	eststo clear
	eststo: qui reghdfe mayor_turnover2_3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover2_3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover2_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover2_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
	
	
	* PANEL C
	
	eststo clear
	eststo: qui reghdfe mayor_turnover3_3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover3_3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover3_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover3_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
	
	
	* PANEL D
	
	eststo clear
	eststo: qui reghdfe mayor_turnover4_3y Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover4_3y Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover4_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe mayor_turnover4_3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
 

	
*** Table A5: Subway Approval and Mayoral Promotion: Alternative Measures for Political Connections***	
	
	eststo clear
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont2 $base_cont Mayor_connection_work i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont2 $base_cont Mayor_connection_home i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont2 $base_cont Mayor_connection_college i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont2 $base_cont Mayor_connection_prom i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont2 $base_cont Mayor_c_2currentgvn i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear) replace
	esttab using tableA5.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear) replace

	

*** Table A6: Subway Approval and Mayors’ Promotion with Alternative Outcome Measures***	

	eststo clear
	eststo: qui xtreg Mayor_promotion1y Mayor_plan i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion1y Mayor_plan $mayor_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion2y Mayor_plan i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion2y Mayor_plan $mayor_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion4y Mayor_plan i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion4y Mayor_plan $mayor_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion5y Mayor_plan i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion5y Mayor_plan $mayor_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.Year *.provinceyear) replace
 	
	
*** Table A7: Subway Approval and Mayor’s Promotion with Alternative Measures for Cities’ Economic Performance ***	

	eststo clear
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont lpop_m3 lrev_m3 lgdp_m3 GRP_growth_m3 i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont lpop_m3 lrev_m3 lgdp_m3 logltavg i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont lpop_m3 lrev_m3 lgdp_m3 gdpidx_1st_m3 gdpidx_2nd_m3 gdpidx_3rd_m3 i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear) replace
 
	
*** Table A8: Subway Approval and Mayor’s Promotion: Drop Kunming ***	

	eststo clear
	eststo: qui xtreg Mayor_promotion3y Mayor_plan i.Year if fsj2 == 0 & City_Code != 5301, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont i.Year if fsj2 == 0 & City_Code != 5301, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont $base_cont i.Year if fsj2 == 0 & City_Code != 5301, fe cluster(City_Code)	
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont $base_cont i.provinceyear if fsj2 == 0 & City_Code != 5301, fe cluster(City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.Year *.provinceyear) replace
 	
	
*** Table A9: Subway Approval and Mayoral Promotion:  A Smaller Sample ***	

	eststo clear
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan if fsj2 == 0 & with_subway == 1, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont if fsj2 == 0 & with_subway == 1, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont if fsj2 == 0 & with_subway == 1, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan $mayor_cont $base_cont Year##pro_code if fsj2 == 0 & with_subway == 1, fe vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan) replace
 

*** Table A11: Placebo Test: Subway Approval and the Promotion of City Party Secretary *** 
	eststo clear
	eststo: qui xtreg PS_promotion3y PS_plan i.provinceyear if fsj2 == 0, fe cluster(City_Code) 
	eststo: qui xtreg PS_promotion3y PS_plan $PS_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg PS_promotion3y PS_plan $PS_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear) replace	
	
	
*** Table A12: Subway Approval and Mayoral Promotion: with Mayor Fixed Effects***	

	eststo clear 
	global mayor_contfe  Mayor_age    Mayor_connection_work // only time variant
	
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan if fsj2 == 0, absorb(Year City_Code Mayor_leaderindex) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_contfe if fsj2 == 0, absorb(Year City_Code Mayor_leaderindex) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_contfe $base_cont if fsj2 == 0, absorb(Year City_Code Mayor_leaderindex) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_contfe Mayor_c_tenure $base_cont if fsj2 == 0, absorb(Year City_Code Mayor_leaderindex) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) replace	
	 
	
*** Table A13: Subway Approval and Mayor’s Promotion: Heterogeneous effect over Age***	

	global mayor_contage gender2 race6  Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league  Mayor_connection_work

	recode Mayor_age (min/50 = 0)(51/max=1),gen(age_cat)
	
	eststo clear
	eststo: qui reghdfe Mayor_promotion3y 1.Mayor_plan##ib0.age_cat if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y 1.Mayor_plan##ib0.age_cat $mayor_contage Mayor_age if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y 1.Mayor_plan##ib0.age_cat $mayor_contage Mayor_age $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y 1.Mayor_plan##ib0.age_cat $mayor_contage Mayor_age $base_cont  if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop($mayor_contage Mayor_age $base_cont)
	 
	esttab using table_age_hte.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01)  replace
	

*** Table A14:  Test Parallel Trends Assumption *** 

	* Table A14 updated
	eststo clear
	eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn0 mpconn1 mpconn2 mpconn3 mpconn4 mppost_any i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui reghdfe Mayor_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn0 mpconn1 mpconn2 mpconn3 mpconn4 mppost_any $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	eststo: qui xtreg Mayor_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn0 mpconn1 mpconn2 mpconn3 mpconn4 mppost1 mppost2 mppost3 mppost4 i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui reghdfe Mayor_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn0 mpconn1 mpconn2 mpconn3 mpconn4 mppost1 mppost2 mppost3 mppost4 $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(mpprior* mpconn* mppost*) replace