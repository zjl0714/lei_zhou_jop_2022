*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	TABLE 4 &
	APPENDIX FIGURE A12, TABLE A21, TABLE A22 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: estout; plotbeta; coefplot; 

	ssc install estout, replace
	ssc install plotbeta, replace
	ssc install coefplot, replace 

*/
*------------------------------CODE START HERE---------------------------------*

*** Data setup *** 

set more off
set matsize 11000

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"

use Subway_clean_prov_use.dta, clear
 

xtset Provincial_Code Year 

global base_c lgdp  lpop lrev  gdp_gr  
global pps_c  pps_age pps_edu pps_gender 
global gov_c  gov_age gov_edu gov_gender 

keep if Year>=2003 & Year<=2017

*** Table 4: Subway Approval and the Promotion of Provincial Party Secretary ***
 

	eststo clear
	eststo: qui xtreg pps_promotion3y pps_plan  i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg pps_promotion3y pps_plan $pps_c  i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg pps_promotion3y pps_plan $pps_c $base_c i.Year, fe cluster(Provincial_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(pps_plan) replace
	esttab using Table4.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(pps_plan) replace


*** Figure A12: Dynamic Effects on the Promotion of Provincial Party Secretaries ***
	
	capture drop mpprior? mppost? mpconn?
	g mpprior1=(pps_plan==0 & F.pps_plan==1)
	g mpprior2=(pps_plan==0 & F.pps_plan==0 & F2.pps_plan==1)
	g mpprior3=(pps_plan==0 & F.pps_plan==0 & F2.pps_plan==0 & F3.pps_plan==1)
	g mpprior4=(pps_plan==0 & F.pps_plan==0 & F2.pps_plan==0 & F3.pps_plan==0 & F4.pps_plan==1)
	g mpprior5=(pps_plan==0 & F.pps_plan==0 & F2.pps_plan==0 & F3.pps_plan==0 & F4.pps_plan==0 & F5.pps_plan==0)

	g mpconn1=(pps_plan==1 & L.pps_plan==0)
	g mpconn2=(pps_plan==1 & L.pps_plan==1 & L2.pps_plan==0 )
	g mpconn3=(pps_plan==1 & L.pps_plan==1 & L2.pps_plan==1 & L3.pps_plan==0)
	g mpconn4=(pps_plan==1 & L.pps_plan==1 & L2.pps_plan==1 & L3.pps_plan==1 & L4.pps_plan==0 )
	g mpconn5=(pps_plan==1 & mpconn1==0 & mpconn2==0 & mpconn3==0 & mpconn4==0)
	
	xtreg pps_promotion3y mpprior5 mpprior4 mpprior3 mpprior2 mpconn1-mpconn5 $pps_c $base_c i.Year, fe cluster(Provincial_Code)
	coefplot, keep(mpprior5 mpprior4 mpprior3 mpprior2 ///
		mpconn1 mpconn2 mpconn3 mpconn4 mpconn5 ) ///
		 coeflabels(mpprior5 = "=<-5" mpprior4="-4" mpprior3="-3" ///
		mpprior2="-2" mpconn1="0" mpconn2="1" ///
		mpconn3="2" mpconn4="3" mpconn5=">=4") vertical yline(0, lp(dash)) scheme(s1mono) ///
		ytitle("Effect of Approval on PPS Promotion in 3 Years") ///
		levels(95, 90) xline(4.5, lp(dash)) ///
		mfcolor(white) ylabel(-0.2(0.2)0.8) yscale(range(-0.2 0.8) titlegap(0.2))
	graph export pps_did.png, replace
	

	
*** Table A21: Placebo Test: Subway Approval and the Promotion of Provincial Governor ***

	eststo clear
	eststo: qui xtreg gov_promotion3y gov_plan  i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg gov_promotion3y gov_plan $gov_c  i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg gov_promotion3y gov_plan $gov_c $base_c i.Year, fe cluster(Provincial_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(gov_plan) replace 


*** Table A22: Subway Approval and the Promotion of Provincial Party Secretary with Alternative Outcome Measures ***


	eststo clear
	eststo: qui xtreg pps_promotion1y pps_plan $pps_c $base_c pps_corrupt i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg pps_promotion2y pps_plan $pps_c $base_c pps_corrupt i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg pps_promotion4y pps_plan $pps_c $base_c pps_corrupt i.Year, fe cluster(Provincial_Code)
	eststo: qui xtreg pps_promotion5y pps_plan $pps_c $base_c pps_corrupt i.Year, fe cluster(Provincial_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*Year*) replace 
