*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	FIGURE 3, &
	APPENDIX FIGURE A11, 
	TABLE A23, TABLE A24, TABLE A25, TABLE A27, TABLE A28, TABLE A29, TABLE A30 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: estout; plotbeta; coefplot; reghdfe

	ssc install estout, replace
	ssc install plotbeta, replace
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
	global base_cont lpop_1 lgdp_1 lrev_1 GRP_growth_1
		
	global PS_cont PS_age PS_gender2 PS_race8 PS_connection_work PS_c_2currentsec2 ///
		PS_c_prov_exp PS_c_central_exp ///
		PS_c_edu PS_c_soe_exp PS_c_univ_exp PS_c_league 
		
	
*** MECHANISM *** 


*** Figure 3 Dynamic Effects of Subway Approvals on Economic Performance, Unemployment Rate and Air Quality ***

	capture drop prior? conn? 

	g prior1=(First_Plan_Passed==0 & F.First_Plan_Passed==1)
	g prior2=(First_Plan_Passed==0 & F.First_Plan_Passed==0 & F2.First_Plan_Passed==1)
	g prior3=(First_Plan_Passed==0 & F.First_Plan_Passed==0 & F2.First_Plan_Passed==0 & F3.First_Plan_Passed==1)
	g prior4=(First_Plan_Passed==0 & F.First_Plan_Passed==0 & F2.First_Plan_Passed==0 & F3.First_Plan_Passed==0 & F4.First_Plan_Passed==1)
	g prior5=(with_subway == 1 & First_Plan_Passed==0 & prior4==0 & prior3==0 & prior2==0 & prior1==0)

	g conn1=(First_Plan_Passed==1 & L.First_Plan_Passed==0)
	g conn2=(First_Plan_Passed==1 & L.First_Plan_Passed==1 & L2.First_Plan_Passed==0 )
	g conn3=(First_Plan_Passed==1 & L.First_Plan_Passed==1 & L2.First_Plan_Passed==1 & L3.First_Plan_Passed==0 )
	g conn4=(First_Plan_Passed==1 & L.First_Plan_Passed==1 & L2.First_Plan_Passed==1 & L3.First_Plan_Passed==1 & L4.First_Plan_Passed==0 )
	g conn5=(First_Plan_Passed==1 & conn1==0 & conn2==0 & conn3==0 & conn4==0)
	
	*Mechanism: infrastructure investment*

	areg inv1_per prior2-prior5 conn1-conn5 $mayor_cont ///
		lpop_1 lrev_1 GRP_growth_1 i.Year, a(City_Code) cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel A. Infrastructure Investment") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)	
	graph save 4a, replace  
	
	*Mechanism: GDP per capita*
	areg GRP_per prior2-prior5 conn1-conn5 $mayor_cont ///
		lpop_1 lrev_1 GRP_growth_1 i.Year, a(City_Code) cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel B. GDP Per Capita") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 4b, replace 
	
	*Mechanism: Land sales revenue*
	xtreg land_per prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel D. Land Sales Revenue") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 4d, replace  
	
	*Mechanism: Fiscal revenue* 
	xtreg rev_per prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel C. Fiscal Revenue") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 4c, replace 
	
	*Exclude: Unemployment*
	xtreg uerate prior2-prior5 conn1-conn5 $mayor_cont ///
		$base_cont i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel E. Unemployment Rate") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 4e, replace 
	
	*Exclude: air quality improvement*
	xtreg AQI prior2-prior5 conn1-conn5 $mayor_cont ///
		$base_cont i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel F. Air Quality") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 4f, replace 
	
	
	graph combine 4a.gph 4b.gph 4c.gph 4d.gph 4e.gph 4f.gph, ///
		 iscale(0.7) ysize(12) xsize(18) scheme(sj) graphregion(color(white) icolor(white) fcolor(white)) saving(figure4, replace)
	graph export figure3.tif, replace
	

*** Figure A11: Dynamic Effects of Subway Approvals on Land Price and Area for Government Land Sales ***
	
	*Mechanism: Land price and area*
	xtreg land_price prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 lrev_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel A. Land Price per Hectare") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 5a, replace
	
	xtreg area_total prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 lrev_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel C. Total Area of Land Sales (hectare)") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 5c, replace	
	
	xtreg value_plot prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 lrev_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel B. Land Price per Transaction") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 5b, replace	
	
	xtreg area_plot prior2-prior5 conn1-conn5 $mayor_cont ///
		lgdp_1 lpop_1 lrev_1 GRP_growth_1 i.Year, fe cluster(City_Code)
	plotbeta prior5 | prior4 | prior3  | prior2 | ///
		conn1 | conn2 | conn3 | conn4 | conn5 , vertical level(95) ///
		xlab(1 "<=-5" 2 "-4" 3 "-3" 4 "-2" 5 "0" 6 "1" ///
		7 "2" 8 "3" 9 ">=4", labsize(medsmall) labcolor(black) axis(1)) ///
		subtitle("Panel D. Land Area per Transaction") xlab(none, axis(2)) ///
		ytitle("") xtitle("") yline(0, lp(dash)) xline(4.5, lp(dash)) ///
		scheme(s1mono)
	graph save 5d, replace		
	
	graph combine 5a.gph 5b.gph 5c.gph 5d.gph, ///
		 iscale(0.7) ysize(10) xsize(12) scheme(sj) graphregion(color(white) icolor(white) fcolor(white)) saving(figure5, replace)
	graph export figureA11.png, replace	


	

*** Table A23:  Mayor Promotion and Subway Ridership Intensity	***


preserve
	
	use subway_volume.dta, clear
	
	ttest volume, by(promotion)
	ttest pass_7000, by(promotion)

restore	

	
*** Table A24: Is There Strategic Appointment of Mayors before Subway Approvals? ***

	gen change_mayor = (Mayor_leaderindex != Mayor_leaderindex [_n-1])
	replace change_mayor = . if Year == 2003
	gen pass_before1 = Plan_Pass[_n+1]
	gen pass_before2 = Plan_Pass[_n+2]
	gen pass_before3 = Plan_Pass[_n+3]
	
	eststo clear
	
	eststo: qui xtreg change_mayor pass_before1 i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg change_mayor pass_before2 i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg change_mayor pass_before3 i.Year if fsj2 == 0, fe cluster(City_Code)
	esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.Year) replace	
	

	

*** Table A25: Is There Strategic Appointment of Mayors before Subway Approvals? ***
	
	* test selection of less educated mayor 
	gen change_mayor_less = (change_mayor==1) & (Mayor_c_edu[_n-1]==0)  
	replace change_mayor_less = . if Year == 2003
	eststo clear
	eststo: qui xtreg change_mayor_less pass_before1 i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg change_mayor_less pass_before2 i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg change_mayor_less pass_before3 i.Year if fsj2 == 0, fe cluster(City_Code)
	esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.Year) replace	
	
	

*** Table A27: Testing Corruption Mechanism ***

	eststo clear
	eststo: qui xtreg Mayor_promotion3y Mayor_plan Mayor_corruption i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan Mayor_corruption $mayor_cont $base_cont i.Year if fsj2 == 0, fe cluster(City_Code)
	eststo: qui xtreg Mayor_promotion3y Mayor_plan Mayor_corruption $mayor_cont $base_cont i.provinceyear if fsj2 == 0, fe cluster(City_Code)
	esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.Year *.provinceyear) replace
	

*** Table A28: Subway Approval and Official Corruption: Cross-Sectional Analysis***
	
preserve
	
	by City_Code: egen tot_level_ju = sum(level_ju)
	by City_Code: egen tot_level_chu = sum(level_chu)
	by City_Code: egen tot_level_ke = sum(level_ke)
	
	by City_Code: egen first_plan_total = sum(First_Plan_Passed)
	gen first_plan_ever = first_plan_total
	
	replace first_plan_ever=1 if first_plan_ever>1
	
	gen tot_corr = tot_level_ju+tot_level_chu+tot_level_ke
	
	keep if Year==2004 
	
	gen per_level_ju = tot_level_ju/Per_pop_1*100 // per million
	gen per_level_chu = tot_level_chu/Per_pop_1*100 // per million
	gen per_level_ke = tot_level_ke/Per_pop_1*100 // per million
	gen per_tot_corr = tot_corr/Per_pop_1*100 // per million
	
	sum per_level_ju per_level_chu per_level_ke per_tot_corr
	
	* PANEL A 
	eststo clear
	eststo: qui areg tot_level_ju first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg tot_level_chu first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg tot_level_ke first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg tot_corr first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(first_plan_ever) replace
	
	* PANEL B
	eststo clear
	eststo: qui areg per_level_ju first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg per_level_chu first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg per_level_ke first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	eststo: qui areg per_tot_corr first_plan_ever $base_cont if fsj2 == 0, a(pro_code) cluster(pro_code)
	
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(first_plan_ever) replace
	
	 
restore
	

*** Table A29: Subway Approvals and Discounted Land Sales to Princelings ***

	eststo clear
	eststo: qui reghdfe F.princeling Mayor_plan if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe F.princeling Mayor_plan $mayor_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe F.princeling Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe F.princeling Mayor_plan $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	eststo: qui reghdfe F.princeling Mayor_plan later_mayor $mayor_cont $base_cont if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan later_mayor) replace
	esttab using ck2019a.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan later_mayor) replace

*** Table A30: Subway Approval and Mayoral Promotion: Controlling for Discounted Land Sales to Princelings ***
	eststo clear
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan princeling lnarea if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont princeling lnarea if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont princeling lnarea if fsj2 == 0, absorb(Year City_Code) vce(cluster City_Code)
	eststo: qui reghdfe Mayor_promotion3y Mayor_plan $mayor_cont $base_cont princeling lnarea if fsj2 == 0, absorb(Year#pro_code City_Code) vce(cluster City_Code)
	esttab, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan princeling lnarea) replace
	esttab using ck2019b.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan princeling lnarea) replace


