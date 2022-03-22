*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	TABLE 3 & 
	APPENDIX FIGURE A4, FIGURE A5, FIGURE A6, FIGURE A7, FIGURE A8, FIGURE A9, FIGURE A10,
	TABLE A15, TABLE A16, TABLE A17, TABLE A18, TABLE A19, TABLE A20 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: estout; coefplot; ivreg2; rdrobust; rddensity; wyoung

	ssc install estout, replace 
	ssc install coefplot, replace
	ssc install ivreg2, replace 
	net install rdrobust, from(https://sites.google.com/site/rdpackages/rdrobust/stata) replace
	net install rddensity, from(https://sites.google.com/site/rdpackages/rddensity/stata) replace
	ssc install wyoung
	
*/
*------------------------------CODE START HERE---------------------------------*

clear

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"

set more off
set matsize 11000

use subway_analysis_use.dta, clear


*** Fuzzy Regression Discontinuity (RD) ***


* Density Plot *
	gen city_pop = Per_pop / 100
	gen city_rev = Budget_income / 100000
	gen city_GRP = GRP / 100000


*** Figure A5: Distribution of City Population Size ***

	histogram city_pop if city_pop >= 2 & city_pop <= 4, width(0.05) ///
		ytitle(Density) xtitle(Population (in million)) xline(3, lpattern(dash)) ///
		scheme(s1mono) bfcolor(gs12) blwidth(thin) graphregion(color(gs16)) ylabel(#5, nogrid)
	graph save density_a, replace
	graph export density_pop.png, replace
	
*** Figure A6:  Distribution of City GDP Size *** 

	histogram city_GRP if city_GRP > 80 & city_GRP < 120, width(1) ///
		ytitle(Density, size(large)) xtitle(Annual City GDP (in billion Yuan), size(large)) xline(100, lpattern(dash)) ///
		scheme(s1mono) bfcolor(gs12) blwidth(thin) graphregion(color(gs16)) ylabel(#5, nogrid)
	graph save density_c, replace 
	graph export density_GDP.png, replace
	
*** Figure A7:  Distribution of City Fiscal Revenue Size *** 


	histogram city_rev if city_rev > 8 & city_rev < 12, width(0.1) ///
		ytitle(Density, size(large)) xtitle(Government Annual Fiscal Revenue (in billion Yuan), size(large)) xline(10, lpattern(dash)) ///
		scheme(s1mono) bfcolor(gs12) blwidth(thin) graphregion(color(gs16)) ylabel(#5, nogrid)
	graph save density_b, replace
	graph export density_rev.png, replace

	
	
* Define GLOBAL VARIABLES *
		global mayor_cont3 Mayor_age gender2 race6 Mayor_c_edu Mayor_c_central_exp Mayor_c_prov_exp Mayor_c_county_exp Mayor_c_soe_exp Mayor_c_univ_exp Mayor_c_league Mayor_connection_work
	
	

*** Table A17:  Manipulation Tests of Candidate Discontinuities ***
	
preserve 
		
	rddensity Per_pop_2, c(300)  plot
	rddensity Budget_income_2, c(1000000) plot
	rddensity GRP_2, c(10000000)  plot

restore
	

*** MAIN RD ANALYSIS ***
	
preserve 
	* uniform kernal, 300w cutoff
		
	* PREP  *
	replace Per_pop_2 = (Per_pop_2 - 300)/100
	gen iv1 = (Per_pop_2 >= 0)
	gen iv1_int = iv1*Per_pop_2
	gen iv1_int2 = iv1 * Per_pop_2^2
	gen iv1_int3 = iv1 * Per_pop_2^3
	gen iv1_int4 = iv1 * Per_pop_2^4
		 
		 
	* BANDWIDTH(BW) SELECTION*
	rdbwselect Mayor_plan Per_pop_2, c(0)
	ereturn list
	*BW=1.058 
		  
	* KEEP ONLY QUALIFIED CITIES *
	keep if Budget_income_2 > 1000000 & GRP_2 > 10000000

	* RD PLOT *
	
	*** Figure A8:Graphical Presentation of the Fuzzy RD Design (Reduced Form) ***
	
		rdplot Mayor_promotion3y Per_pop_2 if fsj2 == 0 & abs(Per_pop_2) <= 1.058 , ///
			c(0) h(1.058) graph_options(scheme(s1mono) legend(off) ///
			xtitle(Population Compared to 3 Million, size(large)) ytitle(Mayor Promoted in 3 Years, size(large)))
		graph export rd_plot.png, replace
		
	*** Figure A9:Graphical Presentation of the Fuzzy RD Design (First Stage)*** 
	
		rdplot Mayor_plan Per_pop_2 if fsj2 == 0 & abs(Per_pop_2) <= 1.058 , ///
			c(0) p(4) nbins(15 15) h(1.058) graph_options(scheme(s1mono) legend(off) ///
			xtitle(Population Compared to 3 Million, size(large)) ytitle(Subway approval, size(large)))
		graph export rd_plot2.png, replace
		
		
	* COVARIATES BALANCE TEST*
	
	*** Table A15:  Balance Test on Pre-treatment Covariates ***
						
		matrix storeMyP = J(14, 1, .)  //create empty matrix with 2 (as many variables as we are looping over) rows, 1 column
				
		loc n = 0 //count the iterations
				
		eststo clear
		foreach x of var $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1{
			loc n = `n' + 1  //each iteration, adjust the count
			qui rdbwselect `x' Per_pop_2 
			local bandwidth = e(h_mserd)
			eststo: qui xi:reg `x' iv1 Per_pop_2 iv1_int i.Year i.City_Code if abs(Per_pop_2)<= `bandwidth' & fsj2 == 0, cluster(City_Code) 
			qui test iv1 //this does an F-test, but for one variable it's equivalent to a t-test (check: -help test- there is lots this can do
			matrix storeMyP[`n', 1] = `r(p)'  //save the p-value in the matrix 
		}
		
		matrix list storeMyP  // show p-value mat 
		

		* multiple testing (notice: bandwidth set to 1.058)
				
		wyoung  $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1, cmd(regress OUTCOMEVAR iv1 Per_pop_2 iv1_int ///
				i.Year i.City_Code if abs(Per_pop_2)<= 1.058  & fsj2 == 0, cluster(City_Code)) ///
				cluster(City_Code) familyp(iv1) bootstraps(100) seed(12345)
				
		mat list r(table)

		putexcel set balance_pcorr, replace
		putexcel A1=matrix(r(table)) ,names  
			
			
	*** Figure A4:Balance Test on Pre-treatment Covariates	*** 
		
		coefplot est1 || est2 || est3 || est4 || est5 || est6 || est7 || est8 || ///
				est9 || est10 || est11 || est12 || est13 || est14, ///
				keep(iv1) xline(0) legend(off) mfcolor(white) ///
				bycoefs byopts(xrescale) scheme(s1mono) levels(95) ///
				ylabel(1 "Mayor age" 2 "Mayor gender" 3 "Mayor race" 4 "Mayor education" 5 "Central government experience" ///
				6 "Mayor province experience" 7 "Mayor county experience" ///
				8 "Mayor SOE experience" 9 "Mayor university experience" ///
				10 "Youth league experience" ///
				11 "Political connection" 12 "GDP per capita" ///
				13 "Fiscal revnue per capita" 14 "GDP growth rate")
			graph export balance2.png, replace	
		
		
	*** Table 3:  Subway Approval and Mayors’ Promotion: Fuzzy RD Design ***
	
		eststo clear
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	
		esttab using table_rd.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	

	
	*** Table A18: Fuzzy Regression Discontinuity Design: Check Functional Form ***
		
		eststo clear
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) c.Per_pop_2##c.Per_pop_2 iv1_int iv1_int2 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) c.Per_pop_2##c.Per_pop_2 iv1_int iv1_int2 $mayor_cont3 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		eststo: ivreg2 Mayor_promotion3y (Mayor_plan =iv1) c.Per_pop_2##c.Per_pop_2 iv1_int iv1_int2 $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if abs(Per_pop_2) <= 1.058  & fsj2 == 0, cluster(City_Code) first
		esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	
		esttab using table_rd_2nd.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) drop(*.provinceyear *.City_Code) replace	
		
	*** Figure A10: Fuzzy Regression Discontinuity Design Results with Alternative Bandwidth Choices ***

		eststo clear
		forvalues i = 0.5(0.1)2{
			eststo: qui ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.Year ///
			i.pro_code##c.Year i.City_Code if abs(Per_pop_2) <= `i'  & fsj2 == 0, cluster(City_Code) first
		}
		
		coefplot est1 || est2 || est3 || est4 || est5 || est6 || est7 || est8 || ///
			est9 || est10 || est11 || est12 || est13 || est14 || est15, ///
			keep(Mayor_plan) yline(0) legend(off) ///
			bycoefs byopts(xrescale) scheme(s1mono) levels(95 90) vertical ///
			xlabel(1 "0.5" 2 "0.6" 3 "0.7" 4 "0.8" 5 "0.9" 6 "1.0" 7 "1.1" ///
			8 "1.2" 9 "1.3" 10 "1.4" 11 "1.5" 12 "1.6" 13 "1.7" 14 "1.8" 15 "1.9" 16 "2.0") ///
			xtitle(Bandwidth (million residents)) mfcolor(white) ylabel(-1(1)3)
		graph export bandwidth.png, replace	
		

	restore
	


	
*** Table A16 Subway Approval and Mayor’s Promotion:  Heterogeneous effect over Edcuationand Province Experience ***
	
	preserve

		gen rdd = 0
		replace  rdd = (Per_pop_2 - 300) / 100
		
		gen int1 = Mayor_plan*Mayor_c_edu
		gen int2 = Mayor_plan*Mayor_c_prov_exp
		
		eststo clear
		eststo: qui reghdfe Mayor_promotion3y Mayor_plan int1 $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1  if fsj2 == 0 & abs(rdd)<=1.058, absorb(Year  City_Code) vce(cluster City_Code)
		eststo: qui reghdfe Mayor_promotion3y Mayor_plan int1 $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1   if fsj2 == 0   & abs(rdd)<=1.058  & Budget_income_2 > 1000000 & GRP_2 > 10000000 , absorb(Year  City_Code) vce(cluster City_Code)
		eststo: qui reghdfe Mayor_promotion3y Mayor_plan int2 $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1   if fsj2 == 0  & abs(rdd)<=1.058, absorb(Year  City_Code) vce(cluster City_Code)
		eststo: qui reghdfe Mayor_promotion3y Mayor_plan int2 $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1  if fsj2 == 0 & abs(rdd)<=1.058  & Budget_income_2 > 1000000 & GRP_2 > 10000000 , absorb(Year  City_Code) vce(cluster City_Code)

	 		 	
		esttab , se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01)  keep(Mayor_plan int1 int2 Mayor_c_edu Mayor_c_prov_exp)

		
		esttab using table_rd_hte.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan int1 int2 Mayor_c_edu Mayor_c_prov_exp) replace	
	
	restore


*** Table A19: Fuzzy Regression Discontinuity Design:  Check Alternative Kernel ***
	
	preserve

		replace Per_pop_2 = (Per_pop_2 - 300) / 100
		gen iv1 = (Per_pop_2 >= 0)
		gen iv1_int = iv1*Per_pop_2
		gen iv1_int2 = iv1 * Per_pop_2^2
		gen iv1_int3 = iv1 * Per_pop_2^3
		gen iv1_int4 = iv1 * Per_pop_2^4
		
		keep if Budget_income_2 > 1000000 & GRP_2 > 10000000
		
		* triangular kernel
		gen h_t = (1-abs(Per_pop_2/1.058))
		replace h_t = 0 if h_t<0  
		
		* cosine kernel
		gen h_c =_pi/4*cos(_pi/2*Per_pop_2/1.058)
		replace h_c=0 if h_t<=0
		replace h_c=0 if h_c<=0
		
		* Quartic kernel
		gen h_q =15/16*(1-(Per_pop_2/1.058)^2)^2
		replace h_q=0 if h_t<=0	 
	
		* Epanechnikov  kernel
		gen h_e =3/4*(1-(Per_pop_2/1.058)^2)
		replace h_e=0 if h_t<=0	 
		
		
		eststo clear
		eststo:  ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if  fsj2 == 0 [pweight=h_t],  cluster(City_Code) first 
 		eststo:  ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if  fsj2 == 0 [pweight=h_c],  cluster(City_Code) first 
		eststo:  ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if  fsj2 == 0 [pweight=h_q],  cluster(City_Code) first 		
		eststo:  ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2 iv1_int $mayor_cont3 lgdp_per_1 lrev_per_1 GRP_growth_1 i.provinceyear i.City_Code if  fsj2 == 0 [pweight=h_e],  cluster(City_Code) first 
		esttab using table_rd_kernel.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan  Per_pop_2 iv1_int) replace	
 
	
	restore
	
*** Table A20:  Fuzzy Regression Discontinuity Design:  Check Placebo Cutoff ***
	
	eststo clear 
	
	foreach cutoff of numlist  200 400 500 {
	 		
		preserve
		
		replace Per_pop_2 = (Per_pop_2 - `cutoff') / 100
		gen iv1 = (Per_pop_2  >= 0)
		gen iv1_int = iv1*Per_pop_2
		
		qui rdbwselect Mayor_plan Per_pop_2  if Budget_income_2 > 1000000 & GRP_2 > 10000000 , c(0)
		local bw =  e(h_mserd)  
			
		keep if Budget_income_2 > 1000000 & GRP_2 > 10000000
		
		* uniform kernel 
		gen h_t = (1-abs(Per_pop_2/`bw'))
		replace h_t = 0 if h_t<0   

		eststo:  ivreg2 Mayor_promotion3y (Mayor_plan =iv1) Per_pop_2  iv1_int $mayor_cont3  ///
		i.pro_code##c.Year lgdp_per_1 lrev_per_1 GRP_growth_1 i.Year i.City_Code if  fsj2 == 0 & h_t>0,  cluster(City_Code) first 
		
		restore
	}
	esttab using table_rd_placebo.rtf, se b(3) t(3) star(* 0.1 ** 0.05 *** 0.01) keep(Mayor_plan  Per_pop_2 iv1_int) replace	

		
