*------------------------------PLEASE READ-------------------------------------*
/*
REPLICATION FILE FOR 
	FIGURE 1 & APPENDIX FIGURES A1 AND A2 IN
	"PRIVATE RETUNRS TO PUBLIC INVESTMENT: POLITICAL CAREER INCENTIVES AND 
	INFRASTRUCTURE INVESTMENT IN CHINA"
	IN THE JOURNAL OF POLITICS
AUTHOR: ZHENHUAN LEI & JUNLONG ZHOU
DATE: JULY 27, 2020
SOFTWARE: STATA 15.1 SE OPERATED ON WINDOWS 10

PACKAGE NEEDED: cleanplots
net install cleanplots, from("https://tdmize.github.io/data/cleanplots")
*/
*------------------------------CODE START HERE---------------------------------*
clear

* CHANGE TO YOUR DIRECTORY
cd "~/Dropbox/Research/Subway/0_subway_final/Replication files"


* Figure 1(a)
	use rail.dta, clear

	gen lgdp_per = log(gdp/ pop)
	gen rail_share = rail / (0.91* gdp) * 100
	gen rail2 = rail / 1000000000

	twoway (lfitci rail_share lgdp_per) ///
		(scatter rail_share lgdp_per if id != "CHN", mlabel(country) msymbol(o)) ///
		(scatter rail_share lgdp_per if id == "CHN", mlabel(country) msymbol(T) msize(large)), ///
		ytitle(Investment in Railway / GDP (%)) ///
		xtitle(GDP per capita (log)) ///
		title(Railway) ///
		scheme(cleanplots) ///
		ylabel(#5) ///
		legend(off)
	graph save rail, replace

* Appendix Figure A1 (a) 

	twoway (lfitci rail2 lgdp_per) ///
		(scatter rail2 lgdp_per if id != "CHN", mlabel(country) msymbol(o)) ///
		(scatter rail2 lgdp_per if id == "CHN", mlabel(country) msymbol(T) msize(large)), ///
		ytitle(Total Investment in Railway (billion euro)) ///
		xtitle(GDP per capita (log)) ///
		title(Railway) ///
		scheme(cleanplots) ///
		ylabel(#5) ///
		legend(off)
	graph save rail_a1, replace

* Figure 1(b)

	use road.dta, clear

	gen lgdp_per = log(gdp/ pop)
	gen road_share = road / (0.91* gdp) * 100
	gen road2 = road / 1000000000

	twoway (lfitci road_share lgdp_per) ///
		(scatter road_share lgdp_per if id != "CHN", mlabel(country) msymbol(o)) ///
		(scatter road_share lgdp_per if id == "CHN", mlabel(country) msymbol(T) msize(large)), ///
		ytitle(Investment in Road / GDP (%)) ///
		xtitle(GDP per capita (log)) ///
		title(Road) ///
		scheme(cleanplots) ///
		ylabel(#5) ///
		legend(off)
	graph save road, replace

* Appendix Figure A1 (b)

	twoway (lfitci road2 lgdp_per) ///
		(scatter road2 lgdp_per if id != "CHN", mlabel(country) msymbol(o)) ///
		(scatter road2 lgdp_per if id == "CHN", mlabel(country) msymbol(T) msize(large)), ///
		ytitle(Total Investment in Road (billion euro)) ///
		xtitle(GDP per capita (log)) ///
		title(Road) ///
		scheme(cleanplots) ///
		ylabel(#5) ///
		legend(off)
	graph save road_a1, replace
	
* Merge (a) and (b) into Figure 1 and Appendix Figure A1

	graph combine rail.gph road.gph, ///
		graphregion(color(white) icolor(white) fcolor(white)) 
	graph export Fig1.tif, replace

	graph combine rail_a1.gph road_a1.gph, ///
		graphregion(color(white) icolor(white) fcolor(white)) 
	graph export FigA1.tif, replace
		
	erase rail.gph
	erase road.gph
	erase rail_a1.gph
	erase road_a1.gph

* Appendix Figure A2
	
	use four_countries.dta, clear
	
	gen road_share = road / gdp * 100
	gen rail_share = rail / gdp * 100
	
	twoway (scatter road_share year if country == "China", connect(l) ///
		sort)(scatter road_share year if country == "Mexico", connect(l) ///
		sort)(scatter road_share year if country == "India", connect(l) ///
		sort)(scatter road_share year if country == "United States", connect(l) ///
		sort), legend(label(1 "China") label(2 "Mexico") label (3 "India") ///
		label(4 "United States")) xtitle(Year) ytitle("Investment in Road / GDP (%)") ///
		scheme(s2mono) title(Road) graphregion(color(white) icolor(white) fcolor(white))
	graph save road_4countries, replace
	
	twoway (scatter rail_share year if country == "China", connect(l) ///
		sort)(scatter rail_share year if country == "Mexico", connect(l) ///
		sort)(scatter rail_share year if country == "India", connect(l) ///
		sort)(scatter rail_share year if country == "United States", connect(l) ///
		sort), legend(label(1 "China") label(2 "Mexico") label (3 "India") ///
		label(4 "United States")) xtitle(Year) ytitle("Investment in Railway / GDP (%)") ///
		scheme(s2mono) title(Railway) graphregion(color(white) icolor(white) fcolor(white))
	graph save rail_4countries, replace

	graph combine rail_4countries.gph road_4countries.gph, ///
		 iscale(0.72) scheme(sj) graphregion(color(white) icolor(white) fcolor(white)) ///
		 note(Data sources: OECD and World Bank)
	graph export FigA2.tif, replace	
	
	erase rail_4countries.gph
	erase road_4countries.gph

