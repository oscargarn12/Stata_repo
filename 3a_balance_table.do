/*
Date: May/13/2025

Intro: In this Do-file, we run the balance table

Author: Oscar Andres Garnica Toro
*/


clear all 
set more off

*globals
global cd "directory" 
global data "$cd/2_data"
global output "$cd/3_output"
global tables "$output/1_tables"


*load dataset 
use "$data/1b_panel_data.dta", clear 

*keep baseline obs 
keep if survey==0

*treatment groups
tab usergroup, gen(treatment)
	rename treatment1 control
	rename treatment2 pref_treat
	rename treatment3 ai_treat
	rename treatment4 ai_pref_treat

*create FE
gen female = 0
replace female = 1 if gender == "female"

gen white = 0 
replace white = 1 if race == "white"

gen age_18_24 = 0
replace age_18_24 = 1 if age <25

gen age_25_34 = 0
replace age_25_34 = 1 if age >= 25 & age <35 

gen age_35_44 = 0
replace age_35_44 = 1 if age >= 35 & age <45

gen age_45_54 = 0
replace age_45_54 = 1 if age >= 45 & age <55

gen age_55_64 = 0
replace age_55_64 = 1 if age >= 55 & age <65

gen age_65_plus = 0
replace age_65_plus = 1 if age >=65

egen strata = group(female white age_18_24 age_25_34 age_35_44 age_45_54 age_55_64 age_65_plus)

*.Create variables
tab experimentcityid, g(city)
	rename city1 Atlanta
	rename city2 Chicago
	rename city3 Cleveland
	rename city4 LasVegas
	rename city5 Phoenix
	rename city6 BayArea 
	rename city7 Seattle
	
*.Lets build the table	for the whole sample 
{
	foreach x in female white age_18_24 age_25_34 age_35_44 age_45_54 age_55_64 ///
	age_65_plus Atlanta Chicago Cleveland LasVegas Phoenix BayArea Seattle {
		
		reg `x' pref_treat ai_treat ai_pref_treat

		*test of equality p-value
		test pref_treat=ai_treat=ai_pref_treat	
		gen p=string(r(p),"%03.2f")
		

		preserve
			tempfile `x'_tbl
			egen fullsample = mean(`x')
			collapse (mean) `x', by(usergroup p fullsample)	
			tab usergroup `x'
			gen category="`x'"
			destring usergroup, replace 
			reshape wide `x', i(category) j(usergroup)
			rename (`x'0 `x'1 `x'2 `x'3) (control pref_treat ai_treat ai_pref_treat)
			order fullsample, a(category)
			order p, a(ai_pref_treat)
			save ``x'_tbl'
		restore
		
		drop p 
	}
		
		
	*Sample size
	preserve
		tempfile sample_size
		gen n_sample = 1 
		collapse (count) n_sample, by(usergroup)
		gen category = "n_sample"
		destring usergroup, replace
		reshape wide n_sample, i(category) j(usergroup)
		egen fullsample = rowtotal(n_sample0 n_sample1 n_sample2 n_sample3)
		order fullsample, a(category)
		rename (n_sample0 n_sample1 n_sample2 n_sample3) (control pref_treat ai_treat ai_pref_treat)
		save `sample_size'
	restore
	
	*test if any of the characteristics predict being in the treatment group 
	global X "female white age_18_24 age_25_34 age_35_44 age_45_54 age_55_64 age_65_plus Atlanta Chicago Cleveland LasVegas Phoenix BayArea Seattle"
	reg control $X , robust
	
	* Table Consolidation 
		use `sample_size', clear 
		append using `female_tbl'
		append using `white_tbl'
		append using `age_18_24_tbl'
		append using `age_25_34_tbl'
		append using `age_35_44_tbl'
		append using `age_45_54_tbl'
		append using `age_55_64_tbl'
		append using `age_65_plus_tbl'
		append using `Atlanta_tbl'
		append using `Chicago_tbl'
		append using `Cleveland_tbl'
		append using `LasVegas_tbl'
		append using `Phoenix_tbl'
		append using `BayArea_tbl'
		append using `Seattle_tbl'
		
		
	/*
		use `female_tbl', clear 
		append using `white_tbl'
		append using `age_18_24_tbl'
		append using `age_25_34_tbl'
		append using `age_35_44_tbl'
		append using `age_45_54_tbl'
		append using `age_55_64_tbl'
		append using `age_65_plus_tbl'
		append using `Atlanta_tbl'
		append using `Chicago_tbl'
		append using `Cleveland_tbl'
		append using `LasVegas_tbl'
		append using `Phoenix_tbl'
		append using `BayArea_tbl'
		append using `Seattle_tbl'
		append using `sample_size'
	*/
		
		
	* Export table to Excel
	export excel using "$tables/3a_sample_balance.xlsx", sheet("SampleBalance") firstrow(variables) replace

}	
