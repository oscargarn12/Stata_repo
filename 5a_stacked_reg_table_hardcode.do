clear all 
set more off

*globals
global cd "/directory" 
global data "$cd/2_data"
global output "$cd/3_output"
global tables "$output/1_tables"


*load dataset 
use "$data/1b_panel_data.dta", clear 

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

gen wave = 0 
replace wave = 1 if createdate<date("20/march/2024", "DMY") 
replace wave = 2 if createdate>=date("20/march/2024", "DMY") 

gen month_enrollment = month(createdate)

gen month_answer = month(completedate)

encode experimentcityid, gen(experimentcityid_e)

*baseline employment
gen bl_employed =.
bys userid: replace bl_employed = 1 if survey==0 & (job_employment_status == ///
"Employed full-time" | job_employment_status == "Employed part-time")
replace bl_employed = 0 if survey==0 & (job_employment_status == ///
"Retired" | job_employment_status == "Unemployed")
bysort userid (bl_employed): replace bl_employed = bl_employed[_n-1] if missing(bl_employed)

gen bl_missing = (missing(bl_employed))
replace bl_employed = 0 if bl_missing == 1

*baseline job search hours
gen bl_search_hours = .
replace bl_search_hours = job_search_hours if survey ==0
bys userid (bl_search_hours): replace bl_search_hours = bl_search_hours[_n-1] if missing(bl_search_hours)

*baseline applications
gen bl_applications = .
replace bl_applications = job_search_applications if survey==0
bys userid (bl_applications): replace bl_applications = bl_applications[_n-1] if missing(bl_applications)

*baseline interviews 
gen bl_interviews = .
replace bl_interviews = job_search_interviews if survey ==0
bys userid (bl_interviews): replace bl_interviews = bl_interviews[_n-1] if missing(bl_interviews)

*baseline job offers 
gen bl_job_offers = .
replace bl_job_offers = job_search_offers if survey == 0 
bys userid (bl_job_offers): replace bl_job_offers = bl_job_offers[_n-1] if missing(bl_job_offers)

*baseline hourly pay
gen hourly_pay_wins =  job_hourly_pay
replace hourly_pay_wins = (hourly_pay_wins/50)/job_weekly_hours if ///
hourly_pay_wins>15000
replace hourly_pay_wins = . if hourly_pay_wins == 0
replace hourly_pay_wins = 100 if hourly_pay_wins>100 & ///
hourly_pay_wins!=.

gen bl_job_hourly_pay = .
replace bl_job_hourly_pay = hourly_pay_wins if survey ==0
bys userid (bl_job_hourly_pay): replace bl_job_hourly_pay = bl_job_hourly_pay[_n-1] if missing(bl_job_hourly_pay)

*baseline job satisfaction 
gen sat_overall_e = .
replace sat_overall_e = 7 if sat_overall == "Strongly agree"
replace sat_overall_e = 6 if sat_overall == "Agree"
replace sat_overall_e = 5 if sat_overall == "Somewhat agree"
replace sat_overall_e = 4 if sat_overall == ///
"Neither agree nor disagree"
replace sat_overall_e = 3 if sat_overall == "Somewhat disagree"
replace sat_overall_e = 2 if sat_overall == "Disagree"
replace sat_overall_e = 1 if sat_overall == "Strongly disagree"

gen bl_job_satisf = .
replace bl_job_satisf = sat_overall_e if survey == 0
bys userid (bl_job_satisf): replace bl_job_satisf = bl_job_satisf[_n-1] if missing(bl_job_satisf)

*baseline tenure
gen tenure = datediff(job_start, job_last_employed, "month")
replace tenure = datediff(job_start, survey_createdate, "month") ///
if tenure ==.		
replace tenure = . if tenure < 0 //these are people with start_date>finish_date 

gen bl_tenure = .
replace bl_tenure = tenure if survey ==0
bys userid (bl_tenure): replace bl_tenure = bl_tenure[_n-1] if missing(bl_tenure)

gen bl_tenure_missing = (missing(bl_tenure))
replace bl_tenure = 0 if bl_tenure_missing == 1

*drop baseline survey
drop if survey==0

*drop follow-ups not completed 
drop if survey_createdate ==.


*Employment status

gen employed =.
replace employed = 1 if job_employment_status == "Employed full-time" | ///
job_employment_status == "Employed part-time"
replace employed = 0 if job_employment_status == "Retired" | ///
job_employment_status == "Unemployed"

reghdfe employed pref_treat ai_treat ai_pref_treat bl_employed ///
i.bl_missing, absorb(month_enrollment month_answer i.strata ///
i.experimentcityid_e wave) vce(robust)
	lincom pref_treat - ai_treat
	local coef1 = r(estimate)
	local se1 = r(se)
	local t1 = `coef1'/`se1'
	local pv1 = r(p)
	
	lincom pref_treat - ai_pref_treat
	local coef2 = r(estimate)
	local se2 = r(se)
	local t2 = `coef1'/`se1'
	local pv2 = r(p)
	
	lincom ai_treat - ai_pref_treat
	local coef3 = r(estimate)
	local se3 = r(se)
	local t3 = `coef1'/`se1'
	local pv3 = r(p)
	
	sum employed if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
reg employed pref_treat ai_treat ai_pref_treat bl_employed i.strata, r

// DEFINE ROW AND COLUMNS OF TABLE

gen varname = ""
forvalues i = 1/10 {
	gen c`i' = ""
	gen stars`i' = ""
}

replace varname = "Preference " in 1
replace varname = "Score " in 2
replace varname = "(T1) " in 3
*p
*q

replace varname = "AI " in 5
replace varname = "Score" in 6 //p
replace varname = "(T2)" in 7 //q

replace varname = "Preference +" in 9
replace varname = "AI Score" in 10 //p
replace varname = "(T3)" in 11 //q


replace varname = "Pr(T1 = T2)" in 14
replace varname = "Pr(T3 = T1)" in 15
replace varname = "Pr(T3 = T2)" in 16


replace varname = "R-squared" in 19
replace varname = "Control Mean" in 20
replace varname = "Control S.D." in 21
replace varname = "Observations" in 22

// MAKE PROGRAMS
cap drop baseline

cap program drop ancovarow
program define ancovarow

** Mean SD control
quietly sum `2' if usergroup==0 
quietly replace c`1' = string(r(mean),"%4.2f") in 20
quietly replace c`1' = string(r(sd),"%4.2f") in 21

** regression coefficients

gen baseline = `3'
sum baseline
replace baseline = r(mean) if baseline==.
gen nobaseline = (`3'==.)
reghdfe `2'  pref_treat ai_treat ai_pref_treat baseline nobaseline, absorb(month_enrollment month_answer i.strata ///
i.experimentcityid_e wave) vce(robust) 

mat V = r(table)

*matlist V

drop baseline nobaseline



quietly replace c`1' = string(V[1,1],"%4.2f") in 1
quietly replace c`1' = string(V[2,1],"%4.2f") in 2
quietly replace c`1' = string(V[4,1],"%4.2f") in 3
quietly replace c`1' = "("+c`1'+")" in 2
glo p_val_`1'_T1= V[4,1]
local stars = V[4,1]
quietly replace stars`1' = "*" in 1 if `stars'<0.1
quietly replace stars`1' = "**" in 1 if `stars'<0.05
quietly replace stars`1' = "***" in 1 if `stars'<0.01

quietly replace c`1' = string(V[1,2],"%4.2f") in 5
quietly replace c`1' = string(V[2,2],"%4.2f") in 6
quietly replace c`1' = string(V[4,2],"%4.2f") in 7
quietly replace c`1' = "("+c`1'+")" in 6
glo p_val_`1'_T2= V[4,2]
local stars = V[4,2]
quietly replace stars`1' = "*" in 5 if `stars'<0.1
quietly replace stars`1' = "**" in 5 if `stars'<0.05
quietly replace stars`1' = "***" in 5 if `stars'<0.01

quietly replace c`1' = string(V[1,3],"%4.2f") in 9
quietly replace c`1' = string(V[2,3],"%4.2f") in 10
quietly replace c`1' = string(V[4,3],"%4.2f") in 11
quietly replace c`1' = "("+c`1'+")" in 10
glo p_val_`1'_T3= V[4,3]
local stars = V[4,3]
quietly replace stars`1' = "*" in 9 if `stars'<0.1
quietly replace stars`1' = "**" in 9 if `stars'<0.05
quietly replace stars`1' = "***" in 9 if `stars'<0.01

*matlist V
mat drop V

** tests of equality of treatment effects

test pref_treat = ai_treat
quietly replace c`1' = string(r(p),"%4.2f") in 14
quietly replace c`1' = "("+c`1'+")" in 14
test ai_pref_treat = pref_treat
quietly replace c`1' = string(r(p),"%4.2f") in 15
quietly replace c`1' = "("+c`1'+")" in 15
test ai_pref_treat = ai_treat
quietly replace c`1' = string(r(p),"%4.2f") in 16
quietly replace c`1' = "("+c`1'+")" in 16


** other statistics

quietly replace c`1' = string(e(r2),"%4.2f") in 19
quietly replace c`1' = string(e(N)) in 22


** combine columns to include stars

*egen col`1' = concat(c`1' )

end

* Each element below represent the following terms in the code 
*     	 `1'    `2'      `3'      
*olsrow 1 m_foodsec 
ancovarow 1 employed bl_employed
ancovarow 2 job_search_hours bl_search_hours
ancovarow 3 job_search_applications bl_applications
ancovarow 4 job_search_interviews bl_interviews
ancovarow 5 job_search_offers bl_job_offers
ancovarow 6 hourly_pay_wins bl_job_hourly_pay
ancovarow 7 sat_overall_e bl_job_satisf
ancovarow 8 tenure bl_tenure


*keep varname col*
forvalues i = 1/8 {
 replace c`i' = " & " + c`i'
}

* Keep only the first 22 rows and relevant columns
keep in 1/22
keep varname c1 stars1 c2 stars2 c3 stars3 c4 stars4 ///
     c5 stars5 c6 stars6 c7 stars7 c8 stars8

* Open LaTeX file and write header
file open texout using "$tables/summary_table.tex", write replace
file write texout "\begin{tabular}{lcccccccc}" _n
file write texout "\toprule" _n
file write texout " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\" _n
file write texout "\midrule" _n

* Loop over each row
forvalues i = 1/22 {
    local vname = varname[`i']
    local line = "`vname'"
    
    forvalues j = 1/8 {
        local rawcell = c`j'[`i']
        local rawstar = stars`j'[`i']

        * Remove leading ampersands if present
        local cell = subinstr("`rawcell'", "&", "", .)
        local star = subinstr("`rawstar'", "&", "", .)

        * Append to row line
        local line "`line' & `cell'`star'"
		
    }

    file write texout "`line' \\\\" _n
}

* Write footer
file write texout "\bottomrule" _n
file write texout "\end{tabular}" _n
file close texout


