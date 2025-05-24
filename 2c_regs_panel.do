/*
Date: May/13/2025

Intro: In this Do-file, we run panel regs with the panel dataset

Author: Oscar Andres Garnica Toro
*/


clear all 
set more off

*globals
global cd "/Users/oscarandresgarnicatoro/Good Business Lab Dropbox/AIJS" 
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

*create after variable
gen after = 1
replace after = 0 if survey ==0 

*drop follow-ups not completed 
drop if survey_createdate ==.


*Employment status
{
gen employed =.
replace employed = 1 if job_employment_status == "Employed full-time" | ///
job_employment_status == "Employed part-time"
replace employed = 0 if job_employment_status == "Retired" | ///
job_employment_status == "Unemployed"

reghdfe employed pref_treat ai_treat ai_pref_treat ///
i.bl_missing after, absorb(month_enrollment month_answer i.strata ///
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
	
	outreg2 using "$tables/2b_panel_regs.txt", replace ctitle("Employment Status")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


*Search hours
{
reghdfe job_search_hours pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum job_search_hours if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Job Searching Hours")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


*number of applications
{
reghdfe job_search_applications pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum job_search_applications if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Job Applications")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')	
}


*number of interviews
{
reghdfe job_search_interviews pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum job_search_interviews if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Job Interviews")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')	
}


*number of job offers
{
reghdfe job_search_offers pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum job_search_offers if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Job Offers")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


*clicks
{
reghdfe clicks pref_treat ai_treat ai_pref_treat after, ///
absorb(month_enrollment month_answer i.strata ///
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
	
	sum clicks if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Clicks")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


*saves
{
reghdfe saves pref_treat ai_treat ai_pref_treat after, ///
absorb(month_enrollment month_answer i.strata ///
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
	
	sum saves if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Saved Jobs")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


*hourly pay 
{
reghdfe hourly_pay_wins pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum hourly_pay_wins if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Hourly Pay")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
	
}


*Job satisfaction
{
reghdfe sat_overall_e pref_treat ai_treat ai_pref_treat ///
after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum sat_overall_e if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Job Satisfaction")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')

}


*Tenure
{
reghdfe tenure pref_treat ai_treat ai_pref_treat ///
i.bl_tenure_missing after, absorb(month_enrollment month_answer i.strata ///
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
	
	sum tenure if control == 1
	local control_mean = r(mean)
	local control_sd = r(sd)
	
	outreg2 using "$tables/2b_panel_regs.txt", append ctitle("Tenure")  ///
	addstat("pref_treat - ai_treat", `coef1', "SE", `se1', "t-stat", `t1', "P-value", `pv1', ///
	"pref_treat - ai_pref_treat", `coef2', "SE", `se2', "t-stat", `t2', "P-value", `pv2', ///
	"ai_treat - ai_pref_treat", `coef3', "SE", `se3', "t-stat", `t3', ///
	"P-value", `pv3', "control_mean", `control_mean', "control_sd", `control_sd')
}


