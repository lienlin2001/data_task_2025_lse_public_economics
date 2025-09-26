* Data Task - LSE Public Economics
* Author: Li-En Lin
* Date: 2025-03-18
///////////////////////////////////////////////////////////////////////////////

*** Data Cleaning ***

* Setting directory
cd "/Users/lienlin/Desktop/DataTask/DataTask_LSE_LSEPublic"
// Please change your working directory here

* Import dataset
use "sample_75_02.dta", clear

* Merge datasets
merge 1:1 penr file using work_history.dta
drop _merge


* Sample restriction
keep if duration >= 365 & duration < 1825 & dempl5 >= 365 & dempl5 < 1825 
keep if volquit == 0 & recall == 0 & region != 0
keep if endy > 1980 & endy < 2002
/// Following this criteria we can get the exact number as the paper 
// Construction worker is already removed
// The dataset also exclude those age above 50 or below 20.


*** Replication of figures ***
gen month_length = 31
gen sevpay_eligible = 1096 // 3 years
gen prevjob_tenure_sevpay = floor((duration - sevpay_eligible)/month_length) + 36
gen eb_eligible = 1096
gen prevjob_demp_eb = floor((dempl5 - sevpay_eligible)/month_length) + 36

// Dummy variables for job tenure
tab prevjob_tenure_sevpay, gen(prevjob_tenure_sevpay_dum_)

// Dummy variables for employment years
tab prevjob_demp_eb, gen(prevjob_demp_eb_dum_)

// Variables for job tenure
gen duration_2 = duration^2
gen duration_3 = duration^3
gen duration_centered = duration - sevpay_eligible
gen duration_2_centered = (duration - sevpay_eligible)^2
gen duration_3_centered = (duration - sevpay_eligible)^3
gen eligible_for_sevpay = duration >= sevpay_eligible

gen eligxdura_1 = duration_centered * eligible_for_sevpay
gen eligxdura_2 = duration_2_centered * eligible_for_sevpay
gen eligxdura_3 = duration_3_centered * eligible_for_sevpay

// Variabls for employment years
gen dempl5_2 = dempl5^2
gen dempl5_3 = dempl5^3
gen dempl5_centered = dempl5 - eb_eligible
gen dempl5_2_centered = (dempl5 - eb_eligible)^2
gen dempl5_3_centered = (dempl5_3 - eb_eligible)^3
gen eligible_for_eb = dempl5 >= eb_eligible

gen eligxdemp_1 = dempl5_centered * eligible_for_eb
gen eligxdemp_2 = dempl5_2_centered * eligible_for_eb
gen eligxdemp_3 = dempl5_3_centered * eligible_for_eb


* Figure II
// Frequency of layoffs by severance payment
bys prevjob_tenure_sevpay: gen freq_layoff = _N // _N means total number of observation in the dataset
bys prevjob_tenure_sevpay: gen rep_prevjob_tenure_sevpay = _n // _n mean the number of appearance
replace freq_layoff =. if prevjob_tenure_sevpay <= 12 | prevjob_tenure_sevpay >= 59
#delimit;
twoway scatter freq_layoff prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1, c(l)
xline(35.5) graphregion(fcolor(white)) legend(off) msize(medsmall) xlabel(12(6)60) 
title(Figure 2) subtitle(Frequency of Layoffs by Job Tenure) xtitle(Previous Job Tenure (Months)) ytitle(Number of Layoffs);
#delimit cr
graph export figure2.png, replace

* Figure III

// Figure 3a
cap drop rep_prevjob_tenure_sevpay
bys prevjob_tenure_sevpay: egen mean_numemployment = mean(last_break)
bys prevjob_tenure_sevpay : gen rep_prevjob_tenure_sevpay = _n 
#delimit;
twoway scatter mean_numemployment prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay == 1, 
msize(medsmall) xlabel(12(6)60) xline(35.5) graphregion(fcolor(white)) legend(off)  
title(Figure 3a) subtitle(Number of Jobs by Job Tenure) xtitle(Previous Job Tenure (Months)) ytitle(Mean Number of Jobs)
|| lfit mean_numemployment prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1&prevjob_tenure_sevpay<35.5 
|| lfit mean_numemployment prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1&prevjob_tenure_sevpay>35.5;
#delimit cr
graph export figure3a.png, replace

// Figure 3b
gen annual_wage = wage0 *12
cap drop rep_prevjob_tenure_sevpay
bys prevjob_tenure_sevpay: egen mean_wage = mean(annual_wage)
bys prevjob_tenure_sevpay : gen rep_prevjob_tenure_sevpay = _n if wage0 !=.
#delimit;
twoway scatter mean_wage prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay == 1, 
msize(medsmall) xlabel(12(6)60) xline(35.5) graphregion(fcolor(white)) legend(off)  
title(Figure 3b) subtitle(Wage by Job Tenure) xtitle(Previous Job Tenure (Months)) ytitle(Mean Annual Wage)
|| lfit mean_wage prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1&prevjob_tenure_sevpay<35.5 
|| lfit mean_wage prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1&prevjob_tenure_sevpay>35.5;
#delimit cr
graph export figure3b.png, replace


* Figure V
// Mean unemployment duration with Previous Job Tenure
cap drop rep_prevjob_tenure_sevpay
bys prevjob_tenure_sevpay: gen rep_prevjob_tenure_sevpay = _n if noneduration <= 2*365
bys prevjob_tenure_sevpay: egen mean_noneduration = mean(noneduration) if noneduration <= 2*365
// Exclude observation a nonemployment duration of more than two years to eliminate the long right tail of the distribution.
replace mean_noneduration =. if prevjob_tenure_sevpay <= 12 | prevjob_tenure_sevpay >= 59

#delimit;
twoway scatter mean_noneduration prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay == 1,
msize(medsmall) xlabel(12(6)60) xline(35.5) graphregion(fcolor(white)) legend(off) yscale(range(142.5 167.5)) ylabel(145(5)165)
title(Figure 5) subtitle(Effect of Severance Pay on Nonemployment Durations) xtitle(Previous Job Tenure (Months)) ytitle(Mean Nonemployment Duration)
|| qfit mean_noneduration prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay == 1 & prevjob_tenure_sevpay < 35.5
|| qfit mean_noneduration prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay == 1 & prevjob_tenure_sevpay > 35.5;
#delimit cr

graph export figure5.png, replace
// superimpose a quadratic regression model fit separately to points on the right and left of the eligibility threshold.

* Figure VI

// Running the hazard model
gen cens =  noneduration >= 140 
// we censor the spells at 140 days in order to isolate the effects of the policy variables in the first twenty weeks of job search

stset noneduration, failure(cens==0)
stcox prevjob_tenure_sevpay_dum_2-prevjob_tenure_sevpay_dum_23 prevjob_tenure_sevpay_dum_25-prevjob_tenure_sevpay_dum_47 eligible_for_eb dempl5 dempl5_2 dempl5_3 eligxdemp_1-eligxdemp_3, nohr

// Plotting Figure 6
cap drop rep_prevjob_tenure_sevpay
bys prevjob_tenure_sevpay: gen rep_prevjob_tenure_sevpay = _n if noneduration <= 2*365
gen hazard_rate = .
foreach X of numlist 2/23,25/47 {
	replace hazard_rate = _b[prevjob_tenure_sevpay_dum_`X'] if rep_prevjob_tenure_sevpay ==1 & prevjob_tenure_sevpay_dum_`X'==1
}

#delimit;
twoway scatter hazard_rate prevjob_tenure_sevpay if rep_prevjob_tenure_sevpay==1, 
xline(35.5) msize(medsmall) xlabel(12(6)60) graphregion(fcolor(white)) legend(off)
title(Figure 6) subtitle(Effect of Severance Pay on Job Finding Hazards) xtitle(Previous Job Tenure (Months)) ytitle(Average Daily Job Finding Hazard in First 20 Weeks)
|| qfit hazard_rate prevjob_tenure_sevpay if prevjob_tenure_sevpay<35.5&rep_prevjob_tenure_sevpay==1 
|| qfit hazard_rate prevjob_tenure_sevpay if prevjob_tenure_sevpay>35.5&rep_prevjob_tenure_sevpay==1;
#delimit cr

graph export figure6.png, replace

* Figure VIII

// Figure 8a
cap drop mean_noneduration
bys prevjob_demp_eb: egen mean_noneduration = mean(noneduration) if noneduration <= 2*365
replace mean_noneduration =. if prevjob_demp_eb <= 12 | prevjob_demp_eb >= 59
bys prevjob_demp_eb: gen rep_prevjob_demp_eb = _n // _n mean the number of

#delimit;
twoway scatter mean_noneduration prevjob_demp_eb if rep_prevjob_demp_eb == 1,
msize(medsmall) xlabel(12(6)60) xline(35.5) graphregion(fcolor(white)) legend(off) yscale(range(142.5 167.5)) ylabel(145(5)165)
title(Figure 8a) subtitle(Effect of Benefit Extension on Nonemployment Durations) xtitle(Months Employed in Past Five Years (Months)) ytitle(Mean Nonemployment Duration)
|| qfit mean_noneduration prevjob_demp_eb if rep_prevjob_demp_eb == 1 & prevjob_demp_eb < 35.5
|| qfit mean_noneduration prevjob_demp_eb if rep_prevjob_demp_eb == 1 & prevjob_demp_eb > 35.5;
#delimit cr

graph export figure8a.png, replace

// Figure 8b

// Running the hazard model
stcox prevjob_demp_eb_dum_2-prevjob_demp_eb_dum_23 prevjob_demp_eb_dum_25-prevjob_demp_eb_dum_47 eligible_for_sevpay duration duration_2 duration_3 eligxdura_1-eligxdura_3, nohr

// Plotting figure 8b
cap drop rep_prevjob_demp_eb
cap drop hazard_rate
bys prevjob_demp_eb: gen rep_prevjob_demp_eb = _n if noneduration <= 2*365
gen hazard_rate = .
foreach X of numlist 2/23,25/47 {
	replace hazard_rate = _b[prevjob_demp_eb_dum_`X'] if rep_prevjob_demp_eb ==1 & prevjob_demp_eb_dum_`X'==1
}

#delimit;
twoway scatter hazard_rate prevjob_demp_eb if rep_prevjob_demp_eb ==1, 
xline(35.5) msize(medsmall) xlabel(12(6)60) graphregion(fcolor(white)) legend(off)
title(Figure 8b) subtitle(Effect of Benefit Extension on Job Finding Hazards) xtitle(Months Employed in Past Five Years (Months)) ytitle(Average Daily Job Finding Hazard in First 20 Weeks)
|| qfit hazard_rate prevjob_demp_eb if prevjob_demp_eb < 35.5 & rep_prevjob_demp_eb ==1 
|| qfit hazard_rate prevjob_demp_eb if prevjob_demp_eb > 35.5 & rep_prevjob_demp_eb ==1;
#delimit cr

graph export figure8b.png, replace


*** Testing ***
// There might be difference in awareness of the unemployment benifit. Those with blue collar job might be less aware than white collar job worker, due to previously this policy only serve white collar workers.

// The logic is that, if white collar workers are more knowledgable about the policy of severance payment, and they might be more likely to get the severance payment since their employer will know they can't fool them. Combining with white collar workers has longer duration of searching for job, this may create the observed treatment effect.

sum noneduration if etyp == 1
sum noneduration if etyp ==2


bys prevjob_tenure_sevpay etyp: gen freq_layoff_etyp = _N // _N means total number of observation in the dataset
replace freq_layoff_etyp =. if prevjob_tenure_sevpay <= 12 | prevjob_tenure_sevpay >= 59

// White collar
#delimit;
twoway scatter freq_layoff_etyp prevjob_tenure_sevpay if etyp == 1, c(l)
xline(35.5) graphregion(fcolor(white)) legend(off) msize(medsmall) xlabel(12(6)60) 
title(Testing Figure 1) subtitle(Frequency of Layoffs by Job Tenure (White Collar)) xtitle(Previous Job Tenure (Months)) ytitle(Number of Layoffs);
#delimit cr

graph export testfigure1.png, replace


// Blue Collar
#delimit;
twoway scatter freq_layoff_etyp prevjob_tenure_sevpay if etyp == 2, c(l)
xline(35.5) graphregion(fcolor(white)) legend(off) msize(medsmall) xlabel(12(6)60) 
title(Testing Figure 2) subtitle(Frequency of Layoffs by Job Tenure (Blue Collar)) xtitle(Previous Job Tenure (Months)) ytitle(Number of Layoffs);
#delimit cr

graph export testfigure2.png, replace

// no obvious difference









