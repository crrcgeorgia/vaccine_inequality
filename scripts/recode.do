clear

cls

// use "data/NDI_July_2021_27.07.2021.dta"

use "https://caucasusbarometer.org/downloads/NDI_2021_Jul_28.07.21_Public.dta"

svyset ID[pweight=WTIND]

gen ownership=0
foreach var of varlist OWN* {
replace ownership = ownership + 1 if `var'== 1
}

recode RESPEDU (-2 1/4=0) (5/6=1), gen(hied)

recode ETHNIC (3=1)(.=.)(else=0), gen(ethnic)

recode PARTSUPP (1=1 "Government")(-5 -2 -1 = 3 "Unaffiliated") (-9 -3 =.) (nonmissing=2 "Opposition"), gen(party)

recode WILLVACC19 (-5=1) (-9 -3=.) (nonmissing=0), gen(vaccinated)

recode WILLVACC19 (-9=.) (-2 -1 0 = 0), gen(know_booking)

lab def vaccinated 0 "Not vaccinated" 1 "Vaccinated", modify

lab val vaccinated vaccinated

// recode q20 (-5=3) (-9 -3=.) (1=1) (0=0) (-2/-1=98)

recode INFC19VAC (0=1) (-9 -3=.) (nonmissing=0), gen(not_enough_information)

rename RESPSEX sex

rename AGEGROUP agegroup

rename SETTYPE stratum

svy: logit vaccinated i.sex i.agegroup i.stratum hied ethnic ownership not_enough_information i.party

svy: logit not_enough_information i.sex i.agegroup i.stratum hied ethnic ownership  i.party

svy: logit know_booking not_enough_information i.sex i.agegroup i.stratum hied ethnic ownership  i.party

// Test parameters of significant covariates

foreach x of varlist agegroup stratum {

testparm i.`x'

}

// Export tables

tabout vaccinated using "tables/frequency/vaccinated.csv", c(column) clab(prop) svy replace percent ptotal(none)

foreach x in agegroup stratum hied not_enough_information ownership {

tabout vaccinated `x' using "tables/crosstabs/vaccinated_`x'.csv", c(column) svy replace percent ptotal(none) h1(nil) h3(nil)

}

foreach x in agegroup ethnic stratum hied ownership {

tabout not_enough_information `x' using "tables/crosstabs/not_enough_information_`x'.csv", c(column) svy replace percent ptotal(none) h1(nil) h3(nil)

}


foreach x in not_enough_information agegroup stratum hied ethnic ownership {

tabout know_booking `x' using "tables/crosstabs/know_booking_`x'.csv", c(column) svy replace percent ptotal(none) h1(nil) h3(nil)

}

