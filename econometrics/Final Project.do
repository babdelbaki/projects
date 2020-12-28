/* 

final

1? effect of obesity on poverty rate
2? does increasing recreation centers have an effect on obesity rate? keep in 
	mind low pops inflate rates - maybe add interaction
3? does increasing primary care physician have an effect on obesity rate
4? college education and obesity rate
5? education expentiture and obesity rate


cvs:
unemployment? - maybe multicollinearity
age
race 
sexratio
population size (make groups?)
education level
education spending

interaction between race and umeployment/poverty - see if effect 



*/
*basic setup of data
cd "C:\Users\Able Care\Documents\Stata\data 5"
import excel using "Data 5.xlsx", clear firstrow

ssc install estout, replace
drop nophys Food_Stores_per_100000 Fast_Food_per_100000 Education_Expenditure_per_Studen Primary_Care_Physicians_per_1000 Violent_Crime_per_100000 Fast_Food_per_100000 County_FIPS State StateName Area_Name County_Name County_Name_State County_State County_State_Name diab

drop if pop<5000
*dropped counties below 5000


*label variables
label var obes 		"Obesity Rate (%)"
label var pove		"Poverty Rate (%)"
label var median 	"Median Income (2012 USD)"
label var unem		"Unemployment Rate (%)"
label var pop		"Population"
label var Age_4564	"Age 45-64"
label var Age_65	"Age 65+"
label var Age_45	"Age 45+"
label var SexRatio	"Sex Ratio (M:F)"
label var white		"White, non-hispanic (%)"
label var black		"Black, non-hispanic (%)"
label var asian		"Asian-American (%)"
label var hispanic	"Hispanic (%)"
label var high_school	"High School Diploma"
label var bachelors		"Bachelor's Degree"
label var rec			"Recreation Centers per 100,000"
	
*descriptive statistics
estpost tabstat *, stats(mean sd sk p5 p25 p50 p75 p95) ///
                      column(statistics)
esttab using final_tabstat.rtf, ///
           cells("mean(fmt(2)) sd skewness p5 p25 p50 p75 p95") ///
           replace ///
           label ///
           varwidth(35) ///
           nomtitles ///
           nonumbers ///
           title("2012 United States County Data") ///
           addnote("Source: Centers for Disease Control and Prevention")
					  
					  
*corr table
quietly: estpost corr *, matrix listwise
esttab using final_corr.rtf, ///
       replace ///
	   b(3) ///
       plain ///
       nonumbers ///
       unstack ///
       not ///
       compress ///
       title("2012 United States County Data") ///
       addnote("Source: Centers for Disease Control and Prevention")

*histograms
hist obes, ///
     start(10) width(2.5) frequency mlabsize(half_tiny) ///
     xlabel(10(5)50, labsize(small) format(%-9.0gc)) ///
     xtick(10(2.5)50)  ///
     xtitle("Obesity Rate (%)", size(medsmall))  ///
     ylabel(, angle(horizontal) format(%9.0gc))  ///
     subtitle("Histogram for 2012 US County Obesity Rates") ///
     note("Note:  Number of observations is 2833"  ///
          "Source:  Centers for Disease Control and Prevention") ///
     name("hist_final_obes", replace)
	 
hist rec, ///
     start(0) width(2) frequency mlabsize(half_tiny) ///
     xlabel(0(4)80, labsize(small) format(%-9.0gc)) ///
     xtick(0(2)80)  ///
     xtitle("Recreation Centers per 100,000", size(medsmall))  ///
     ylabel(, angle(horizontal) format(%9.0gc))  ///
     subtitle("Histogram for 2012 US County Recreation Center Prevalence") ///
     note("Note:  Number of observations is 2833"  ///
          "Source:  Centers for Disease Control and Prevention") ///
     name("hist_final_rec", replace)

graph combine hist_final_obes hist_final_rec, col(1) xsize(1.5) ysize(2)
graph export hists_final.pdf, replace
	 
*scatterplots
reg obes rec, robust
predict fitted
gen resids = fitted - obes
graph twoway (lfit obes rec) (scatter obes rec), ///
      title("County Obesity Rates vs Recreation Center Prevalence") ///
      ytitle("County Obesity Rate (%)", size(medsmall)) ///
	  ylabel(10(5)50) ///
	  xtitle("Recreation Centers per 100,000") ///
	  xlabel(0(4)80) ///
	  note("Note:  2833 Counties in the United States (2012)" /// 
			"Source:  Centers for Disease Control and Prevention") ///
	  name("scatter_obes_rec", replace)
graph twoway (lfit resids rec) (scatter resids rec), ///
      subtitle("Residual Plot") ///
      ytitle("Residuals", size(medsmall)) ///
	  ylabel(-20(5)20) ///
	  xtitle("Recreation Centers per 100,000") ///
	  xlabel(0(4)80) ///
	  note("Note:  2833 Counties in the United States (2012)" /// 
			"Source:  Centers for Disease Control and Prevention")	///
	  name("scatter_resids_rec", replace)
graph combine scatter_obes_rec scatter_resids_rec, col(1) xsize(1.5) ysize(2)
graph export scatters_final.pdf, replace

/*regressions
maybe use VIF, heteroscedasticity and robust standard errors, 
OVB, multicollinearity, non-linearities (such as quadratic, logs, etc),
dummy, interactions, categorical dummies, percentages, 
“normalized” variables (such as “per capita” measures), 
hypothesis testing, statistical significance, practical relevance, adjusted R2
*/

/*
maybe rec interactions with pov, race ones, age ones

*/
*economic indicator
eststo clear   /* clear any regressions that may be already stored in memory */
eststo:  quietly  regress obes rec, robust
eststo:  quietly  regress obes rec pove, robust
eststo:  quietly  regress obes rec median, robust
eststo:  quietly  regress obes rec unem, robust
eststo:  quietly  regress obes rec pove median, robust
eststo:  quietly  regress obes rec pove unem, robust
eststo:  quietly  regress obes rec median unem, robust
eststo:  quietly  regress obes rec pove median unem, robust

esttab  using final_regs_1.rtf, ///
          r2 ar2 se scalar(F rmse) ///
          star(* 0.10 ** 0.05 *** 0.01) ///
          replace ///
          depvars ///
          varwidth(20) ///
          title("Regression results for 2008 CPS Data, Economic Indicator") ///
          nonotes ///
          addnote("Source:  Centers for Disease Control and Prevention" ///
                  "Significance levels:  * p<0.10; ** p<0.05; *** p<0.01")

*demographics 1 (pop, age, sex)
gen log_pop = log(pop)
eststo clear   /* clear any regressions that may be already stored in memory */
eststo:  quietly  regress obes rec median , robust
eststo:  quietly  regress obes rec median pop, robust
eststo:  quietly  regress obes rec median log_pop, robust
eststo:  quietly  regress obes rec median log_pop Age_4564, robust
eststo:  quietly  regress obes rec median log_pop Age_65, robust
eststo:  quietly  regress obes rec median log_pop Age_4564 Age_65, robust
eststo:  quietly  regress obes rec median log_pop Age_45, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio, robust

esttab  using final_regs_2.rtf, ///
          r2 ar2 se scalar(F rmse) ///
          star(* 0.10 ** 0.05 *** 0.01) ///
          replace ///
          depvars ///
          varwidth(20) ///
          title("Regression results for 2012 US County Data, Demographics (1)") ///
          nonotes ///
          addnote("Source:  Centers for Disease Control and Prevention" ///
                  "Significance levels:  * p<0.10; ** p<0.05; *** p<0.01")

*demographics 2 (race, education)
eststo clear   /* clear any regressions that may be already stored in memory */
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio white, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio black, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio asian, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio hispanic, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio white black asian hispanic, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio white black asian hispanic high_school, robust
eststo:  quietly  regress obes rec median log_pop Age_65 SexRatio white black asian hispanic high_school bachelors, robust

esttab  using final_regs_3.rtf, ///
	r2 ar2 se scalar(F rmse) ///
          star(* 0.10 ** 0.05 *** 0.01) ///
          replace ///
          depvars ///
          varwidth(20) ///
          title("Regression results for 2012 US County Data, Demographics (2)") ///
          nonotes ///
          addnote("Source:  Centers for Disease Control and Prevention" ///
                  "Significance levels:  * p<0.10; ** p<0.05; *** p<0.01")

*interactions
gen rec_black = rec * black
gen rec_pop = rec * log_pop
gen rec_sex = rec * SexRatio
gen rec_bh = rec * bachelors			  
eststo clear   /* clear any regressions that may be already stored in memory */
eststo:  quietly regress obes rec Age_65 SexRatio white black asian hispanic high_school bachelors, robust
eststo:  quietly regress obes rec Age_65 SexRatio white black asian hispanic high_school bachelors rec_black, robust
eststo:  quietly regress obes rec Age_65 SexRatio white black asian hispanic high_school bachelors rec_sex, robust
eststo:  quietly regress obes rec Age_65 SexRatio white black asian hispanic high_school bachelors rec_bh, robust
eststo:  quietly regress obes rec Age_65 SexRatio white black asian hispanic high_school bachelors rec_black rec_sex, robust



esttab  using final_regs_4.rtf, ///
	r2 ar2 se scalar(F rmse) ///
          star(* 0.10 ** 0.05 *** 0.01) ///
          replace ///
          depvars ///
          varwidth(20) ///
          title("Regression results for 2012 US County Data, Interactions") ///
          nonotes ///
          addnote("Source:  Centers for Disease Control and Prevention" ///
                  "Significance levels:  * p<0.10; ** p<0.05; *** p<0.01")
