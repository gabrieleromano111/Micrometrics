*******************************************************
*******************************************************
*******************************************************
clear
*
*
*
*******************************************************
*********************QUESTION 1************************
*******************************************************
*
*
use pset_2_q_1
*
************* Point (a)


* Observing the graphs, one can notice a pattern of lower average levels of schooling in individuals born in the first quarter relative to the rest of the year, even though the data is not always consistent with the pattern and the pattern is not always salient. The relevance of the instrument is something that could and should be tested.

* First, we use the 'collapse' command to transform the dataset in a suitable way.

preserve
	collapse birthyear birthqtr Education , by(birthdate)
	gen birthdate_1900 = birthdate-1900

* Then, we produce the graphs and export them in .png format. 
	twoway connected Education birthdate_1900 if birthdate < 1940, mlabel(birthqtr) msymbol(s) mlabposition(6) ytitle("Years of Completed Education") xtitle("Year of Birth") xlabel(30(1)40)
	graph export I.png, replace
	
	twoway connected Education birthdate_1900 if birthdate>=1940 & birthdate < 1950, mlabel(birthqtr) msymbol(s) mlabposition(6) ytitle("Years of Completed Education") xtitle("Year of Birth") xlabel(40(1)50)
	graph export II.png, replace
	
	twoway connected Education birthdate_1900 if birthdate>=1950 & birthdate < 1960, mlabel(birthqtr) msymbol(s) mlabposition(6) ytitle("Years of Completed Education") xtitle("Year of Birth") xlabel(50(1)60)
	graph export III.png, replace

* Now, we go back to our previous dataset.
restore


************* Point (b)


*Exogeneity would arguably not be plausible since both quarter of birth and health status are both correlated with age

*The exclusion restriction, on the other hand, appears to be plausible, as we believe that quarter of birth has no direct effect on health

*In the setting considered by the paper the independent variable is schooling, the dependent is earnings and quarter of birth is used as an instrument. We identify the following potential problems with the two assumptions
*EXOGENEITY - Again, quarter of birth is 'mechanically' correlated with age and one could argue that earnings are correlated with age too. The acknowledge this issue and consider men between 40 and 49, whose wages, as they argue and verify, are hardly related to age  
*EXCLUSION - It is arguably the case that the exclusion restriction is satisfied and that quarter of birth does not have a direct effect on earnings


************* Point (c)


* OLS estimates should be biased upward. 

* This is because of the correlation of both schooling and earnings to variables that affect them in the same direction, such as verbal and quantitative skills. Intuitively, there is a "selection into treatment", as the earnings of those who have completed more years of schooling in the counterfactual case of them completing less are arguably not comparable to the earnings of those who actually completed less. This is because those that completed less schooling also have lower levels of verbal and quantitative skills, so that their earnings are lower not only as a consequence of the fewer years of schooling. Analogously, the earnings of those who have more years of schooling are higher not only as a consequence of their additional schooling.

* Let D, Y, Z be a binary causal variable, an outcome and a binary instrument respectively. Compliers are defined as those for whom
* D(Z=0) = 0 and D(Z=1) = 1
* If the outcome considered is health, the causal variable is education, and the instrument is quarter of birth, the compliers would be those for whom quarter of birth has an effect on education. As argued in Angrist and Krueger (1991), this is true for individuals that leave school at the minimum legal age possible.



*******************************************************
*********************QUESTION 2************************
*******************************************************


	clear 

* We import the relevant dataset.

	use pset_2_q_2_and_3.dta


************* Point (a)


* We first summarize the variable 'Healthy' and then we store its mean as a scalar named 'mu_y'.
	
	qui sum Healthy
	scalar mu_y = r(mean)


* We now summarize the variable 'Education' and then we store its mean as a scalar named 'mu_x'.
	
	qui sum Education
	scalar mu_x = r(mean)



************* Point (b)


* We generate the dummy variables for each quarter of birth. In this case we are aware of the values contained in 'birthqtr', hence, we can take a more explicit approach than with 'region'.
	
	forvalues i = 1/4 				{
	
		gen Quarter`i' = (birthqtr==`i')
	
	}  

* We generate the dummy variables for each region. Since a priori we do not know the values contained in 'region', we implement a general procedure. In particular, we exploit the 'gen()' option of the 'tab' command.
	
	foreach var in region birthyear {
		
		qui tab `var', gen(`var'_)
		
	}


* Now, we create the two local variables using the 'unab' command, which allows us not to list all controls. Note: we are including all the dummy variables. Since the regressions will be run with the constant, in order to avoid perfect multicollinearity, Stata will automatically drop one dummy for each covariates set. 
	
	unab Controls: Central Married region_*
	unab Birth_Year_FEs: birthyear_*


************* Point (c)


* We now run the three regressions with an increasing number of controls as requested and we store the estimates. We account for heteroskedasticity, computing robust standard errors.

	reg Healthy Education, robust
	estimates store ols_1
	reg Healthy Education `Controls', robust
	estimates store ols_2
	reg Healthy Education `Controls' `Birth_Year_FEs', robust
	estimates store ols_3



************* Point (d)


* We create the table on the Excel file using the stored outputs. 
	
	outreg2 [ols_1] using TABLE_Q_2.xls, excel replace nocons keep(Education) addtext(Controls, NO, Year of Birth FEs, NO) addstat("Mean y", mu_y, "Mean x", mu_x)

	outreg2 [ols_2] using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Year of Birth FEs, NO) addstat("Mean y", mu_y, "Mean x", mu_x)

	outreg2 [ols_3] using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Year of Birth FEs, YES) addstat("Mean y", mu_y, "Mean x", mu_x)



************* Point (e)


* We run the three IV regressions using Quarter1-Quarter3 as instrument and adding progressively more controls. We then store the estimates and save the F-statistic of the excluded instruments as a scalar. We take a conservative approach by computing robust standard errors.
	
	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3), robust
	estimates store iv_1
	scalar F_first_1 = e(widstat)
	
	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls', robust
	estimates store iv_2
	scalar F_first_2 = e(widstat)
	
	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust
	estimates store iv_3
	scalar F_first_3 = e(widstat)

	
************* Point (f)


* We procede in parallel with Point (d). We append to the table the estimate of the IV coefficient for Education, together with indication for the presence of controls and the F-statistic value.

	outreg2 [iv_1] using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, NO, Year of Birth FEs, NO) addstat("Mean y", mu_y, "Mean x", mu_x, "F-statistic IVs", F_first_1)

	outreg2 [iv_2] using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Year of Birth FEs, NO) addstat("F-statistic IVs", F_first_2)

	outreg2 [iv_3] using TABLE_Q_2.xls, excel append nocons keep(Education) addtext(Controls, YES, Year of Birth FEs, YES) addstat("F-statistic IVs", F_first_3)

	
	
*******************************************************
*********************QUESTION 3************************
*******************************************************


************* Point (a)


* We run a simple OLS of 'Healthy' on 'Education', adding the full list of controls defined in the previous question.

	reg Healthy Education `Controls' `Birth_Year_FEs', robust

* We now create a table, including the coefficient for Education only and the usual statistics.

	outreg2 using TABLE_Q_3.xls, replace keep(Education) nocons addstat("Mean y", mu_y, "Mean x", mu_x) cttop("OLS")

	
************* Point (b)


* We run an IV regression of 'Healthy' on 'Education', using 'Quarter1-Quarter3' as instruments and including the full set of controls. Using the 'savefirts' option, we store the estimates of the first stage.

	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', savefirst robust

* We append the estimates to the table, including the first stage coefficients of the quarters only. We add the usual statistics and the F-statistic as well.
 
	outreg2 [_ivreg2_Education] using TABLE_Q_3.xls, append keep(Quarter1 Quarter2 Quarter3) nocons addstat("Mean y", mu_y, "Mean x", mu_x, "F-statistic, IVs", e(widstat))


************* Point (c)

* We run the reduced form regression of 'Healthy' on 'Quarter1-Quarter3', using the full set of controls. We include and 'if' clause to be sure we are computing the regression on the relevant support, that is, all individuals who are not missing data on education. We then append the estimates to the table.

	reg Healthy Quarter1 Quarter2 Quarter3 `Controls' `Birth_Year_FEs' if Education != ., robust

	outreg2 using TABLE_Q_3.xls, append keep(Quarter1 Quarter2 Quarter3) nocons cttop("Reduced form") addstat("Mean y", mu_y, "Mean x", mu_x)

* In point 2.e, we run three iv regressions of the variable "Healthy", using "Quarter1", "Quarter2" and "Quarter3" as instruments for "Education" and progressively expanding the set of controls. What these regressions show is that education has a positive and significant impact on health, even when controlling for date and place of birth and marital status. If we also consider the first stage regression of point 3.b, one can notice that dummies for quarter of birth have a negative and statistically significant impact on education. This is because individuals born earlier in the year receive on average less schooling than others, since they reach the legal dropout age earlier in their education path compared to students born in later quarters. 
*From the reduced-form regression, we see that being born in the first or second quarter of the year has a negative and significant impact on health outcomes, while that of the third quarter is not significant. Indeed, this is consistent with what preoviously observed. Since, as we have estimated, education has a positive impact on health outcomes, while being born in earlier quarter of the year has a negative impact on education, the reduced-form shows that being born in earlier quarters negatively (and significantly) impacts health outcomes and we can conclude that education is a channel for this process.


************* Point (d)

* We run the IV regression, using quarters as instruments and the full set of controls. 

	ivreg2 Healthy (Education = Quarter1 Quarter2 Quarter3) `Controls' `Birth_Year_FEs', robust savefirst

	matrix list e(first)

*We append the estimate of the coefficient for 'Education' to the table.

	outreg2 using TABLE_Q_3.xls, append keep(Education) nocons addstat("Mean y", mu_y, "Mean x", mu_x) cttop("Second stage")


************* Point (e)


* [task] Analizzare se si può dire che lo strumento è debole. (?)
* [task] R^2? ->Bound et al 
* [even though analyzing the R^2 we find a negative value, which however could occur in the regressions for a 2SLS. (da togliere se non siamo sicuri).]
*
*
* The two issues identified in Bound et al.(1995) related to the use of weak instruments are inconsistency and finite sample bias.
* As regards the first issue, if the instrument is weakly correlated with the endogenous variable but also slightly correlated with the error term, this could cause inconsistency of the IV estimator, greater than that of an OLS with endogenous variable.  In our specific context, if the quarters of birth were weak instruments for education level, inconsistency would be an issue if there were other channels through which quarter of birth may influence health other than educational level. As we remarked in part b of question 1, quarter of birth and health status are both correlated with age. This means that a correlation of the instrument with the outcome variable is more than plausible. Moreover, to make some examples, we refer to the literature Bound et. al (1995) cite to criticize Angrist and Krueger (1991), as we think it may be of relevance also in our setting. For example, they report evidence that quarter of birth is related to the likelihood that students show behavioural difficulties, suffer from psychological or mental issues, but also evidence that children from higher-income families are less likely to be born in winter. Thus we think that the factors presented may have impacts on health outcomes. If so, it could be that quarter of birth affects health through these other channels and not only through education. If these effects were strong enough, they may cast doubt on quarter of birth as an instrument for education, and might as well be a cause for inconsistency in the estimation of a model such as the one we are studying.

* The second issue, "finite-sample bias", occurs as the IV is biased in small samples, due to the fact that while executing the second stage regression, the estimates from first stage are used, instead of the true values of first stage parameters, which remain unknown. In this case, we reason on the presence of finite-sample bias analyzing the F-statistic for the first stage of the regression. The high value of the F-statistic (by far greater than the conventional threshold of ten) does not indicate that the instruments are weak. The F-test of significance of the instruments in the first stage rejects the null hypothesis.

* It's worth noting that all IV estimates are still biased in finite samples and that all samples are finite by definition.


************* Point (f)   

* We generate the dummy variables for the states, the interaction between year of birth and quarter of birth and the interaction between state and quarter of birth. In particular, for the former two we use a similar approach to Exercise 2, exploiting the 'generate' option of the 'tab' command. For the latter, we use the consìvenient command 'xi'. 

	tab bpl, generate(state)

	tab birthdate, generate(yearXquarter)

	xi i.bpl*i.birthqtr, noomit prefix(_ii_)

*We now define the sets of controls for the dummies as local variables. 

	local State_FEs "state1-state50" // We omit state51, which is Wyoming

	local Year_Quarter_FEs "yearXquarter1-yearXquarter39" // We omit the dummy for the fourth quarter of 1939, yearXquarter40

	local State_Quarter_FEs "_ii_bplXbir_1_1-_ii_bplXbir_56_3" // We omit _ii_bplXbir_56_4, which corresponds to being from Wyoming and being born in the fourth quarter 


************* Point (g)


* We run an IV regression of 'Healthy' on 'Education', using 'Year_Quarter_FEs' as instrument, including also the full set of controls. For later use, we store the F-statistic of the excluded variables as a scalar.

	ivreg2 Healthy (Education = `Year_Quarter_FEs' ) `Controls' `Birth_Year_FEs', robust
	scalar F_stat_1 = e(widstat)

* We run an IV regression of 'Healthy' on 'Education', using 'State_Quarter_FEs' as instrument, including also the full set of controls. For later use, we store the F-statistic of the excluded variables as a scalar.

	ivreg2 Healthy (Education = `State_Quarter_FEs' ) `Controls' `Birth_Year_FEs' `State_FEs', robust //Wyoming is omitted from State_FEs and used as omitted category
	scalar F_stat_2 = e(widstat)


************* Point (h)


* We display the values of the F-statistics computed in the previous point. 

	scalar list F_stat_1 F_stat_2

* Both F-statistics are smaller than ten, the conventional critical level for testing the relevance of the instruments. Hence, the instruments used are weak. This will very likely entail severe bias in smaller samples. This issue would have to be considered for both regressions.

*[task] Discuss instrument weakness more perhaps 

