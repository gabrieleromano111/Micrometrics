**************************PRELIMINARY OPERATIONS********************************
*
*
clear all
*
*********************************Exercise 1*************************************
*
*** We import the relevant dataset
sysuse jtrain2
*
***************** Point 1.a
*
*** We build the balance table usign the 'balancetable' command. This command is particularly useful, as it allows to display directy on table the level of significance of the difference in means as represented by a "*". In particular "***", "**" and "*" stand for 0.01, 0.05, 0.10 level of significance respectively. 
balancetable train age educ black hisp nodegree re74 re75 using "TABLE_1.xls", ctitles("Untreated Mean (Standard Error)" "Treated Mean (Standard Error)" "Difference in Means (Standard Error)") leftctitle("Exercise 1.a") cell(A1) sheet(TABLE_1) modify 
*
*
**** Note that the above may be done also by means of a loop, as explained in TA 2. Below is the code that might be used to perform a similar exercise.
*
**** We create the column of the table that displays on each row the variable we are checking the balance of. 
*
*			gen Variables=""       
**
**
**** We create the other columns of the table that will contain the statistics we are interested in, together with 'N_treated' and 'N_control', which we will use in the computations and 'treatment' and 'control', which we exploit to compute the statistics on the two groups.  
*
* 			foreach x of newlist mean_treated mean_control sd_treated sd_control diff_means se_diff_means treatment control {
*		
*				gen `x'=.
*			
*			}
**
**
**** Group all the statistics we will need to add to the table under a single label
*
*			global table_statistics_1 "mean_treated mean_control sd_treated sd_control diff_means se_diff_means"
**
**
**** 	Define the index i, which allows to move across the 
*
*			local i=1
**
**
**** 	Start the loop
*
*	foreach x of varlist age educ black hisp nodegree re74 re75 {
*
*					numlist "1/7"												
*
*	local myvar: word `i' of `r(numlist)'										// We match to each variable of the model a row in the table.
*	
*	replace Variables="`x'" if _n==`myvar'										// We assign to each row of the 'Variables' column the corresponding 	
*																				// variable name.
*	
*	replace treatment = `x' if train==1											// We use 'treatment' as a "box" where we store the variable's data 
*																				// concerning the treatment group (i.e. of treated units only).
*																				
*	qui summarize treatment, d													// We produce the statistics. 												
*
*	scalar N_treated=r(N) 														// We store in the corresponding row the statistic computed on the variable.
*	
*	replace mean_treated=r(mean) if _n==`myvar' 								
*	
*	replace sd_treated=r(sd) if _n==`myvar' 							
*
*	
*	replace control = `x' if train==0											// We repeat the same procedure for the control group.
*																				
*	qui summarize control, d												
*
*	scalar N_control=r(N)  								
*	replace mean_control=r(mean) if _n==`myvar' 											
*	replace sd_control=r(sd) if _n==`myvar' 									
*	
*	replace diff_means = mean_treated - mean_control if _n==`myvar'				// We store the data concerning the difference in means.
*  	
*	replace se_diff_means=sqrt((sd_treated^2/N_treated )+(sd_control^2/N_control) ) if _n==`myvar'		// We compute the standard errors.
*
*	foreach x of global table_statistics_1 { 									// We round the statistics computed.
*
*		replace `x'=round(`x', 0.01)
*
*	}
*	
*	local i=`i'+1 																// We move on to the next line of the table.
*	
*}
**
**** Now we create the file with the table.
**
*			outsheet Variables $table_statistics_1 using TABLE_1.xls, replace
**
**
**** 	We test the null hypothesis that there is no difference in the means of the covariates across the two groups. Then, we display the p-value for the significant (<0.1) differences.
*
*			foreach x of varlist age educ black hisp nodegree re74 re75 {
*		
*				qui ttest `x', by(train) unequal
*				scalar pvalue_`x'=r(p)
*				
*					if pvalue_`x'<0.1 {
*					
*						scalar list pvalue_`x' 
*					
*					}
*		
*			}
**
**
****** Comment on the Table:
*There are no significant differences between treatment and control group in terms of age, education, proportion of black people and real earnings in 1974 and 1975. There is a small difference (of around 1.67 standard errors) in the proportion of hispanic people, with the treatment group having proportionally less hispanic people. In addition, the control group has a significantly higher proportion (by 3.25 standard errors) of people not holding a degree. Hence, on average, individuals in the treatment group have a significantly higher level of education. One would not expect such difference, as treatment was randomly allocated to units. In addition, such difference may be crucial, as a generally higher level of education in the treatment group can likely translate into better job opportunities and, hence, higher future earnings. This may be a source of bias. 
*
*
***************** Point 1.b
*
*** We run the regression.
*
reg re78 train, robust 			
*
*
*** We store the regression coefficient and the relative standard error as scalars, denoted by coeff_reg1 and se_coeff_reg1 respectively.
scalar coeff_reg1 = _b[train]
scalar se_coeff_reg1 = _se[train]
*
*
***** The coefficient is the estimated difference in the average amount of earnings in 1978 (measured in 1978's US$1000) between the treatment and the control group, i.e. it coincides with the difference in sample mean between treatment and control group. Under the hypothesis of null selection bias, it is the estimated ATT. Numerically, it is equal to US$1,794.34.  
*
*
*
***************** Point 1.c
*
*** We first run the regressions and store the estimates and then create the tables. 
*
qui reg re78 train, robust
*
estimates store reg_ex1c_1
*
*
global controls_set_1 "age educ black hisp"
global controls_set_2 "age educ black hisp re74 re75"
*
*
qui reg re78 train $controls_set_1, robust
*
estimates store reg_ex1c_2
*
*
qui reg re78 train $controls_set_2, robust 
*
estimates store reg_ex1c_3
*
*** Since we need to display the sample size for both subgoups, we compute the two scalars 'N_control' and 'N_treated'.
*
qui summarize re78 if train==1																									
scalar N_treated=r(N)
qui summarize re78 if train==0
scalar N_control=r(N)
*
*** We build the table
outreg2 [reg_ex1c_1 reg_ex1c_2 reg_ex1c_3] using TABLE_2.xls, excel replace addstat(Number of Treated Units, N_treated, Number of Control Units, N_control)
*
*
***** Comment on the result: the coefficient assessing the impact of training on earnings in 1978 is stable, since it is not affected much by the introduction of the controls and it remains significant. 
*
*
*
***************** Point 1.d
*
*
*** We run again the last regression of point (c). If we run the code straight away it is not necessary, but we shall do it to be sure. Afterwards we use the command to estimate the dfbeta. 
qui reg re78 train $controls_set_2 
*
dfbeta
*
*
*** We store the data contained in the dfbeta relative to 'train' in a new variable. 
gen influence_train = _dfbeta_1
*
*
*** We now sort the data in ascending order, relative to 'influence_train'.
sort influence_train
*
*
*** We now perform the same regression as in point (c), by removing from the dataset the units having the 3,5 and 10 most extreme 'influence_train' values.
reg re78 train $controls_set_2 if _n>3 & _n<(445-2), robust
*
reg re78 train $controls_set_2 if _n>5 & _n<(445-4), robust
*
reg re78 train $controls_set_2 if _n>10 & _n<(445-9), robust
*
*
*** By removing the outliers, the coefficient associated to the treatment progressively falls and, when removing the 20 most extreme values, it is one standard error smaller compared to the one estimated in point (c). This implies that the coefficient is significant at 0.05 level only. Hence, there are some outliers driving upwards the estimate of the coefficient. Indeed, it can be observed that there are some individuals whose 'influence_train' is very high relatively to the rest of the sample. This can be assessed by noting that the 'influence_train' mean value lies in the 3rd quartile and its skewness is equal to 2.54. 
*
*
*
*
*********************************Exercise 2*************************************
*
*
*
***************** Point 2.a
*
*
*** We import the relevant dataset.
use jtrain3, clear
*
*
*** Here again we show both methods to build the balance table.
balancetable train age educ black hisp re74 re75 using "TABLE_1.xls", ctitles("Untreated Mean (Standard Error)" "Treated Mean (Standard Error)" "Difference in Means (Standard Error)") leftctitle("Exercise 2.a")cell(E1) sheet(TABLE_1) modify 
*
*
*** We leave as a comment the alternative procedure.
*
**** We define the new statistics we need and group the ones we will intoduce in the table under the same label 
*		gen Variables=""       
**
**  
*		foreach x of newlist mean_treated mean_control sd_treated sd_control diff_means se_diff_means treatment control {
*		
*				gen `x'=.
*			
*		}
**
*		global table_statistics_2 "mean_treated mean_control sd_treated sd_control diff_means se_diff_means"
**
**
**** We perform the loop in a similar manner as in Exercise 1
**
*		local i=1
**
**
**** Start the loop
*		
*		foreach x of varlist age educ black hisp re74 re75 {
*		
*			numlist "1/6"												
*		
*			local myvar: word `i' of `r(numlist)'										
*			
*			replace Variables="`x'" if _n==`myvar'										
*			
*			replace treatment = `x' if train==1									 
*																						
*			qui summarize treatment, d																									
*		
*			scalar N_treated=r(N)  										 
*			replace mean_treated=r(mean) if _n==`myvar' 								
*			replace sd_treated=r(sd) if _n==`myvar' 							
*		
*			replace control = `x' if train==0									
*																						
*			qui summarize control, d												
*		
*			scalar N_control=r(N)  								
*			replace mean_control=r(mean) if _n==`myvar' 											
*			replace sd_control=r(sd) if _n==`myvar' 									
*			
*			replace diff_means = mean_treated - mean_control if _n==`myvar'				
*		  	
*			replace se_diff_means=sqrt(((sd_treated^2)/N_treated) +((sd_control^2)/N_control) ) if _n==`myvar'		
*		
*			foreach x of global table_statistics_2 { 				
*		
*				replace `x'=round(`x', 0.01)
*		
*			}
*			
*			local i=`i'+1 	
*			
*		}
**
**
*		outsheet Variables $table_statistics_2 using TABLE1_1.xls, replace
**
**
**** We perform the t-test to look for the differences.
**
*		foreach x of varlist age educ black hisp re74 re75 {
*		
*			qui ttest `x', by(train) unequal
*			scalar pvalue_`x'=r(p)
*			
*			if pvalue_`x'<0.1 {
*			
*				scalar list pvalue_`x' 
*			
*			}
*		
*		}
*
*** Comment on the table: there are some clear and significant differences between the subgroups in terms of the covariates (as confirmed by the results of the t-test). Hence, we acknowledge that the units are not randomly assigned into the treatment, that is, there seem to be selection into the treatment. Therefore, it is not possible to use the control group as a good proxy for the treated group if its units were not treated. 
*
*
***************** Point 2.b
*
gen treated=.
*
*** We use the same seed as in the question
set seed 88888
*
gen random=uniform()
*
*** Sort the whole dataset based on 'random', i.e. giving a random order.
sort random
*
egen random_order=rank(random)
*
qui sum random
gen N = r(N)
replace treated = 0 if random_order <= (N/2)
replace treated = 1 if random_order > (N/2) & random_order <=(N)
*
***************** Point 2.c
*
*
*** Once 'randtreat' is installed using "ssc install randtreat", we build the new randomly assigned "treatment status" variable.
randtreat, generate(treated_2) replace setseed(88888)
*
*
*** Computing the correlation between 'treated' and 'treated_2' and its significance.
pwcorr treated treated_2, sig
*
*
*** The correlation between treated_2 and treated is not statistically significant. In fact, the p-value of the corr. coefficient is equal to 0.7867. This is what we expected, since the treatment is assigned pseudo-randomly in both cases and using different procedures.
*
*
***************** Point 2.d
*
*** Once again, we indicate both ways to produce the table.
balancetable treated age educ black hisp re74 re75 using "TABLE_1.xls", ctitles("Untreated Mean (Standard Error)" "Treated Mean (Standard Error)" "Difference in Means (Standard Error)") leftctitle("Exercise 2.d") cell(I1) sheet(TABLE_1) modify 
*
*** Coomented below is the alternative version - note that we are assuming we had used this same procedure in Point 2.a.
*
**** We clear all the values in the statistics. This way we are sure that we can safely re-use the same variables.
*
*		foreach x of varlist $table_statistics_2 treatment control {
*		
*				replace `x'=.
*			
*		}
**
**
**** We build the table as we have done before.
*
*		local i=1
**
*		foreach x of varlist age educ black hisp re74 re75 {
*		
*			numlist "1/6"												
*		
*			local myvar: word `i' of `r(numlist)'										
*			
*			replace Variables="`x'" if _n==`myvar'										
*			
*			replace treatment = `x' if treated==1									 
*																						
*			qui summarize treatment, d																									
*		
*			scalar N_treated=r(N) 										 
*			replace mean_treated=r(mean) if _n==`myvar' 								
*			replace sd_treated=r(sd) if _n==`myvar' 							
*		
*			replace control = `x' if treated==0									
*																						
*			qui summarize control, d												
*		
*			scalar N_control=r(N)  								
*			replace mean_control=r(mean) if _n==`myvar' 											
*			replace sd_control=r(sd) if _n==`myvar' 									
*			
*			replace diff_means = mean_treated - mean_control if _n==`myvar'				
*		  	
*			replace se_diff_means=sqrt(((sd_treated^2)/N_treated) +((sd_control^2)/N_control) ) if _n==`myvar'		
*		
*			foreach x of global table_statistics_2 { 					
*		
*				replace `x'=round(`x', 0.01)
*		
*			}
*			
*			local i=`i'+1 	
*			
*		 }
**
**
*		outsheet $table_statistics_2 using TABLE1_2.xls, replace
**
**	
**** We perform here as well a t-test and list the variables which experience significant differences in mean.
*
*		foreach x of varlist age educ black hisp re74 re75 {
*		
*			qui ttest `x', by(treated) unequal
*			
*			scalar pvalue_`x'=r(p)
*		
*				if pvalue_`x'<0.1 {
*				
*					scalar list pvalue_`x' 
*				
*				}
*		}
**
***  Comment on the Table: as expected, there is no significant difference vis-à-vis the covariates between treatment and control group. Indeed, also the t-test provides no evidence aganist the null hypothesis. Hence, the two subgroups are balanced. This is what we expected since the "fake treatment" is randomly assigned to the units.
*
*
***************** Point 2.e
*
*
*** We proceed as in exercise 1c. 
*
qui reg re78 treated, robust
*
estimates store reg_ex2e_1
*
*
global controls_set_1 "age educ black hisp"
global controls_set_2 "age educ black hisp re74 re75"
*
*
qui reg re78 treated $controls_set_1, robust
*
estimates store reg_ex2e_2
*
*
qui reg re78 treated $controls_set_2, robust 
*
estimates store reg_ex2e_3
*
*** Below we define two scalars, which we need to include the output table.
qui summarize re78 if treated==1																									
scalar N_treated=r(N)
qui summarize re78 if treated==0
scalar N_control=r(N)
*
outreg2 [reg_ex2e_1 reg_ex2e_2 reg_ex2e_3] using TABLE_2.xls, excel append addstat(Number of Treated Units, N_treated, Number of Control Units, N_control)
*
*
*
*** Comment on the regression: the treatment has no relevant impact on explaining the real returns in 1978. This is of course due to the fact that we have allocated to the treatment group both untreated and (possibly) treated individuals. Similarly, we have assigned untreated and (possibly) also treated units to the control group. Thus, being in the "fake" treatment group has no actual effects on earnings, since it is just a random selection process. Therefore, this is more of an exercise to assess the determinants of real earnings in 1978 (hence, using the covariates as explanatory variables) than the estimation of a treatment effect.
*
*
*
**********************************Exercise 3************************************* 
*
*
***************** Point 3.a
*
*
* The following instructions are used to perform the regressions
*
qui reg re78 train, robust
estimates store reg_ex3a_1
*
global controls_set_1 "age educ black hisp"
global controls_set_2 "age educ black hisp re74 re75"
*
qui reg re78 train $controls_set_1, robust
estimates store reg_ex3a_2
*
qui reg re78 train $controls_set_2, robust 
estimates store reg_ex3a_3
*
*
outreg2 [reg_ex3a_1 reg_ex3a_2 reg_ex3a_3] using TABLE_2.xls, excel append addstat(Number of Treated Units, N_treated, Number of Control Units, N_control)
*
*** COMMENT: As shown in Table 2 we can notice that in the first two regressions the effect of train on earnings in 1978 is statistically significant while it is not in the third one. As we introduce more covariates from reg_ex3a_1 to reg_ex3a_2 the effect of 'train' decreases in absolute terms but it remains highly significant. Moreover, the new covariates ('age', 'educ' and 'black') are statistically signficant too. Then, from reg_ex3a_2 to reg_ex3a_3 we add two more covariates ('re74' and 're75') which are highly significant. Besides this, the introduction of 're74' and 're75' changes the impact of other covariates, that is 'black' is not signficant while 'hisp' becomes statistically significant. 
*
* Now, if we look at the results obtained in point 1.c, the introduction of new covariates in the regression did not change too much the impact of 'train' on 're78' and also from one regression to another there were no relevant differences in the impact of each single covariate or in their signficance level.
*
* The results obtained in point 1.c are due to the fact that there was a balance between treatment and control as shown in the first balance table which refers to an experimental setting. As we move in a non-experimental setting this is not true anymore, as the balance table in point 2.a underlines. Thus, we expected to obtain these kind of results. Indeed, if the allocation of treatment was random there would be no consequences on the coefficients of training due to the introduction of covariates as point 1.c shows. In conclusion, we might say that, in reg_ex3a_1 and reg_ex3a_2, the effect of train was hiding the impact that covariates have on 're78', which was made explicit in reg_ex3a_3.
*
***************** Point 3.b
*
*** We run the logit regression and predict the propensity score.
logit train age educ black hisp re74 re75  
predict pscore_hat, pr   
*
*** The following lines of code are used to calculate the common support of the propensity score. In particular, we calculate the upper and lower bounds for both the treated and untreated, then we determine the common support taking the greater upper bound and the smallest lower bound.
*
gen pscore_C=pscore_hat if train==0
qui sum pscore_C
scalar lower_bound_C=r(min)
scalar upper_bound_C=r(max)
*
gen pscore_T=pscore_hat if train==1
qui sum pscore_T
scalar lower_bound_T=r(min)
scalar upper_bound_T=r(max)
*
*Then we calculate the bounds for the common support; the lower bound is the highest value between the minimum estimated propensity score across treated and controls; the upper bound is the lowest value between the maximum estimated propensity score across treated and controls.
*
scalar lower_bound=max(lower_bound_C, lower_bound_T)
display lower_bound
scalar upper_bound=min(upper_bound_C, upper_bound_T)
display upper_bound
*
*
***************** Point 3.c
*
*** These four lines of code are just used to calculate the number of treated and untreated
qui sum re78 if train == 0 & pscore_hat >= lower_bound & pscore_hat <= upper_bound
scalar C_size_3c = r(N)
*
qui sum re78 if train == 1 & pscore_hat >= lower_bound & pscore_hat <= upper_bound
scalar T_size_3c = r(N)
*
* The following instructions are used to perform the regressions after restricting the sample, deleting the observations outside the common support.
*
qui reg re78 train if pscore_hat >= lower_bound & pscore_hat <= upper_bound, robust
estimates store reg_ex3c_1
*
global controls_set_1 "age educ black hisp"
global controls_set_2 "age educ black hisp re74 re75"
*
qui reg re78 train $controls_set_1 if pscore_hat >= lower_bound & pscore_hat <= upper_bound, robust
estimates store reg_ex3c_2
*
qui reg re78 train $controls_set_2 if pscore_hat >= lower_bound & pscore_hat <= upper_bound, robust
estimates store reg_ex3c_3
*
outreg2 [reg_ex3c_1 reg_ex3c_2 reg_ex3c_3] using TABLE_2.xls, excel append addstat(Number of Treated Units, T_size_3c, Number of Control Units, C_size_3c)
*
*
*** Comment: In general the results obtained in point 3.a and point 3.c have a similar pattern. In particular, the impact of 'train' is statistically significant in the first two regressions while it is not in the third one. On the other hand, there are some differences in terms of the absolute value of each coefficient in the three regressions compared with the other regression done in point 3.a. However, if we consider the reg_ex3c_3 where all the covariates are included these differences are not so relevant with respect to the reg_ex3a_3. 

* As regards the propensity score, from a theoretical point of view we prefer to consider the common support. This is due to the fact that one should ensure that there is an overlap of the range of propensity scores across treatement and control groups. In fact, it is better to consider the common support to make inferences about treatment effects based on the propensity score because for the observations outside the common support it is harder to find a "counterfactual" in terms of propensity score. 
*
* In this specific case, considering the common support does not produce significant difference in the results since the compositions of the two groups are very different and probably also after the exclusion of the observations outside the common support does not change very much. In order to obtain a suitable "counterfactual", it would be necessary to use some matching techniques. In doing so, we should be careful about choosing the right specification. 
*
*
***************** Point 3.d
*
*
*** We create two histograms plotted in the same graph for the propensity score.
twoway ///
(hist pscore_hat if train==1, fraction lcolor(blue%50) fcolor(blue%50))  ///
(hist pscore_hat if train==0, fraction lcolor(red%50) fcolor(red%50)) , ///
legend(label(1 "pscore for Treatment Group") label(2 "pscore for Control Group")) ///
xlab(.00019513 "Lower Bound" .2 ".2" .4 ".4" .6 ".6".87313724 "Upper Bound") ///
xtitle("Propensity Score") ytitle("Frequency") ///
title("Propensity Score for the Treated and Control Groups") ///
xline(.00019513, lcolor(green) lw(medthick) lp(solid)) ///
xline(.87313724, lcolor(green) lw(medthick) lp(solid)) ///
note("Lower and Upper Bound labels are the extrema of the Common Support.")
*
graph export pset_1_question_3_d.pdf
*
*
*** Comment: the two histograms show that the distribution in terms of propensity score of the treatment group and control group is different. In particular, the distribution of the control group is skewed on the left while the one of the treatment group is just slightly skewed on the right. Indeed, we can notice that the highest number of untreated is characterized by a low level of the propensity score -there are more than 1000 units to the left of the common support- while the opposite holds for the treatment group. 
* 
*
*********************************Exercise 4************************************* 
*
***************** Point 4.a
*
*
*** We estimate the propensity scores, adding squares and an interaction to the initial variables to ensure that, given a propensity score, the treated and control groups are similar in terms of covariates given the propensity score (Following Becker and Ichino)
*
gen educ2     = educ^2
gen re742     = re74^2
gen re752     = re75^2
gen black_u74 = black*unem74
*
global reg_covariates_matching "age agesq educ educ2 married black hisp re74 re75 re742 re752 black_u74"
*
pscore train $reg_covariates_matching, pscore(e_pscore) comsup level(0.005) logit
*
*
***************** Point 4.b
*
*
*** We construct the common support of the p-score distributions among treated and control, defined as the intersection of the supports (ranges) of the two distributions. 
*** This will have lower boundary equal to the largest of the two lower boundaries and upper boundary equal to the smallest of the two upper boundaries. Obviously these will be the treatment lower boundary and the control upper boundary, as we verify.

* Obtain the range for T
qui sum e_pscore if train == 1					
*
scalar low_supT = r(min)						//lower boundary, T
scalar up_supT 	= r(max)						//upper boundary, T
*
* Obtain the range for C
qui sum e_pscore if train == 0					
*
scalar low_supC = r(min)						//lower boundary, C
scalar up_supC 	= r(max)						//upper boundary, C
*
* Calculate the common range 
scalar low_sup	= max(low_supT, low_supC)		//lower bd, common support
scalar up_sup	= min(up_supT, up_supC)			//upper bd, common support
*
scalar list low_sup up_sup
scalar list low_supT up_supC
*
*** The authors find a different common support using the comsup option. It contains ALL treated and the controls lying in the region of common support. One can notice that the support computed with this procedure coincides with the support of the treated.
*
*
***************** Point 4.c
*
*
*
*** We generate dummies for and use the common support previously derived from the definition.
*
gen mycomsup = e_pscore >= low_sup & e_pscore <= up_sup
*
sum e_pscore if train						
*														
scalar mean_t=r(mean)					//mean, treated, all values
*
qui sum e_pscore if !train
*
scalar mean_c=r(mean)					//mean, control, all values
*
qui sum e_pscore if train & mycomsup				
*													
scalar mean_t_cs = r(mean)				//mean, treated, in common supp
*
qui sum e_pscore if !train & mycomsup
*
scalar mean_c_cs = r(mean)				//mean, control, in common supp
*
*** We display the means without the common support restriction.
scalar list mean_t mean_c
*
***We display the means with the common support restriction.
scalar list mean_t_cs mean_c_cs 
*
*
***************** Point 4.d
*
*
*
*** We estimate the ATT using propensity score matching, first with a nearest neighbor and then with a radius procedure.
*
set seed 20295
*
attnd re78 train, pscore(e_pscore) comsup boot reps (100) dots 					//estimating the ATT (nearest neighbor matching)
*
attr  re78 train, pscore(e_pscore) comsup boot reps (100) dots radius(0.05)		//estimating the ATT (radius matching)
*
*
*** Comment: the two methods give substantially different results: with nearest neighbor matching, the estimated ATT is close to the one estimated in the experimental case, when accounting for the full list of controls. However, the power to detect the effect is lacking. In the case of radius matching, the effect is negative and significant. 
*** Remark: Becker & Ichino in their paper use bootstrapping of SE. However, we have seen in class that its use is controversial and, in particular, it has been argued that biased estimates are produced when using it with nearest neighbor matching.
*
***************** Point 4.e
*
*** In general, when we analyze observational data, the self-selection of units into treatment and control is an issue to be adressed. And,in most cases, matching techniques can hardly be regarded as a perfect solution. This is because one would have to control for all the variables that determine the selection, and some may be latent or the knowledge about the selection process may be incomplete. Hence, we can say with confidence that experimental designs should always be preferred. 
*** As we have verified ourselves in point (d), one problem with p-score matching is that results can be very sensitive to the specific matching procedure. Smith and Todd (2005) also show that results can be sensitive to the specification used for calculating the propensity score. As Dehejia (2005) remarks, sensitivity to small changes in the specification is a basic check that should be performed always and specifically before using pscores to estimate treatment effects. 
*** Angrist and Pischke (2008) argue that the use of propensity scores is more justified when the probability of selection into treatment is easy to model and motivate. However, we also agree with their view that the uncertainty that the implementation of pscore matching entails, producing very different results using the same data, makes a simple regression a more practical starting point. 
*
*
*********************************Exercise 5************************************* 
*
*** We clear the workspace.
use jtrain3, clear
*
*
***************** Point 5.a
*
*** We estimate the pscore, limiting ourselves to the selected set of covariates.
psestimate train, totry(age educ black hisp re74 re75 married unem74 unem75)
*
return list
*
*** We save the full specification as a scalar.
scalar vars_pscore = r(h)
*
*** We estimate the ATT using nearest neighbor matching first and radius matching second.
attnd re78 train $vars_pscore, comsup logit  
*
attr re78 train $vars_pscore, comsup logit redius(0.05)
*
*
*
***************** Point 5.b
*
global covariates_matching="age educ black hisp re74 re75"
*
*** We use the command to estimate the pscore and the ATT
psmatch2 train $covariates_matching, out(re78) common
*
*** The ATT is not far from the one computed in point 1 in the experimental setting. However, it is not statistically significant. 
*
*** We check for the balance between treatment and control group
pstest $covariates_matching
*
*** The matcing procedure has produced two pretty comparable samples. 
*
*
***************** Point 5.c
*
*
*** We first identify variables whose propensity score is outside the common support. Then we estimate the ATE and ATT.
capture noisily teffects ipw (re78) (train $covariates_matching, logit), osample(no_match)
*
teffects ipw (re78) (train $covariates_matching, logit) if no_match==0
*
teffects ipw (re78) (train $covariates_matching, logit) if no_match==0, atet
*
*
*** Both the ATE and the ATT are statistically non different from zero. The ATT is numerically close to the ATT previously estimated.
*
*
*** We implement the ipwra estimate of the ATE and ATT. 
teffects ipwra (re78 $covariates_matching, linear) (train $covariates_matching, logit) if no_match==0
*
teffects ipwra (re78 $covariates_matching, linear) (train $covariates_matching, logit) if no_match==0, atet
*
*
*** This model estimates a positive and significant ATT and a negative and significant ATE. 
*
*** ipwra requires more assumptions, as it must be that either the logit model used to estimate the pscore or the linear regression model are correctly specified. 
*
*


