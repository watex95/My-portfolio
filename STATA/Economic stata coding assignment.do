
/*************************************************************************************************
ECON 120B, Spring 2020
Stata Assignment 2

Name:
PID:
**************************************************************************************************/



clear all// clear the environment

set more off
sysuse nlsw88 // load the built in dataset nlsw88


//Question 1:  Run multiple regression to study how education experience,job tenure affects women's wage
//NOTE generate the lnwage from wage
gen lnwage=log(wage)*100
regress lnwage grade ttl_exp tenure //run the regression model

// (a). Percentage change in wage when education increases by one year
* Looking at the regression output ttl_exp has a coefficient of 3.0377 thus is the percentage change in wage
 

// (b). Test the null hypothesis that beta2 = 3 check the t-statistic & p-value  of coefficient of ttl_expi and compare it 
//with critical t value where significance = 10% is 1.645, t_calculated =10.92, 

*beta2 is the coefficient of ttl_exp (3.0377), (10.99 > 1.645) thus reject null hypothesis and conclude that beta2
*is not equal to 3


// Study if the education has a quadratic effect on lnwage 

// (c).What is the value of beta1 and the confidence interval: run a regression model with  additional new variable: grade_squared
gen grade_sq=grade^2 //generate new variable

regress lnwage grade ttl_exp tenure grade_sq //beta1=3.77 confidence interval(-1.489,9.029),


// (d). What is the value of beta4 and to check its statistical significance against a 10% t-test
// beta4=0.1624 C.I(-0.0347,0.3595) , checking the t-tables critical value=1.645, 
//(t_calculated = 0.62 < 1.645) thus beta4 is not significance


//(e). Inject 12 for education variable in the regression model and get the value of lnwage,
// then get the percentage change between 12 and 13 for education.

gen var_12=12*(_b[ttl_exp]) //this is the lnwage at 12 years of education
gen var_13=13*(_b[ttl_exp]) //this is the lnwage at 13 years of education

display (var_13-var_12)/(var_12)*100  //then display the percentage change between the two



// (f). Restricted regression, the  number of restrictions equals to 2, because there are two equal signs: b1=b4=0
//Bonferroni statistic can be reported from a onwway anova test
oneway grade grade_sq, bonferroni

 
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////// 
 
 // Question 2. Run multiple regression with binnary and categorical variables
 // (a). frequencies of occupation
 tabulate occupation
 
 // (b). Introduce the nolabel option to show the numeric values corresponding to different categories of occupation
 tabulate occupation,nolabel
 
 // (c). Summarize wage based on occupation
 summarize wage if occupation==3
 
 // (d) Introduce i to run a regression with binary variables for every occupation category
 regress wage i.occupation
//The mean of wage for both part c and d are the same
 
 
// (e). Managers/Admin had the highest mean of 10.8998
 
 
// (f). Which occupution works the longest hours per week
regress hours i.occupation // summing up the constant with each coefficient gives the average hours, the one with highest is Managers/admin

 
 
 // (g). What is the average wage for white women

regress wage ib("other").race  //this will specify that other is the base category hence white & black category values shown are compared to other race

// (h). What is the wage gap between black and white, what is the 95% confidence interval for the wage gap
//  The wage gap between black and white races is 

gen white_wg = _b[_cons]-0.4677 // generate a variable for white wage
gen black_wg = _b[_cons]-1.70622  // generate a variable for black wage

display (white_wg-black_wg) // this is the wagegap between white and black 

 
 // (i). Generate 3 dummy binnary variables  from race and then run regression of wage and the three dummy variables
 tabulate race, generate(R) //generate the dummies
 //rename the dumiies to white, black and other respectively
 rename R1 white 
 rename R2 black
 rename R3 other
 regress wage black white other //run the regression 
 
 
 /////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
 
 //Question 3. Createing an interraction term then running a regression
 gen collgrad_union=collgrad*union
 regress wage union collgrad collgrad_union
 
 //(a) what is the base category in this model
 * The base category For union variable is "union" as for collgrad is "college graduate"
 // What is the average wage in the base category
 display 6.36869-1.098846+3.748679+1.454899
 
 // (b). Difference in average wage for non-college geaduates in a union and non-college graduates not in a union
 gen wage_union=6.36869+1.454899
 gen wage_notunion=6.36869
 display wage_union-wage_notunion
 * the difference was 1.4548988
 
// (c).Difference in average wage for college geaduates in a union and college graduates not in a union
gen wage2_union=6.36869+1.454899+3.748679-1.098846
gen wage2_notunion=6.36869+3.748679
display wage2_union-wage_notunion
 *the difference was 4.104732
 
 
 
 ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
 // Question 4. Regression with an interaction term between ttl_exp and union
 gen ttl_exp_union=ttl_exp*union  // generate the interaction term 
 regress lnwage union ttl_exp ttl_exp_union //run the regression 
 
 
 
 
 
 
 
 