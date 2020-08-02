
# Cox Proportional-Hazards Model
# ---------------------------------------------------------------

library(survminer)
library(survival)

# R function to compute the Cox model: coxph()
# The function coxph()[in survival package] can be used to compute
# the Cox proportional hazards regression model in R.

# We'll use the lung cancer data in the survival R package.

data("lung")
head(lung)

# inst: Institution code
# time: Survival time in days
# status: censoring status 1=censored, 2=dead
# age: Age in years
# sex: Male=1 Female=2
# ph.ecog: ECOG performance score (0=good 5=dead)
# ph.karno: Karnofsky performance score (bad=0-good=100) rated by physician
# pat.karno: Karnofsky performance score as rated by patient
# meal.cal: Calories consumed at meals
# wt.loss: Weight loss in last six months


# Univariate Cox regression
# Univariate Cox analyses can be computed as follow:
res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox

summary(res.cox)


 
# The Cox regression results can be interpreted as follow:
#   Statistical significance. The column marked "z" gives the 
# Wald statistic value.It corresponds to the ratio of each 
# regression coefficient to its standard error (z = coef/se(coef)).
# The wald statistic evaluates, whether the beta (??) coefficient 
# of a given variable is statistically significantly different 
# from 0. From the output above, we can conclude that the variable
# sex have highly statistically significant coefficients.
# The regression coefficients. The second feature to note in 
# the Cox model results is the the sign of the regression 
# coefficients (coef). A positive sign means that the hazard 
# (risk of death) is higher, and thus the prognosis worse, for
# subjects with higher values of that variable. The variable sex 
# is encoded as a numeric vector. 1: male, 2: female. The R summary 
# for the Cox model gives the hazard ratio (HR) for the second group
# relative to the first group, that is, female versus male. The beta
# coefficient for sex = -0.53 indicates that females have lower risk 
# of death (lower survival rates) than males, in these data.
# 
# Hazard ratios. The exponentiated coefficients (exp(coef) = exp(-0.53) = 0.59), also known as hazard ratios, give the effect size of covariates. For example, being female (sex=2) reduces the hazard by a factor of 0.59, or 41%. Being female is associated with good prognostic.
# 
# Confidence intervals of the hazard ratios. The summary output also gives upper and lower 95% confidence intervals for the hazard ratio (exp(coef)), lower 95% bound = 0.4237, upper 95% bound = 0.816.
# 
# Global statistical significance of the model. Finally, the output gives p-values for three alternative tests for overall significance of the model: The likelihood-ratio test, Wald test, and score logrank statistics. These three methods are asymptotically equivalent. For large enough N, they will give similar results. For small N, they may differ somewhat. The Likelihood ratio test has better behavior for small sample sizes, so it is generally preferred.
# 
# To apply the univariate coxph function to multiple covariates at once, type this:
# 



covariates <- c("age", "sex",  "ph.karno", "ph.ecog", "wt.loss")
univ_formulas <- sapply(covariates,
      function(x) as.formula(paste('Surv(time, status)~', x)))

univ_models <- lapply( univ_formulas, function(x){coxph(x, data = lung)})

# Extract data 
univ_results <- lapply(univ_models,
     function(x){
      x <- summary(x)
      p.value<-signif(x$wald["pvalue"], digits=2)
      wald.test<-signif(x$wald["test"], digits=2)
      beta<-signif(x$coef[1], digits=2);#coeficient beta
      HR <-signif(x$coef[2], digits=2);#exp(beta)
      HR.confint.lower <- signif(x$conf.int[,"lower .95"], 2)
      HR.confint.upper <- signif(x$conf.int[,"upper .95"],2)
      HR <- paste0(HR, " (",HR.confint.lower, "-", HR.confint.upper, ")")
      res<-c(beta, HR, wald.test, p.value)
      names(res)<-c("beta", "HR (95% CI for HR)", "wald.test","p.value")
      return(res) #return(exp(cbind(coef(x),confint(x))))
      })
res <- t(as.data.frame(univ_results, check.names = FALSE))
summary(res)




# The p-value for all three overall tests (likelihood, Wald, and score) are
# significant, indicating that the model is significant. These tests
# evaluate the omnibus null hypothesis that all of the betas (??) are 0.
# In the above example, the test statistics are in close agreement, and
# the omnibus null hypothesis is soundly rejected.
# 
# In the multivariate Cox analysis, the covariates sex and ph.ecog remain 
# significant (p < 0.05). However, the covariate age fails to be significant
# (p = 0.23, which is grater than 0.05).

# The p-value for sex is 0.000986, with a hazard ratio HR = exp(coef) = 0.58,
# indicating a strong relationship between the patients' sex and decreased
# risk of death. The hazard ratios of covariates are interpretable as
# multiplicative effects on the hazard. For example, holding the other 
# covariates constant, being female (sex=2) reduces the hazard by a factor
# of 0.58, or 42%. We conclude that, being female is associated with good 
# prognostic.
# 
# Similarly, the p-value for ph.ecog is 4.45e-05, with a hazard ratio
# HR = 1.59, indicating a strong relationship between the ph.ecog value
# and increased risk of death. Holding the other covariates constant, 
# a higher value of ph.ecog is associated with a poor survival.
# 
# By contrast, the p-value for age is now p=0.23. The hazard ratio
# HR = exp(coef) = 1.01, with a 95% confidence interval of 0.99 to 1.03.
# Because the confidence interval for HR includes 1, these results
# indicate that age makes a smaller contribution to the difference 
# in the HR after adjusting for the ph.ecog values and patient's sex,
# and only trend toward significance. For example, holding the other
# covariates constant, an additional year of age induce daily hazard
# of death by a factor of exp(beta) = 1.01, or 1%, which is not a
# significant contribution.


# Visualizing the estimated distribution of survival times
# Having fit a Cox model to the data, it's possible to visualize the
# predicted survival proportion at any given point in time for a 
# particular risk group. The function survfit() estimates the
# survival proportion, by default at the mean values of covariates.

# Plot the baseline survival function
res.cox

ggsurvplot(survfit(res.cox),data=res,color = "#2E9FDF",
           ggtheme = theme_minimal())


# Create the new data  
sex_df <- with(lung,data.frame(sex = c(1, 2), 
        age = rep(mean(age, na.rm = TRUE), 2),
                  ph.ecog = c(1, 1)))
sex_df


# Survival curves
fit <- survfit(res.cox, newdata = sex_df)
fit
ggsurvplot(fit,data = sex_df, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal())


















