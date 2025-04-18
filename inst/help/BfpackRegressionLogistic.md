BFpack (Ordered) Logistic Regression
==========================

The analysis allows to test exploratory hypotheses (e.g., equal vs negative vs positive) and confirmatory hypotheses (with equality and/or order constraints) using Bayes factors and posterior probabilities under commonly used statistical models. For the logistic regression that means one can test hypotheses relating to the regression coefficients within a logistic regression model. Approximate fractional Bayes factors are used for hypothesis testing (Gu et al., 2017). For details, see Mulder et al. (2021).

## Input
### Main Window
- Dependent Variable: Input a variable that is ordinal or nominal. If the is ordinal, an ordered logistic regression is performed, if binary, a binary logistic.
- Covariates: Predictors for the logistic regression model; they can have any scaling, that is, continuous, ordinal, nominal

#### Standard Hypothesis Test
- Hypotheses: Test hypothesis that each regression coefficient (beta) is equal to, smaller than, or larger than 0.
- Prior Weights: Specify how to weigh each hypothesis. The default corresponds to a standard setting when testing a two-sided hypothesis test where the null hypothesis has an equal prior weight as the two-sided alternative hypothesis. Because the two-sided alternative is split to the left side and right side, the default prior weight of the null (H0) is 2, and each prior weight for the left-sided and right-sided hypotheses (H1 and H2, respectively) is 1.

#### Parameters
This box contains the names (labels) of the parameters on which equality/one-sided constraints can be formulated in the ‘manual hypothesis test’ box. The names of the effects depend on the names of the predictors/covariates.

#### Manual Hypothesis Test
- Hypotheses: Specify a manual hypothesis with equality and/or one-sided constraints on the parameters; see the tooltip for more info; Specify the prior weights and do not forget to include each hypothesis via the check box. For the regression this could be something like "var1 > var2" implying that the effect of ‘var1’ is larger than the effect ‘var2’ on the dichotomous outcome variable.
- Use the "+" to add more hypotheses.
- Complement: The complement hypothesis (which covers the range of the parameters that are not covered by the above specified hypotheses); prior weight and include.

### Options
#### Bayes Factor
- Log Scale: Reports the natural logarithm of the Bayes factors.
- Bayes Factor Type: Default is the fractional BF, alternatively choose the adjusted fractional BF

#### Tables
- BFs for standard hypothesis test: Print a table that compares each standard hypothesis with its complement.
- Specification: Print the specification table with different parts of the (Savage-Dickey) Bayes factors.
- Estimates With Uncertainty Interval: Print a table with the point estimates and uncertainty intervals (confidence interval for regression coefficients) for the parameter(s) of interest. 

#### Plots
- Manual Hypothesis Plots: Produces plots depicting the prior and posterior probabilities of the manual hypotheses

#### Additional Options
- Uncertainty interval level
- Standardize continous variables
- Repeatability: Seed

#### Interaction Terms
- Box that displays possible two-way interaction terms (if there is more than one covariate); they are by default included in the analysis

## Output

### Tables
#### Posterior probabilities when testing standard hypotheses
- Posterior probabilities for the standard hypotheses

#### BFs: Standard Hypotheses Table
- BF(0u): Bayes factor of the standard H0 vs the unconstrained hypothesis
- BF(-u): Bayes factor of the standard H- vs the unconstrained hypothesis
- BF(+u): Bayes factor of the standard H+ vs the unconstrained hypothesis
- BF(u0): Bayes factor of the unconstrained hypothesis vs the standard H0
- BF(u-): Bayes factor of the unconstrained hypothesis vs the standard H-
- BF(u+): Bayes factor of the unconstrained hypothesis vs the standard H+

#### Manual Hypotheses Legend
- Denotes the manual hypotheses

#### Evidence Matrix (BFs)
- BF matrix with the hypotheses: If the BF for H1 vs H2 is smaller than 1, evidence is in favor of H2, if it is larger than 1 evidence is in favor of H1. If “Log scale” is checked, the printed BFs are on a natural logarithm scale.

#### Posterior probabilities for the manual hypothesis test
- Print the posterior probability for each hypothesis for the manual hypothesis test.

#### BFs: Manual Hypotheses Table
- Equal-Complex: Quantifies the relative complexity of the equality constraints of a hypothesis (the prior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Complex: Quantifies the relative complexity of the order constraints of a hypothesis (the prior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-Fit: Quantifies the relative fit of the equality constraints of a hypothesis (the posterior density at the equality constraints in the extended Savage Dickey density ratio)
- Order-Fit: Quantifies the relative fit of the order constraints of a hypothesis (the posterior probability of the order constraints in the extended Savage Dickey density ratio)
- Equal-BF: Contains the Bayes factor of the equality constraints against the unconstrained hypothesis
- Order-BF: Contains the Bayes factor of the order constraints against the unconstrained hypothesis
- BF: Contains the Bayes factor of the constrained hypothesis against the unconstrained hypothesis
- Posterior Prob.: Contains the posterior probabilities of the hypotheses

#### Estimates Table
- Mean, median, and CI bounds. For the t-test they are confidence intervals.

### Plots
#### Prior And Posterior Probability 
- Pizza plots for the manual hypotheses

### References

- Gu, X., Mulder, J., & Hoijtink, H. (2018). Approximated adjusted fractional Bayes factors: A general method for testing informative hypotheses. *British Journal of Mathematical and Statistical Psychology, 71(2)*, 229-261. https://doi.org/10.1111/bmsp.12110
- Mulder, J., Williams, D. R., Gu, X., Tomarken, A., Böing-Messing, F., Olsson-Collentine, A., Meijerink, M., Menke, J., Fox, J.-P., Hoijtink, H., Rosseel, Y., Wagenmakers, E.J., and van Lissa, C. (2021). BFpack: Flexible Bayes Factor Testing of Scientific Theories in R. *Journal of Statistical Software, 100*(18), 1-63. https://doi.org/10.18637/jss.v100.i18
