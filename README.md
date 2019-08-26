# Sibling Survival Analysis

This is a project undertaken at the Institute for Health Metrics and Evaluation. The original STATA functionality was developed by Chang Park, Alison Levin-Rector, and Julie Rajaratnam. The translation to R was done solely by me. 

More information about methodology is available in [Measuring Adult Mortality Using Sibling Survival: A New Analytical Method and New Results for 44 Countries, 1974–2006](https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1000260).

### Overview

Sibling survival analysis is done for countries with poor vital registry systems. When complete VR data is difficult or impossible to obtain, we conduct sibling history surveys, a type of household survey in which respondents are asked information about their surviving and deceased siblings. This way, we can supplement incomplete VR data for a better picture of adult mortality in these countries. 

### Steps

1. Data Formatting - format survey data for consistency with analysis code

2. Zero Survivor Correction
	- The first correction necessary is correcting for zero-surivorship bias. If all siblings in a household have passed away, no one is available to report about siblings' survival or mortality anymore, leading to under-reporting. We address this bias in the second step. 

3. Data Prep for Analysis

4. Survey Regression
	- Adult mortality is calculated here. We use logistic regression, borrowing strength across survey time periods, to estimate probability of death. 

5. Recall Bias 
	- This is the second correction to address - recall bias refers to the tendency for respondents to mis-report events occuring in the distant past. To correct for this, we estimate the recall bias and adjust our final results using the estimates. 