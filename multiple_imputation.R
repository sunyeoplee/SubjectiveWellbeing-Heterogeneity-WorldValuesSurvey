# packages
library(mice)
library(lattice)
library(pan)
library(micemd)
library(miceadds)
library(jomo)
library(broom)
library(broom.mixed)

# create a dataset with variables that need to be included in imputation models
main_var <- c('country', 'id', 'year', 'weight', 'life_satisfaction', 'income', 'sex', 'socialclass',
              'education', 'maritalstatus', "age", 'happiness2','health2', 'gdp','gni','gini')
aux_var <- c('imp_religion', 'imp_family','trust', 'financial_satisfaction', 'meaning_purpose')

missing <- mywvs6[ , c(main_var, aux_var)]

# examine missing data pattern
# print(md.pattern(missing))

# check dependence of missingness of variables
# histogram(~ life_satisfaction | is.na(meaning_purpose), data=missing)

# multiple imputations 
missing[,c(main_var, aux_var)] <- lapply(missing[,c(main_var, aux_var)], as_numeric) # need to be numerical for pmm
missing$country <- as.integer(wvs6$V2) # convert cluster variables to integer
## specify fixed effects, random effects, and cluster variables
pm <- quickpred(missing) # pick variables with correlations at least 0.1
pm[,"country"] <- -2 # country as a cluster variable
pm[,c("id", "weight")] <- 0 # exclude variables from the imputation model
pm[,c("life_satisfaction","income","sex","socialclass","education","maritalstatus",
      "age","happiness2","health2",'gdp','gni','gini')] <- 1 # include all variables in the main model
diag(pm) <- 0 # exclude the variable being imputed from its imputation model
## run multiple imputations
mi_data <- mice(missing, m=20, maxit=5, predictorMatrix=pm, meth="2l.pmm", seed=0604) # two stage predictive mean matching

# check variable distributions
summary(mywvs6$income) # original dataset
a <- with(mi_data, summary(as_factor(income))) # imputed datasets
a
# percent missing
a$nmis/nrow(mywvs6)
