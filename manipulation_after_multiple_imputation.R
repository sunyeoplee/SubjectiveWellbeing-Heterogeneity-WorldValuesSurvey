# package
library(gtools)

# recover country variable that lost its numeric value during multiple imputation and covert it as a factor in each imputed dataset
mi_complete_long <- mice::complete(mi_data, action='long', include=T)
mi_complete_long$country <- as.factor(mywvs6$country)

mi_data_final <- as.mids(mi_complete_long)
mi_complete <- mice::complete(mi_data_final, action="all")

summary(mi_complete[[1]]$country) # check that there is no missing values in each country

# # Load the SWIID
# load("swiid8_2/swiid8_2.rda")
# 
# # match the country names in SWIID to WVS in each imputed dataset
# for (i in 1:length(mi_complete)) {
#   swiid[[i]]$country <- as.factor(swiid[[i]]$country)
#   swiid[[i]]$country <- fct_recode(swiid[[i]]$country, 
#                                    "Palestine" = "Palestinian Territories", "Hong_Kong" = "Hong Kong",
#                                    "South_Korea" = "Korea", "New_Zealand" = "New Zealand", 
#                                    "South_Africa" = "South Africa", "Trinidad_and_Tobago" = "Trinidad and Tobago",
#                                    "United_States" = "United States")
# }
# levels(as.factor(swiid[[1]]$country)) # check that country names are corrected

# # add Gini to each country in each imputed dataset
# for (j in 1:length(mi_complete)) {
#   mi_each <- mi_complete[[j]]
#   swiid_each <- swiid[[j]]
#   
#   for (i in levels(mi_each$country)) {
#     mi_each_subset <- mi_each[mi_each$country==i,]
#     swiid_subset <- swiid_each[swiid_each$country==i,]
#     mi_each_subset$year <- mywvs6$year[mywvs6$country==i]
#     year_list <- unique(as_numeric(swiid_subset$year))
#     giniyear <- unique(as_numeric(mi_each_subset$year))
#     year_location <- which(abs(year_list-giniyear)==min(abs(unique(year_list-giniyear)))) 
#     mi_each$gini[mi_each$country==i] <- mean(swiid_subset$gini_disp[swiid_subset$year==swiid_subset$year[year_location]]) # choose the nearest year or average of equally distanced years
#   }
#   mi_complete[[j]]<- mi_each
# }
# summary(mi_complete[[1]]$gini)

# finalize the imputed datasets
for (i in 1:length(mi_complete)) {
  # create derived variables in each imputed dataset
  mi_each <- mi_complete[[i]]
  
  mi_each$age_c <- mi_each$age-mean(mi_each$age, na.rm=T)
  mi_each$age_c_2 <- mi_each$age_c^2

  mi_each$gdp_c <- mi_each$gdp-mean(mi_each$gdp, na.rm=T)
  mi_each$gdp_lowmid <- as.factor(ifelse(mi_each$country %in% countries_gdp_low |mi_each$country %in% countries_gdp_lmid , 1, 0)) # combine the low and lower middle categories because the low income category only have two countries.
  mi_each$gdp_umid <- as.factor(ifelse(mi_each$country %in% countries_gdp_umid, 1, 0))
  mi_each$gdp_high <- as.factor(ifelse(mi_each$country %in% countries_gdp_high, 1, 0))
  
  mi_each$gni_c <- mi_each$gni-mean(mi_each$gni, na.rm=T)
  mi_each$gni_1st <- as.factor(ifelse(mi_each$gni>=0.156 & mi_each$gni<=1.05, 1, 0))
  mi_each$gni_2nd <- as.factor(ifelse(mi_each$gni>1.05 & mi_each$gni<=2.19, 1, 0))
  mi_each$gni_3rd <- as.factor(ifelse(mi_each$gni>2.19 & mi_each$gni<=10.9, 1, 0))
  
  mi_each$gini_c <- mi_each$gini-mean(mi_each$gini, na.rm=T)
  mi_each$gini_1st <- as.factor(ifelse(mi_each$country %in% countries_gini_1st, 1, 0))
  mi_each$gini_2nd <- as.factor(ifelse(mi_each$country %in% countries_gini_2nd, 1, 0))
  mi_each$gini_3rd <- as.factor(ifelse(mi_each$country %in% countries_gini_3rd, 1, 0))
  
  # recover data types for each variable
  main_cont_var <- c('weight','life_satisfaction','age','gdp','gni','gini','age_c','age_c_2',
                     'gdp_c','gni_c','gini_c')
  main_cat_var <- c('income', 'sex', 'socialclass','education', 'maritalstatus',
                    'happiness2','health2',
                    'gdp_umid','gdp_high','gdp_lowmid','gni_1st',
                    'gni_2nd','gni_3rd','gini_1st','gini_2nd','gini_3rd')

  mi_each[,main_cont_var] <- lapply(mi_each[,main_cont_var], as.numeric)
  mi_each[,main_cat_var] <- lapply(mi_each[,main_cat_var], as.factor)
  
  mi_complete[[i]]<- mi_each
}

mi_complete_final <- mi_complete


# check variable distributions in all imputed datasets
lapply(mi_complete_final[["2"]], summary)


