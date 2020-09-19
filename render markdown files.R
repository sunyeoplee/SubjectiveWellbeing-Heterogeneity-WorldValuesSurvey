setwd('C:\\Users\\USER\\Desktop\\Backup\\Harvard\\1_Harvard School Works\\Research\\Vulnerability-variability Project\\southafrica\\southafrica_heterogeneity\\variance-by-CTQ_Total')

# render r markdown files
rmarkdown::render(input = "mlwin-life_satisfaction-gdp.Rmd", clean = FALSE)
rmarkdown::render(input = "mlwin-life_satisfaction-gini.Rmd", clean = FALSE)

rmarkdown::render(input = "primary-analysis.Rmd", clean = FALSE)

# rmarkdown::render(input = "region_gini.Rmd", clean = FALSE)
# rmarkdown::render(input = "region_gdp.Rmd", clean = FALSE)
# 
# 
# rmarkdown::render(input = "mlwin-health-gini.Rmd", clean = FALSE)
# rmarkdown::render(input = "mlwin-happiness-gdp.Rmd", clean = FALSE)
# 
# rmarkdown::render(input = "outliers-removed.Rmd", clean = FALSE)


save.image(file=".RData")


detach_package <- function(pkg, character.only = FALSE)
{
  if(!character.only)
  {
    pkg <- deparse(substitute(pkg))
  }
  search_item <- paste("package", pkg, sep = ":")
  while(search_item %in% search())
  {
    detach(search_item, unload = TRUE, character.only = TRUE)
  }
}
detach_package(R2MLwiN)


# scratch

write.table(x = mywvs6, file = "mywvs6_4.csv",quote=F,row.names = F,col.names = T)
install.packages("C:/Users/USER/Desktop/Backup/R-4.0.0/R2MLwiN_0.8-7.tar.gz", repos = NULL, type = "source")
