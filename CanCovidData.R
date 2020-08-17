# From:
# https://doodles.mountainmath.ca/blog/2020/05/27/on-mixing-covid-19-and-census-data/

# See Full code: https://github.com/mountainMath/doodles/blob/master/content/posts/2020-05-27-on-mixing-covid-19-and-census-data.Rmarkdown

# remotes::install_github("mountainmath/CanCovidData")
library(CanCovidData)



if (T) { 
  options(scipen=999); #remove scientific notation
  library(data.table)
  options(datatable.print.class=TRUE)
  library(magrittr)
  library(dtplyr)
  library(dplyr)
  library(lubridate)
  library(ggplot2)
  # library(plotly)
  # library(DT)
}




covid_data <- get_canada_covid_working_group_health_region_data() %>%
  filter(!is.na(HR_UID)) %>%
  filter(Date<=as.Date("2020-05-05")) %>%
  group_by(GeoUID=HR_UID) %>%
  summarise(Confirmed=last(Confirmed,order_by = Date),
            Deaths=last(Deaths,order_by = Date)) 

members <- c(pop=1,
             age_65p=24,
             lico_at=862,
             immigrants=1142,
             num_income_at=664,
             med_income_at=665,
             npr=1150,
             pop_priv=1323,
             black=1327)
members2 <- set_names(names(members),as.integer(members))

census_data <- get_health_region_census_2016_data() %>%
  mutate(GeoUID=`GEO_CODE (POR)`, Name=GEO_NAME) %>%
  replace_all_health_region_geocodes() %>%
  filter(GEO_LEVEL == 2) %>%
  filter(`Member ID: Profile of Health Regions (2247)` %in% as.integer(members)) %>%
  mutate(label=recode(`Member ID: Profile of Health Regions (2247)`,!!!members2)) %>%
  select(HR_UID=`GEO_CODE (POR)`,GeoUID,Name,label,
         Total=`Dim: Sex (3): Member ID: [1]: Total - Sex`) %>%
  mutate(Total=as.numeric(Total)) %>%
  pivot_wider(names_from = label,values_from = Total) %>%
  mutate(income_at=med_income_at*num_income_at) %>%
  select(-HR_UID) %>%
  group_by(GeoUID,Name) %>%
  summarize_all(sum) %>%
  mutate(med_income_at=income_at/num_income_at) %>%
  mutate(log_med_income_at=log(med_income_at)) %>%
  select(-income_at) %>%
  mutate(share_65p=age_65p/pop,
         share_black=black/pop_priv,
         share_foreign_born=(immigrants+npr)/pop_priv,
         share_lico=lico_at/pop_priv)