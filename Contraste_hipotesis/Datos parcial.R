library(tidyverse)

#Se cargan los datos
datos_HEI <- read.csv2("/cloud/project/Clases/HEI_cost data variable subset.csv")

BMI_sedentario <- datos_HEI %>%
  select(daycode, BMI, activity_level) %>% 
  dplyr::filter(daycode == 1 & activity_level == 1) %>% 
  select(BMI) %>%
  unlist() %>%
  as.vector()

BMI_activo <- datos_HEI %>%
  select(daycode, BMI, activity_level) %>% 
  dplyr::filter(daycode == 1 & activity_level == 3) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

BMI_fumador <- datos_HEI %>%
  select(daycode, BMI, smoker) %>% 
  dplyr::filter(daycode == 1 & smoker == 1) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

BMI_colesterol <- datos_HEI %>%
  select(daycode, BMI, doc_chol) %>% 
  dplyr::filter(daycode == 1 & doc_chol == 1) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

BMI_obesoantes <- datos_HEI %>%
  select(daycode, BMI, sr_overweight) %>% 
  dplyr::filter(daycode == 1 & sr_overweight == 1) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

BMI_blancos_nohispanos <- datos_HEI %>%
  select(daycode, BMI, nhw) %>% 
  dplyr::filter(daycode == 1 & nhw == 1) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

BMI_negros_nohispanos <- datos_HEI %>%
  select(daycode, BMI, nhb) %>% 
  dplyr::filter(daycode == 1 & nhb == 1) %>% 
  select(BMI)%>%
  unlist() %>%
  as.vector()

