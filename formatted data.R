library(tidyverse)
library(readr)
tvivienda <- read_csv("formatted-data/tvivienda.csv")


## Create range variables for housing age##-------------------------------------
# In the survey, they ask about the age in years and in ranges,
# the way to homogenize is to convert the age to ranges as well

# Convert to NA in the original data
data_viv$P4_19_2 = ifelse(data_viv$P4_19_2 == 9, NA, 
                          data_viv$P4_19_2)

data_viv$P4_19_1 = ifelse(data_viv$P4_19_1 == 98, NA,
                          ifelse(data_viv$P4_19_1  == 99, NA,
                                 data_viv$P4_19_1))
#------------------------------------------------------------

# Create range variable
age_range = ifelse(data_viv$P4_19_1 < 1, 1,
                   ifelse(data_viv$P4_19_1  >= 1 & data_viv$P4_19_1 <= 5, 2,
                          ifelse(data_viv$P4_19_1  >= 6 & data_viv$P4_19_1 <= 10, 3,
                                 ifelse(data_viv$P4_19_1  >= 11 & data_viv$P4_19_1 <= 20, 4,
                                        ifelse(data_viv$P4_19_1  >= 21 & data_viv$P4_19_1 <= 30, 5,
                                               ifelse(data_viv$P4_19_1  >= 31 & data_viv$P4_19_1 <= 50, 6,
                                                      ifelse(data_viv$P4_19_1 > 50, 7,
                                                             data_viv$P4_19_1)))))))

#------------------------------------------------------------

# Merge the new range variable with the existing one
D = data_viv[, c("P4_19_1", "P4_19_2")]
D = cbind(D, age_range)

library(tidyverse)
D = D %>% 
  rowwise() %>%
  mutate(final_age = sum(c(P4_19_2, age_range), na.rm = TRUE))

# Change zeros to NA   
D$final_age = replace(D$final_age, D$final_age == 0, NA)

table(D$final_age)
sum(is.na(D$final_age))
#-------------------------------------------------------------------

# Add variable to the original data
data_viv = cbind(data_viv, D$final_age)

data_viv = rename(data_viv,
                  age_range_final = `D$final_age`)

#-------------------------------------------------------------------

# Create quartile variable for the number of rooms in the house and sleeping rooms

# Group quartiles for the number of sleeping rooms
quantile(data_viv$P4_10)

q_room <- ifelse(data_viv$P4_10 >= 4, 4,
                 data_viv$P4_10)

data_viv = cbind(data_viv, q_room)


# Group quartiles for the total number of rooms in the household
quantile(data_viv$P4_10A)

q_total_rooms <- ifelse(data_viv$P4_10A <= 3, 1,
                        ifelse(data_viv$P4_10A == 4, 2,
                               ifelse(data_viv$P4_10A == 5, 3,
                                      ifelse(data_viv$P4_10A >= 6, 4,
                                             data_viv$P4_10A))))
data_viv = cbind(data_viv, q_total_rooms)
#----------------------------------------------------------------------------


## Group area of houses ##----------------------------------------------

# Convert to NA in the original data
data_viv$P4_20_1 = ifelse(data_viv$P4_20_1 == 998, NA,
                          ifelse(data_viv$P4_20_1  == 999, NA,
                                 data_viv$P4_20_1))

data_viv$P4_20_2 = ifelse(data_viv$P4_20_2 == 99, NA,
                          data_viv$P4_20_2)

area_range <- ifelse(data_viv$P4_20_1 <= 60, 1,
                     ifelse(data_viv$P4_20_1 > 60 & data_viv$P4_20_1 <= 90, 2,
                            ifelse(data_viv$P4_20_1 > 90 & data_viv$P4_20_1 <= 120, 3,
                                   ifelse(data_viv$P4_20_1 > 120 & data_viv$P4_20_1 <= 160, 4,
                                          ifelse(data_viv$P4_20_1 > 160 & data_viv$P4_20_1 <= 200, 5,
                                                 ifelse(data_viv$P4_20_1 > 200 & data_viv$P4_20_1 <= 250, 6,
                                                        ifelse(data_viv$P4_20_1 > 250 & data_viv$P4_20_1 <= 300, 7,
                                                               ifelse(data_viv$P4_20_1 > 300 & data_viv$P4_20_1 <= 500, 8,
                                                                      ifelse(data_viv$P4_20_1 > 500, 9,
                                                                             data_viv$P4_20_1)))))))))
df = as.data.frame(cbind(data_viv$P4_20_2, area_range))
df = df %>%
  rowwise() %>%
  mutate(area_range_final = sum(c(V1, area_range), na.rm = TRUE))

df$area_range_final = replace(df$area_range_final, df$area_range_final == 0, NA)

data_viv <- cbind(data_viv, df$area_range_final)
#---------------------------------------------------------------------------------------------------------------------------

# Create a dataframe of credits
credits = data_viv %>%
  select(vid, P5_15_01, P5_15_02, P5_15_03, P5_15_04, P5_15_05, P5_15_06)

# Change values to sum the number of credits
# 2 means the answer is no credit
# 1 means the answer is yes credit
credits[credits == 2] <- 0  
credits[credits == 1] <- 1

credits = credits %>%
  rowwise() %>%
  mutate(n_credits = sum(c(P5_15_01, P5_15_02, P5_15_03, P5_15_04, P5_15_05, P5_15_06)))

table(credits$n_credits)

# Add the number of credits per house column
data_viv = cbind(data_viv, credits$n_credits)

# Identify if a house has a credit or not
credits = tvivienda %>%
  select(vid, P5_1, P5_15_01, P5_15_02, P5_15_03, P5_15_04, P5_15_05, P5_15_06)

# Convert to 0 because it means no infonavit credit
credits[credits == 2] <- 0

credits = credits %>%
  rowwise() %>%
  mutate(n_credits = sum(c(P5_15_02, P5_15_03, P5_15_04, P5_15_05, P5_15_06)))

credits$other_credit = ifelse(credits$n_credits > 0, 2,
                              credits$n_credits)

credits = credits %>%
  mutate(total = sum(c(P5_15_01, other_credit))) %>%
  mutate(credit_type = ifelse(P5_15_01 == 1, "INFO",
                              ifelse(P5_15_01 == 0 & total > 1, "OTHER",
                                     ifelse(total == 0, "NO_CREDIT",
                                            total))))

credits$credit_type[is.na(credits$credit_type)] <- "NO_CREDIT"

#-------------------------------------------------------------------------------------------------------------

# Houses that have some subsidy
subsidy = data_viv %>%
  select(vid, P5_15_08, P5_15_09, P5_15_10)

subsidy[subsidy == 2] <- 0
subsidy[subsidy == 1] <- 1

subsidy = subsidy %>%
  rowwise() %>%
  mutate(n_subsidy = sum(c(P5_15_08, P5_15_09)))

data_viv = cbind(data_viv, subsidy$n_subsidy)

#---------------------------------------------------------------------------------------------------------

tsdem$NIV[tsdem$NIV == 99] <- NA

# Average education level per household
education = tsdem %>%
  select(vid, NIV) %>%
  group_by(vid) %>%
  summarise(avg_education = mean(NIV, na.rm = TRUE))

data_viv = c

bind(data_viv, education$avg_education)

write.csv(data_viv, "tvivienda_changes1.csv")

# Index crosses ----------------------------------------------------------------------------------
# PCA Index

library(readr)
library(tidyverse)
PCA <- read_csv("INDICES Categorical PCA/IndicesCATPCA_Estratificados.csv") %>%
  rename(stratum_hab = strata.CATPCAHab...stratumID...,
         stratum_ser = strata.CATPCASer...stratumID...,
         stratum_ubi = strata.CATPCAUb...stratumID...,
         stratum_acc = strata.CATPCAAcc...stratumID...)
PCA$stratum_hab = as.factor(PCA$stratum_hab)
levels(PCA$stratum_hab)
# We want to find the states and houses that are in bad condition in services and habitability
levels(PCA$stratum_hab)

# Houses that are in bad condition in habitability and services
sustainability <- PCA %>%
  select(vid, stratum_hab, stratum_ser) %>%
  filter(stratum_hab %in% c("Low", "Very low"),
         stratum_ser %in% c("Low", "Very low"))

# Read survey file
tvivienda <- read_csv("formatted-data/tvivienda_changes1.csv")
names(tvivienda)

write.csv(tvivienda, "formatted-data/tvivienda_changes.csv")
sustainable_houses = tvivienda %>%
  filter(tvivienda$vid %in% sustainability$vid)

sustainable_houses = sustainable_houses %>%
  group_by(Entities) %>%
  summarise(n = sum(ENT))

# P2 Model ----------------------------------------------------------------------------------------
p2_habitability <- read_csv("formatted-data/p2_habitability.csv") %>%
  rename(stratum_habp2 = strata.DH_2020...stratumID...)
names(p2_services)

p2_services <- read_csv("formatted-data/p2_services.csv") %>%
  rename(stratum_serp2 = strata...stratumID...)

data = cbind(p2_habitability, p2_services)
names(data)
data$vid <- NULL
# Houses that are in bad condition in habitability and services
sustainabilityP2 <- data %>%
  select(vid, stratum_habp2, stratum_serp2) %>%
  filter(stratum_habp2 %in% c("Low", "Very low"),
         stratum_serp2 %in% c("Low", "Very low"))

sustainable_houses_p2 = tvivienda %>%
  filter(tvivienda$vid %in% sustainabilityP2$vid)

sustainable_houses_p2 = sustainable_houses_p2 %>%
  group_by(Entities) %>%
  summarise(n = sum(ENT))

###--------------------------------------------------------------------------------------------------------
tvivienda$Entities <- case_when(
  tvivienda$ENT == 1  ~ "Aguascalientes",
  tvivienda$ENT == 2  ~ "Baja California",
  tvivienda$ENT == 3  ~ "Baja California Sur",
  tvivienda$ENT == 4  ~ "Campeche",
  tvivienda$ENT == 5  ~ "Coahuila",
  tvivienda$ENT == 6  ~ "Colima",
  tvivienda$ENT == 7  ~ "Chiapas",
  tvivienda$ENT == 8  ~ "Chihuahua",
  tvivienda$ENT == 9  ~ "CDMX",
  tvivienda$ENT == 10 ~ "Durango",
  tvivienda$ENT == 11 ~ "Guanajuato",
  tvivienda$ENT == 12 ~ "Guerrero",
  tvivienda$ENT == 13 ~ "Hidalgo",
  tvivienda$ENT == 14 ~ "Jalisco",
  tvivienda$ENT == 15 ~ "Mexico",
  tvivienda$ENT == 16 ~ "Michoacan",
  tvivienda$ENT == 17 ~ "Morelos",
  tvivienda$ENT == 18 ~ "Nayarit",
  tvivienda$ENT == 19 ~ "Nuevo León",
  tvivienda$ENT == 20 ~ "Oaxaca",
  tvivienda$ENT == 21 ~ "Puebla",
  tvivienda$ENT == 22 ~ "Queretaro",
  tvivienda$ENT == 23 ~ "Quintana Roo",
  tvivienda$ENT == 24 ~ "San Luis",
  tvivienda$ENT == 25 ~ "Sinaloa",
  tvivienda$ENT == 26 ~ "Sonora",
  tvivienda$ENT == 27 ~ "Tabasco",
  tvivienda$ENT == 28 ~ "Tamaulipas",
  tvivienda$ENT == 29 ~ "Tlaxcala",
  tvivienda$ENT == 30 ~ "Veracruz",
  tvivienda$ENT == 31 ~ "Yucatan",
  tvivienda$ENT == 32 ~ "Zacatecas",
  TRUE ~ as.character(tvivienda$ENT)
)

#write.csv(tvivienda, "formatted-data/tvivienda_changes.csv")
