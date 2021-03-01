# This script can be used to compare outcomes of analysis on multicenter studies to meta-analysis on single center studies
rm(list = ls()) # Removes everything in the global environment

# Loading required packages
library(tidyverse)
library(knitr)
library(kableExtra)
library(meta)
library(grid)
library(esc)
library(broom)
library(skimr)

# Change number of decimals that are displayed as default
options("scipen" = 100, "digits"=3)

# Loading dataset
load("~/Documents/GitHub/PulseR/Personal stuff/Lisa H/EMC_set_MHQ.Rdata")

# Remove patients with missing values
EMC_set_MHQ <- EMC_set_MHQ %>%
  select(Patient.traject.ID, Respondent.ID, behandeling, locatie, totaalscore_aangedane_hand_Int, totaalscore_aangedane_hand_3m, score_esthetica_aangedane_hand_Int, score_esthetica_aangedane_hand_3m, primairRecidief, Leeftijd, Geslacht, hoeLangKlacht, zwaarteBeroep) %>% 
  filter(!is.na(totaalscore_aangedane_hand_3m)&!is.na(totaalscore_aangedane_hand_Int)) %>% 
  droplevels()

# Create separate datasets per center and per treatment
Amsterdam <- EMC_set_MHQ %>%
  filter(locatie == "Amsterdam")
Descriptive_stats_Amsterdam <- skim(Amsterdam)
write.table(Descriptive_stats_Amsterdam, file = "Descriptive_stats_Amsterdam.txt", sep = ";", quote = FALSE, row.names = F) # The data from this file can be copy pasted to word and converted into a table
# In word, select the copied data and go to "Table" --> "Convert" --> "Convert text to table"
# Choose ";" under "separate text as"
# This word file can be send to researchers at the coordinating center
Eindhoven <- EMC_set_MHQ %>%
  filter(locatie == "Eindhoven")
skim(Amsterdam)
Rotterdam <- EMC_set_MHQ %>%
  filter(locatie == "Rotterdam")
skim(Amsterdam)

# Create Table 1
variables <- c("Leeftijd", "Geslacht", "hoeLangKlacht", "zwaarteBeroep", "behandeling")
table1 <- tableone::CreateTableOne(vars = variables, strata = "locatie", data = EMC_set_MHQ) # Table 1 for the three different centers
print(table1, nonnormal = "hoeLangKlacht")

table1_data_combined <- tableone::CreateTableOne(vars = variables, data = EMC_set_MHQ) # Table 1 for the three different centers
print(table1_data_combined, nonnormal = "hoeLangKlacht")

# Total MHQ score 3 months postoperatively (multicenter) ------
Data_multicenter <- EMC_set_MHQ %>%
  group_by(behandeling)%>%
  dplyr::summarise(Mean_MHQ_totaal = round(mean(totaalscore_aangedane_hand_3m), 2),
                   SD_MHQ_totaal = round(sd(totaalscore_aangedane_hand_3m), 2),
                   Mean_MHQ_esthetica = round(mean(score_esthetica_aangedane_hand_3m), 2),
                   SD_MHQ_esthetica = round(sd(score_esthetica_aangedane_hand_3m), 2), 
                   N = n())
a2 <- metamean(n = 980, mean = 70.8, sd = 9.81, studlab = "Multicenter study population") # Limited fasciectomy, meta object to create forest plot of combined data
c2 <- metamean(n = 223, mean = 75.3, sd = 7.31, studlab = "Multicenter study population") # Needle aponeurotomy, meta object to create forest plot of combined data
forest(a2, xlim=c(0,100)) # Multicenter data
forest(c2, xlim=c(0,100)) # Multicenter data

# Total MHQ score 3 months postoperatively (single center) ------
Data <- EMC_set_MHQ %>%
  group_by(behandeling, locatie)%>%
  dplyr::summarise(Mean_MHQ_totaal = round(mean(totaalscore_aangedane_hand_3m), 2),
                   SD_MHQ_totaal = round(sd(totaalscore_aangedane_hand_3m), 2),
                   Mean_MHQ_esthetica = round(mean(score_esthetica_aangedane_hand_3m), 2),
                   SD_MHQ_esthetica = round(sd(score_esthetica_aangedane_hand_3m), 2), 
                   N = n())

a <- metamean(n = N, mean = Mean_MHQ_totaal, sd = SD_MHQ_totaal, data = Data[1:3, ], studlab = locatie) # Limited fasciectomy
b <- metamean(n = N, mean = Mean_MHQ_esthetica, sd = SD_MHQ_esthetica, data = Data[1:3, ]) # Limited fasciectomy
c <- metamean(n = N, mean = Mean_MHQ_totaal, sd = SD_MHQ_totaal, data = Data[4:6, ], studlab = locatie) # Needle aponeurotomy
d <- metamean(n = N, mean = Mean_MHQ_esthetica, sd = SD_MHQ_esthetica, data = Data[4:6, ]) # Needle aponeurotomy

# Forest plots of Total MHQ score three months after limited fasciectomy 
forest(a, xlim=c(0,100)) 
grid.text("Total MHQ score after limited fasciectomy", .5, .9, gp=gpar(cex=2)) # Single center data

# Forest plots of Total MHQ score three months after needle aponeurotomy
forest(c, xlim=c(0,100)) 
grid.text("Total MHQ score after needle fasciotomy", .5, .9, gp=gpar(cex=2)) # Single center data


# Comparison of Total MHQ score 3 months postoperatively (multicenter) -------
Data_multicenter
meta_vergelijking_totaal <- metacont(n.e = 980, mean.e = 70.8, sd.e = 9.81, n.c = 223, mean.c = 75.3, sd.c = 7.31, label.e = "LF", label.c = "PNF")
forest(meta_vergelijking_totaal) 

# Comparison of Total MHQ score 3 months postoperatively (single center) -------
# Limited fasciectomy
Data_metacont_LF <- EMC_set_MHQ %>%
  group_by(locatie) %>%
  filter(behandeling == "Partiele fasciëctomie") %>%
  dplyr::summarise(N = n(),
                   Mean_MHQ_totaal = round(mean(totaalscore_aangedane_hand_3m),2),
                   SD_MHQ_totaal = round(sd(totaalscore_aangedane_hand_3m), 2),
                   Mean_MHQ_esthetiek = round(mean(score_esthetica_aangedane_hand_3m),2),
                   SD_MHQ_esthetiek = round(sd(score_esthetica_aangedane_hand_3m), 2), 
                   Leeftijd  = round(mean(Leeftijd), 1))
# Needle aponeurotomy
Data_metacont_PNF <- EMC_set_MHQ %>%
  group_by(locatie) %>%
  filter(behandeling == "Naaldfasciotomie") %>%
  dplyr::summarise(N = n(),
                   Mean_MHQ_totaal = round(mean(totaalscore_aangedane_hand_3m),2),
                   SD_MHQ_totaal = round(sd(totaalscore_aangedane_hand_3m), 2),
                   Mean_MHQ_esthetiek = round(mean(score_esthetica_aangedane_hand_3m),2),
                   SD_MHQ_esthetiek = round(sd(score_esthetica_aangedane_hand_3m), 2))
# Combining treatments
Data_metacont <- inner_join(Data_metacont_LF, Data_metacont_PNF, by="locatie", suffix=c("_LF", "_PNF"))
# Voor MHQ totaalscore op 3m
meta_vergelijking <- metacont(n.e = N_LF, mean.e = Mean_MHQ_totaal_LF, sd.e = SD_MHQ_totaal_LF, n.c = N_PNF, mean.c = Mean_MHQ_totaal_PNF, sd.c = SD_MHQ_totaal_PNF, data = Data_metacont, label.e = "LF", label.c = "PNF", studlab = locatie, comb.random = F)

forest(meta_vergelijking)

# Sample size calculation for incidences ----
# multicenter
# We aim to detect a difference in proportion patients treated for a recurrence between limited fasciectomy and needle aponeurotomy
# We expect that 10% of all needle aponeurotomy patients is treated for a recurrence, compared to 30% of all limited fasciectomy patients 
library(pwr)
pwr.2p.test(h = ES.h(p1 = 0.10, p2 = 0.30), sig.level = 0.05, power = .80) # Sample size calculation
# The sample size calculation indicated that, to detect such a difference, we need at least 59 patients per treatment
pwr.2p2n.test(h = ES.h(p1 = 0.10, p2 = 0.30), n1 = 75, n2 = 225, sig.level = 0.05) # Power calculation


# single center
# n1 and n2 are the average sample sizes in each trial arm we assume across our studies
# We assume that 25% of the patients is treated with needle aponeurotomy, compared to 75% limited fasciectomy
library(dmetar)
p1 <- 0.10
p2 <- 0.30 
OR <- (p2*(1-p1))/(p1*(1-p2)) # Convert proportions to odds ratio
power.analysis(OR = OR, k=3, n1 = 25, n2 = 75, heterogeneity = "low")
# The power calculation indicated that, with the specified difference, 3 locations, and 25 needle aponeurotomy patients and 75 limited fasciectomy patients per location, we have sufficient power (Power: 99.98%)

# Proportion of treatment for recurrences (multicenter) -------
perc_multicenter <- EMC_set_MHQ %>%
  group_by(behandeling) %>%
  dplyr::summarise(N = n(),
                   recidief = sum(primairRecidief == "Recidief"),
                   familie = sum(familie == "Ja")) %>%
  mutate(Perc_recidief = round((recidief/N)*100, 2)) %>%
  mutate(Perc_familie = round((familie/N)*100, 2))

perc_LF <- metaprop(event = 242, n = 980)
perc_PNF <- metaprop(event = 20, n = 223)
forest(perc_LF, xlim=c(0,1))
forest(perc_PNF, xlim=c(0,1))

# Proportion of treatment for recurrences (single center) -------
perc <- EMC_set_MHQ %>%
  group_by(behandeling, locatie) %>%
  dplyr::summarise(N = n(),
                   recidief = sum(primairRecidief == "Recidief"),
                   familie = sum(familie == "Ja")) %>%
  mutate(Perc_recidief = round((recidief/N)*100, 2)) %>%
  mutate(Perc_familie = round((familie/N)*100, 2))

perc_LF_df <- perc[1:3, ] # Limited fasciectomy
perc_PNF_df <- perc[4:6,] # Needle aponeurotomy
perc_LF_meta <- metaprop(event = recidief, n = N, data = perc_LF_df, studlab = locatie) # Limited fasciectomy
perc_PNF_meta <- metaprop(event = recidief, n = N, data = perc_PNF_df, studlab = locatie) # Needle aponeurotomy

forest(perc_LF_meta, xlim=c(0,1)) # Limited fasciectomy
grid.text("Treatment for recurrence after limited fasciectomy", .5, .9, gp=gpar(cex=2))
forest(perc_PNF_meta, xlim=c(0,1)) # Needle aponeurotomy
grid.text("Treatment for recurrence after needle fascioctomy", .5, .9, gp=gpar(cex=2))

# Corrected treatment effects (effect sizes) (multicenter) ------
# Corrected for: Age, gender and baseline Total MHQ score
fit_totaal <- lm(totaalscore_aangedane_hand_3m ~ behandeling + Leeftijd + Geslacht + totaalscore_aangedane_hand_Int, data = EMC_set_MHQ)
summary(fit_totaal)
sd(EMC_set_MHQ$totaalscore_aangedane_hand_3m)
table(EMC_set_MHQ$behandeling)

# b: unstandardized coefficient  of the "treatment" predictor, obtained from summary(fit_totaal)
# sdy: the standard deviation of the dependent variable y (i.e., the outcome), obtained from sd(EMC_set_MHQ$totaalscore_aangedane_hand_3m)
# grp1n: the number of participants in the first group, obtained from table(EMC_set_MHQ$behandeling)
# grp2n: the number of participants in the second group, obtained from table(EMC_set_MHQ$behandeling)
# es.type: the effect measure we want to calculate. In our case this is "g". But we could also calculate Cohen’s d using "d".
esc_B_EMC_set <- as.data.frame(esc_B(b=4.2143, sdy=9.56, grp1n = 980, grp2n = 223, es.type = "g"))
esc_B_EMC_set
effect_size_totaal <- metagen(TE = 0.4469804, se = 0.07475196,  sm = "SMD")
forest(effect_size_totaal)

# Corrected treatment effects (effect sizes) (single center) ------
fit_Amsterdam <- lm(totaalscore_aangedane_hand_3m ~ behandeling + Leeftijd + Geslacht + totaalscore_aangedane_hand_Int, data = Amsterdam)
summary(fit_Amsterdam)
tidy_lm_Amsterdam <- tidy(fit_Amsterdam)%>%
  mutate(Locatie = "Amsterdam")

fit_Eindhoven <- lm(totaalscore_aangedane_hand_3m ~ behandeling + Leeftijd + Geslacht + totaalscore_aangedane_hand_Int, data = Eindhoven)
summary(fit_Eindhoven)
tidy_lm_Eindhoven <- tidy(fit_Eindhoven)%>%
  mutate(Locatie = "Eindhoven")

fit_Rotterdam <- lm(totaalscore_aangedane_hand_3m ~ behandeling + Leeftijd + Geslacht + totaalscore_aangedane_hand_Int, data = Rotterdam)
summary(fit_Rotterdam)
tidy_lm_Rotterdam <- tidy(fit_Rotterdam) %>%
  mutate(Locatie = "Rotterdam")

# Effect sizes
#sd(Amsterdam$totaalscore_aangedane_hand_3m)
#table(Amsterdam$behandeling)
esc_B_Amsterdam <- as.data.frame(esc_B(b=3.5306, sdy=8.69, grp1n = 250, grp2n = 47, es.type = "g"))
esc_B_Amsterdam$study <- "Amsterdam"
#
#sd(Eindhoven$totaalscore_aangedane_hand_3m)
#table(Eindhoven$behandeling)
esc_B_Eindhoven <- as.data.frame(esc_B(b=6.4599, sdy=9.8, grp1n = 136, grp2n = 40, es.type = "g"))
esc_B_Eindhoven$study <- "Eindhoven"

#sd(Rotterdam$totaalscore_aangedane_hand_3m)
#table(Rotterdam$behandeling)
esc_B_Rotterdam <- as.data.frame(esc_B(b=4.0446, sdy=9.76, grp1n = 594, grp2n = 136, es.type = "g"))
esc_B_Rotterdam$study <- "Rotterdam"

data_hedges_g <- rbind(esc_B_Amsterdam, esc_B_Eindhoven)%>%
  rbind(esc_B_Rotterdam)

effect_sizes <- metagen(TE = es, se = se, data = data_hedges_g, sm = "SMD", studlab = study) # 
forest(effect_sizes)


