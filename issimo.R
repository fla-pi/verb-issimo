library(tidyr)
library(dplyr)
library(corrplot)
library(vcd)
library(partykit)
library(htmlTable)
library(magrittr)

issimo1 <- read.csv(file.choose(), sep = ";", encoding = "utf-8")

############################

df_long_withNA <- issimo1 %>%
  pivot_longer(
    cols = c("X1_3sg_pp", "X1_3sg_pres_ind", "X1_3sg_imp", "inf", "ger"),  
    names_to = "VerbForm",         
    values_to = "Intensification"
  )
df_long_withNA <- df_long_withNA %>%
  mutate(SemField= recode(SemField,
                          "verb.cognition" = "mental",
                          "verb.cognitive" = "mental",
                          "verb.emotion" = "mental",
                          "verb.perception" = "mental",
                          "verb.consumption" = "object_related",
                          "verb.creation" = "object_related",
                          "verb.body" = "physical",
                          "verb.contact" = "physical",
                          "verb.social" = "social",
                          "ver.social" = "social",
                          "verb.competition" = "social",
                          "verb.stative" = "stative",
                          "verb.change" = "change",
                          "verb.communication" = "communication",
                          "verb.motion" = "motion",
                          "verb.possession" = "possession"))
df_long_withNA <- df_long_withNA %>%
  mutate(Intensification= recode(Intensification,
                                 "Intensity" = "Intensity/Degree",
                                 "Argument Quantity" = "Quantity"))

df_long_withNA <- df_long_withNA %>%
  mutate(VerbForm = recode(VerbForm,
                           "X1_3sg_pp" = "PastPerfective",
                           "X1_3sg_pres_ind" = "Present",
                           "X1_3sg_imp" = "PastImperfective",
                           "inf" = "Infinitive",
                           "ger" = "Gerund"))
df_long_withNA <- df_long_withNA %>%
  mutate(Aktionsart = recode(Aktionsart2,
                             "stativo" = "State",
                             "attività" = "Activity",
                             "risultativo" = "Resultative",
                             "puntuale telico" = "Achievement",
                             "puntuale atelico" = "Punctual_atelic"))


# Crea la nuova colonna 'status' in base ai valori di 'intensification'
df_long_withNA$Status <- ifelse(df_long_withNA$Intensification == "", "Absent", "Found")

df_byverb <- df_long_withNA %>%
  dplyr::select(verb, Aktionsart, SemField) %>%
  distinct()

# 2. Creazione della colonna 'status' che dipende dal valore di 'Status'
df_byverb <- df_byverb %>%
  dplyr::mutate(
    status = ifelse(verb %in% df_long_withNA$verb[df_long_withNA$Status == "Found"], "Found", "Absent")
  )





##############################

df_clean <- issimo1 %>%
       mutate(across(c("X1_3sg_pp", "X1_3sg_pres_ind", "X1_3sg_imp", "inf", "ger"), ~na_if(., "")))

 
   # Trasformazione in long, escludendo i valori NA
   df_long <- df_clean %>%
         pivot_longer(
               cols = c("X1_3sg_pp", "X1_3sg_pres_ind", "X1_3sg_imp", "inf", "ger"),  
               names_to = "VerbForm",         
               values_to = "Intensification",          
               values_drop_na = TRUE          
           )
   df_long <- df_long %>%
     mutate(SemField= recode(SemField,
                             "verb.cognition" = "mental",
                             "verb.cognitive" = "mental",
                             "verb.emotion" = "mental",
                             "verb.perception" = "mental",
                             "verb.consumption" = "object_related",
                             "verb.creation" = "object_related",
                             "verb.body" = "physical",
                             "verb.contact" = "physical",
                             "verb.social" = "social",
                             "ver.social" = "social",
                             "verb.competition" = "social",
                             "verb.stative" = "stative",
                             "verb.change" = "change",
                             "verb.communication" = "communication",
                             "verb.motion" = "motion",
                             "verb.possession" = "possession"))
   df_long <- df_long %>%
     mutate(Intensification= recode(Intensification,
                             "Intensity" = "Intensity/Degree",
                             "Argument Quantity" = "Quantity"))
   
   df_long <- df_long %>%
     mutate(VerbForm = recode(VerbForm,
                              "X1_3sg_pp" = "PastPerfective",
                              "X1_3sg_pres_ind" = "Present",
                              "X1_3sg_imp" = "PastImperfective",
                              "inf" = "Infinitive",
                              "ger" = "Gerund"))
   df_long <- df_long %>%
     mutate(Aktionsart = recode(Aktionsart2,
                                   "stativo" = "State",
                                   "attività" = "Activity",
                                   "risultativo" = "Resultative",
                                   "puntuale telico" = "Achievement",
                                   "puntuale atelico" = "Punctual_atelic"))
     # Visualizza il risultato
     print(df_long)
     
     length(df_long$verb)
     length(levels(factor(df_long$verb)))
     
     df_long_2 <- df_long %>%
            mutate(across(c("Intensification"), ~na_if(., "Modal/sentence intensification")))
     df_long_2_clean <-na.omit(df_long_2)
     View(df_long)     

     length(df_long_2_clean$verb)
     length(levels(factor(df_long_2_clean$verb)))
     
     ch <- chisq.test(df_long_2_clean$Aktionsart, df_long_2_clean$Intensification, simulate.p.value = T, B = 1000)$stdres
     corrplot(ch, is.cor = FALSE)
     
     issimtree<- ctree(factor(Intensification) ~ factor(Aktionsart) + factor(SemField) + factor(VerbForm), data = df_long_2_clean)

     ch2 <- chisq.test(df_long$VerbForm, df_long$Aktionsart, simulate.p.value = T, B = 1000)$stdres
     corrplot(ch2, is.cor = FALSE)
     
     