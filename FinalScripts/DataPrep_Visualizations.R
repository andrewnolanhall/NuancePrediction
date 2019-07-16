### Preparing data for visualizations
### Andrew N Hall, Sandra Matz

#Load packages ------------- 
library(tidyverse)
library(ggrepel)

### Read in data ------------- 
# personality data
pers_dat <- read_csv("pers_dat_test.csv") %>% 
  select(userid, E_comp:C_comp)#%>% 

# lasso results data
lasso_trait <- read_csv("predlassosvd_traits.csv") %>% 
  rename(userid = X1,
         lassotrait_pred_E = predE_comp,
         lassotrait_pred_N = predN_comp,
         lassotrait_pred_O = predO_comp,
         lassotrait_pred_A = predA_comp,
         lassotrait_pred_C = predC_comp)
lasso_q <- read_csv("predlassosvd_q.csv") %>% 
  rename(lassoq_pred_E = pred_E,
         lassoq_pred_N = pred_N,
         lassoq_pred_O = pred_O,
         lassoq_pred_A = pred_A,
         lassoq_pred_C = pred_C) #%>% #select(lassoq_pred_E:lassoq_pred_C)
lasso_combined <- cbind(lasso_trait, lasso_q)
lasso <- lasso_combined
rm(lasso_trait, lasso_q, lasso_combined)

# RF results data
rf_trait <- read_csv("predRFsvd_traits.csv") %>% 
  rename(userid = X1,
         RFtrait_pred_E = predE_comp,
         RFtrait_pred_N = predN_comp,
         RFtrait_pred_O = predO_comp,
         RFtrait_pred_A = predA_comp,
         RFtrait_pred_C = predC_comp)
rf_q <- read_csv("predRFsvd_q.csv") %>% 
  rename(RFq_pred_E = pred_E,
         RFq_pred_N = pred_N,
         RFq_pred_O = pred_O,
         RFq_pred_A = pred_A,
         RFq_pred_C = pred_C) %>% 
  select(RFq_pred_E:RFq_pred_C)
rf_combined <- cbind(rf_trait, rf_q)
rf <- rf_combined
rm(rf_trait, rf_q, rf_combined)

# combine results data with personality data
combined_lasso <- left_join(pers_dat, lasso, by = "userid")
combined_rf <- left_join(pers_dat, rf, by = "userid")

# prediction outcome data 


#### Load prediction data
swl <- read_csv("Outcomes/swl.csv") %>% 
  right_join(combined_lasso, by = "userid") %>% 
  filter(!is.na(swl))

SV  <- read_csv("Outcomes/SChwartz_values_raw.csv") # cleaned below

### Data cleaning for schwartz values  ------------- 

SV <- as.data.frame(SV)
SV1 <- SV[,c(8:64)] # turn 0s and -1s into NA
SV2 <- SV[,c(1:7)]
SV1[SV1 == 0] <- NA
SV1[SV1 == -1] <- NA
SV <- cbind(SV2, SV1)
## calcuate centred questions variables
SV$MRAT <- rowMeans(SV[,c(8:64)], na.rm = T)
SV <- SV[is.na(SV$MRAT) == F,]
head(SV) # get rid of people who haven't responded to any of the questions
SV$numNAs <- rowSums(is.na(SV))
SV <- SV[!(SV$numNAs > 20),] # exclude people who have missed more than 20 questions
# calculate scale means from centred question scores
centre <- function(x) x - SV$MRAT
SV1 <- data.frame(SV[1:7], apply(SV[8:64],2, centre), SV[,65] )
SV1$Conformity <- rowMeans(SV1[,c("q11","q20","q40", "q49")], na.rm = T) # 11,20,40,47
SV1$Tradition <- rowMeans(SV1[,c("q18","q32","q36", "q44", "q51")], na.rm = T) # 18,32,36,44,51
SV1$Benevolence <- rowMeans(SV1[,c("q33","q45","q49", "q52", "q54")], na.rm = T) # 33,45,49,52,54
SV1$Universalism <- rowMeans(SV1[,c("q1","q17","q24", "q26", "q29", "q30", "q35", "q38")], na.rm = T) # 1,17,24,26,29,30,35,38
SV1$Self_Direction <- rowMeans(SV1[,c("q5","q16","q31", "q41", "q53")], na.rm = T) # 5,16,31,41,53
SV1$Stimulation <- rowMeans(SV1[,c("q9","q25","q37")], na.rm = T) # 9,25,37
SV1$Hedonism <- rowMeans(SV1[,c("q4","q50","q57")], na.rm = T) # 4,50,57
SV1$Achievement <- rowMeans(SV1[,c("q34","q39","q43", "q55")], na.rm = T) # 34,39,43,55
SV1$Power <- rowMeans(SV1[,c("q3","q12","q27", "q46")], na.rm = T) # 3,12,27,46
SV1$Security <- rowMeans(SV1[,c("q8","q13","q15", "q22", "q56")], na.rm = T) # 8,13,15,22,56
nrow(SV1) # 9311
head(SV2)
SV2 <- SV1[,c("id", "Conformity", "Tradition", "Benevolence", "Universalism", "Self_Direction", "Stimulation", "Hedonism", "Achievement", "Power", "Security")]
colnames(SV2) <- c("userid", "Conformity", "Tradition", "Benevolence", "Universalism", "Self_Direction", "Stimulation", "Hedonism", "Achievement", "Power", "Security")
SV <- SV2
rm(SV1, SV2)
values <- SV %>% 
  right_join(combined_lasso, by = "userid") %>% 
  filter(!is.na(Conformity)) #only need to select on one var bc all same NAs
rm(SV)


### Outcome correlations  ------------- 
##### Plotting predictions vs outcomes
#Extract correlations
#swl
swl_cors <- matrix(c(cor(swl$swl, swl$E_comp),
                     cor(swl$swl, swl$lassotrait_pred_E),
                     cor(swl$swl, swl$lassoq_pred_E),
                     
                     cor(swl$swl, swl$N_comp), 
                     cor(swl$swl, swl$lassotrait_pred_N),
                     cor(swl$swl, swl$lassoq_pred_N), 
                     
                     cor(swl$swl, swl$C_comp), 
                     cor(swl$swl, swl$lassotrait_pred_C), 
                     cor(swl$swl, swl$lassoq_pred_C),
                     
                     cor(swl$swl, swl$A_comp),
                     cor(swl$swl, swl$lassotrait_pred_A),
                     cor(swl$swl, swl$lassoq_pred_A),
                     
                     cor(swl$swl, swl$O_comp),
                     cor(swl$swl, swl$lassotrait_pred_O),
                     cor(swl$swl, swl$lassoq_pred_O)), 
                   
                   byrow = T, ncol = 3)
colnames(swl_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(swl_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

## Values
# Conformity
conformity_cors <- matrix(c(cor(values$Conformity, values$E_comp),
                            cor(values$Conformity, values$lassotrait_pred_E),
                            cor(values$Conformity, values$lassoq_pred_E),
                            
                            cor(values$Conformity, values$N_comp), 
                            cor(values$Conformity, values$lassotrait_pred_N),
                            cor(values$Conformity, values$lassoq_pred_N),
                            
                            cor(values$Conformity, values$C_comp), 
                            cor(values$Conformity, values$lassotrait_pred_C), 
                            cor(values$Conformity, values$lassoq_pred_C),
                            
                            cor(values$Conformity, values$A_comp),
                            cor(values$Conformity, values$lassotrait_pred_A),
                            cor(values$Conformity, values$lassoq_pred_A),
                            
                            cor(values$Conformity, values$O_comp),
                            cor(values$Conformity, values$lassotrait_pred_O),
                            cor(values$Conformity, values$lassoq_pred_O)), 

                          byrow = T, ncol = 3)
colnames(conformity_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(conformity_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Tradition
tradition_cors <- matrix(c(cor(values$Tradition, values$E_comp),
                           cor(values$Tradition, values$lassotrait_pred_E),
                           cor(values$Tradition, values$lassoq_pred_E),
                           
                           cor(values$Tradition, values$N_comp), 
                           cor(values$Tradition, values$lassotrait_pred_N),
                           cor(values$Tradition, values$lassoq_pred_N), 
                           
                           cor(values$Tradition, values$C_comp), 
                           cor(values$Tradition, values$lassotrait_pred_C), 
                           cor(values$Tradition, values$lassoq_pred_C),
                           
                           cor(values$Tradition, values$A_comp),
                           cor(values$Tradition, values$lassotrait_pred_A),
                           cor(values$Tradition, values$lassoq_pred_A),
                           
                           cor(values$Tradition, values$O_comp),
                           cor(values$Tradition, values$lassotrait_pred_O),
                           cor(values$Tradition, values$lassoq_pred_O)),
                         
                         byrow = T, ncol = 3)
colnames(tradition_cors) c("Self-Report", "Trait", "Nuance")
rownames(tradition_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Benevolence
benevolence_cors <- matrix(c(cor(values$Benevolence, values$E_comp),
                             cor(values$Benevolence, values$lassotrait_pred_E),
                             cor(values$Benevolence, values$lassoq_pred_E),
                             
                             
                             cor(values$Benevolence, values$N_comp), 
                             cor(values$Benevolence, values$lassotrait_pred_N),
                             cor(values$Benevolence, values$lassoq_pred_N), 
                        
                             
                             cor(values$Benevolence, values$C_comp), 
                             cor(values$Benevolence, values$lassotrait_pred_C), 
                             cor(values$Benevolence, values$lassoq_pred_C),
                            
                             
                             cor(values$Benevolence, values$A_comp),
                             cor(values$Benevolence, values$lassotrait_pred_A),
                             cor(values$Benevolence, values$lassoq_pred_A),
                            
                             
                             cor(values$Benevolence, values$O_comp),
                             cor(values$Benevolence, values$lassotrait_pred_O),
                             cor(values$Benevolence, values$lassoq_pred_O)),
                           
                           byrow = T, ncol = 3)
colnames(benevolence_cors) c("Self-Report", "Trait", "Nuance")
rownames(benevolence_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Universalism
universalism_cors <- matrix(c(cor(values$Universalism, values$E_comp),
                              cor(values$Universalism, values$lassotrait_pred_E),
                              cor(values$Universalism, values$lassoq_pred_E),
                              
                              cor(values$Universalism, values$N_comp), 
                              cor(values$Universalism, values$lassotrait_pred_N),
                              cor(values$Universalism, values$lassoq_pred_N), 
                              
                              cor(values$Universalism, values$C_comp), 
                              cor(values$Universalism, values$lassotrait_pred_C), 
                              cor(values$Universalism, values$lassoq_pred_C),
                              
                              cor(values$Universalism, values$A_comp),
                              cor(values$Universalism, values$lassotrait_pred_A),
                              cor(values$Universalism, values$lassoq_pred_A),
                              
                              cor(values$Universalism, values$O_comp),
                              cor(values$Universalism, values$lassotrait_pred_O),
                              cor(values$Universalism, values$lassoq_pred_O)),
                            
                            byrow = T, ncol = 3)
colnames(universalism_cors) c("Self-Report", "Trait", "Nuance")
rownames(universalism_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Self direction
selfdirection_cors <- matrix(c(cor(values$Self_Direction, values$E_comp),
                               cor(values$Self_Direction, values$lassotrait_pred_E),
                               cor(values$Self_Direction, values$lassoq_pred_E),
                               
                               cor(values$Self_Direction, values$N_comp), 
                               cor(values$Self_Direction, values$lassotrait_pred_N),
                               cor(values$Self_Direction, values$lassoq_pred_N),
                               
                               
                               cor(values$Self_Direction, values$C_comp), 
                               cor(values$Self_Direction, values$lassotrait_pred_C), 
                               cor(values$Self_Direction, values$lassoq_pred_C),
                               sqrt(2/(dim(values)[1] - 3)), #SE for diff in cor
                               
                               cor(values$Self_Direction, values$A_comp),
                               cor(values$Self_Direction, values$lassotrait_pred_A),
                               cor(values$Self_Direction, values$lassoq_pred_A),
                               sqrt(2/(dim(values)[1] - 3)), #SE for diff in cor
                               
                               cor(values$Self_Direction, values$O_comp),
                               cor(values$Self_Direction, values$lassotrait_pred_O),
                               cor(values$Self_Direction, values$lassoq_pred_O),
                               sqrt(2/(dim(values)[1] - 3))), #SE for diff in cor
                             
                             byrow = T, ncol = 4)
colnames(selfdirection_cors) <- c("Self-Report", "Trait", "Nuance", "SE for Difference")
rownames(selfdirection_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Stimulation
stimulation_cors <- matrix(c(cor(values$Stimulation, values$E_comp),
                             cor(values$Stimulation, values$lassotrait_pred_E),
                             cor(values$Stimulation, values$lassoq_pred_E),
                             sqrt(2/(dim(values)[1] - 3)), #SE for diff in cor
                             
                             cor(values$Stimulation, values$N_comp), 
                             cor(values$Stimulation, values$lassotrait_pred_N),
                             cor(values$Stimulation, values$lassoq_pred_N), 
                             
                             cor(values$Stimulation, values$C_comp), 
                             cor(values$Stimulation, values$lassotrait_pred_C), 
                             cor(values$Stimulation, values$lassoq_pred_C),
                             
                             cor(values$Stimulation, values$A_comp),
                             cor(values$Stimulation, values$lassotrait_pred_A),
                             cor(values$Stimulation, values$lassoq_pred_A),
                             
                             cor(values$Stimulation, values$O_comp),
                             cor(values$Stimulation, values$lassotrait_pred_O),
                             cor(values$Stimulation, values$lassoq_pred_O)), 
                           
                           byrow = T, ncol = 3)
colnames(stimulation_cors) c("Self-Report", "Trait", "Nuance")
rownames(stimulation_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Hedonism
hedonism_cors <- matrix(c(cor(values$Hedonism, values$E_comp),
                          cor(values$Hedonism, values$lassotrait_pred_E),
                          cor(values$Hedonism, values$lassoq_pred_E),
                          
                          cor(values$Hedonism, values$N_comp), 
                          cor(values$Hedonism, values$lassotrait_pred_N),
                          cor(values$Hedonism, values$lassoq_pred_N), 
                          
                          cor(values$Hedonism, values$C_comp), 
                          cor(values$Hedonism, values$lassotrait_pred_C), 
                          cor(values$Hedonism, values$lassoq_pred_C),
                          
                          cor(values$Hedonism, values$A_comp),
                          cor(values$Hedonism, values$lassotrait_pred_A),
                          cor(values$Hedonism, values$lassoq_pred_A),
                          
                          cor(values$Hedonism, values$O_comp),
                          cor(values$Hedonism, values$lassotrait_pred_O),
                          cor(values$Hedonism, values$lassoq_pred_O)),
                        
                        byrow = T, ncol = 3)
colnames(hedonism_cors) c("Self-Report", "Trait", "Nuance")
rownames(hedonism_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Achievement
achievement_cors <- matrix(c(cor(values$Achievement, values$E_comp),
                             cor(values$Achievement, values$lassotrait_pred_E),
                             cor(values$Achievement, values$lassoq_pred_E),
                             
                             cor(values$Achievement, values$N_comp), 
                             cor(values$Achievement, values$lassotrait_pred_N),
                             cor(values$Achievement, values$lassoq_pred_N), 
                             
                             cor(values$Achievement, values$C_comp), 
                             cor(values$Achievement, values$lassotrait_pred_C), 
                             cor(values$Achievement, values$lassoq_pred_C),
                             
                             cor(values$Achievement, values$A_comp),
                             cor(values$Achievement, values$lassotrait_pred_A),
                             cor(values$Achievement, values$lassoq_pred_A),
                             
                             cor(values$Achievement, values$O_comp),
                             cor(values$Achievement, values$lassotrait_pred_O),
                             cor(values$Achievement, values$lassoq_pred_O)), 
                           
                           byrow = T, ncol = 3)
colnames(achievement_cors) c("Self-Report", "Trait", "Nuance")
rownames(achievement_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Power 
power_cors <- matrix(c(cor(values$Power, values$E_comp),
                       cor(values$Power, values$lassotrait_pred_E),
                       cor(values$Power, values$lassoq_pred_E),
                       
                       cor(values$Power, values$N_comp), 
                       cor(values$Power, values$lassotrait_pred_N),
                       cor(values$Power, values$lassoq_pred_N), 
                       
                       cor(values$Power, values$C_comp), 
                       cor(values$Power, values$lassotrait_pred_C), 
                       cor(values$Power, values$lassoq_pred_C),
                       
                       cor(values$Power, values$A_comp),
                       cor(values$Power, values$lassotrait_pred_A),
                       cor(values$Power, values$lassoq_pred_A),
                       
                       cor(values$Power, values$O_comp),
                       cor(values$Power, values$lassotrait_pred_O),
                       cor(values$Power, values$lassoq_pred_O)), 
                     
                     byrow = T, ncol = 3)
colnames(power_cors) c("Self-Report", "Trait", "Nuance")
rownames(power_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")

# Security
security_cors <- matrix(c(cor(values$Security, values$E_comp),
                          cor(values$Security, values$lassotrait_pred_E),
                          cor(values$Security, values$lassoq_pred_E),
                          
                          cor(values$Security, values$N_comp), 
                          cor(values$Security, values$lassotrait_pred_N),
                          cor(values$Security, values$lassoq_pred_N), 
                          
                          cor(values$Security, values$C_comp), 
                          cor(values$Security, values$lassotrait_pred_C), 
                          cor(values$Security, values$lassoq_pred_C),
                          
                          cor(values$Security, values$A_comp),
                          cor(values$Security, values$lassotrait_pred_A),
                          cor(values$Security, values$lassoq_pred_A),
                          
                          cor(values$Security, values$O_comp),
                          cor(values$Security, values$lassotrait_pred_O),
                          cor(values$Security, values$lassoq_pred_O)), 
                        
                        byrow = T, ncol = 3)
colnames(security_cors) c("Self-Report", "Trait", "Nuance")
rownames(security_cors) <- c("Extraversion", "Neuroticism", "Conscientiousness", "Agreeableness", "Openness")



### E cors dataset 
e_cors <- matrix(c(achievement_cors["Extraversion",],
                   benevolence_cors["Extraversion",], 
                   conformity_cors["Extraversion",], 
                   hedonism_cors["Extraversion",], 
                   power_cors["Extraversion",],
                   security_cors["Extraversion",], 
                   selfdirection_cors["Extraversion",], 
                   stimulation_cors["Extraversion",], 
                   swl_cors["Extraversion",], 
                   tradition_cors["Extraversion",], 
                   universalism_cors["Extraversion",]),
                 byrow = T, ncol = 3)
colnames(e_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(e_cors) <- c("Achievement", "Benevolence", "Conformity", "Hedonism",
                      "Power", "Security", "SelfDirection", "Stimulation", "SWL", "Tradition",
                      "Universalism")
e_cors <- as.data.frame(e_cors)

### N cors dataset 
n_cors <- matrix(c(achievement_cors["Neuroticism",],
                   benevolence_cors["Neuroticism",], 
                   conformity_cors["Neuroticism",], 
                   hedonism_cors["Neuroticism",], 
                   power_cors["Neuroticism",],
                   security_cors["Neuroticism",], 
                   selfdirection_cors["Neuroticism",], 
                   stimulation_cors["Neuroticism",], 
                   swl_cors["Neuroticism",], 
                   tradition_cors["Neuroticism",], 
                   universalism_cors["Neuroticism",]),
                 byrow = T, ncol = 3)
colnames(n_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(n_cors) <- c("Achievement", "Benevolence", "Conformity", "Hedonism",
                      "Power", "Security", "SelfDirection", "Stimulation", "SWL", "Tradition",
                      "Universalism")

n_cors <- as.data.frame(n_cors)

### O cors dataset 
o_cors <- matrix(c(achievement_cors["Openness",],
                   benevolence_cors["Openness",], 
                   conformity_cors["Openness",], 
                   hedonism_cors["Openness",], 
                   power_cors["Openness",],
                   security_cors["Openness",], 
                   selfdirection_cors["Openness",], 
                   stimulation_cors["Openness",], 
                   swl_cors["Openness",], 
                   tradition_cors["Openness",], 
                   universalism_cors["Openness",]),
                 byrow = T, ncol = 3)
colnames(o_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(o_cors) <- c("Achievement", "Benevolence", "Conformity", "Hedonism",
                      "Power", "Security", "SelfDirection", "Stimulation", "SWL", "Tradition",
                      "Universalism")
o_cors <- as.data.frame(o_cors)

### A cors dataset 
a_cors <- matrix(c(achievement_cors["Agreeableness",],
                   benevolence_cors["Agreeableness",], 
                   conformity_cors["Agreeableness",], 
                   hedonism_cors["Agreeableness",], 
                   power_cors["Agreeableness",],
                   security_cors["Agreeableness",], 
                   selfdirection_cors["Agreeableness",], 
                   stimulation_cors["Agreeableness",], 
                   swl_cors["Agreeableness",], 
                   tradition_cors["Agreeableness",], 
                   universalism_cors["Agreeableness",]),
                 byrow = T, ncol = 3)
colnames(a_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(a_cors) <- c("Achievement", "Benevolence", "Conformity", "Hedonism",
                      "Power", "Security", "SelfDirection", "Stimulation", "SWL", "Tradition",
                      "Universalism")
a_cors <- as.data.frame(a_cors)

### C cors dataset 
c_cors <- matrix(c(achievement_cors["Conscientiousness",],
                   benevolence_cors["Conscientiousness",], 
                   conformity_cors["Conscientiousness",], 
                   hedonism_cors["Conscientiousness",], 
                   power_cors["Conscientiousness",],
                   security_cors["Conscientiousness",], 
                   selfdirection_cors["Conscientiousness",], 
                   stimulation_cors["Conscientiousness",], 
                   swl_cors["Conscientiousness",], 
                   tradition_cors["Conscientiousness",], 
                   universalism_cors["Conscientiousness",]),
                 byrow = T, ncol = 3)
colnames(c_cors) <- c("Self-Report", "Trait", "Nuance")
rownames(c_cors) <- c("Achievement", "Benevolence", "Conformity", "Hedonism",
                      "Power", "Security", "SelfDirection", "Stimulation", "SWL", "Tradition",
                      "Universalism")
c_cors <- as.data.frame(c_cors)


### Predicting Outcomes  ------------- 
### Make predictions and correlate
#swl
mod_swl_sr <- lm(swl ~., data = swl[,2:7])
pred_swl_sr <- predict(mod_swl_sr, data = swl)

mod_swl_q <- lm(swl ~ ., data = swl[,c(2,114:118)])
pred_swl_q <- predict(mod_swl_q, data = swl)

mod_swl_traits <- lm(swl ~., data = swl[,c(2,8:12)])
pred_swl_traits <- predict(mod_swl_traits, data = swl)

pred_swl_cors <- c(cor(pred_swl_sr, pred_swl_q),
                   cor(pred_swl_sr, pred_swl_traits))

## Values
# Conformity
mod_conformity_sr <- lm(Conformity ~., data = values[,c(2,12:16)])
pred_conformity_sr <- predict(mod_conformity_sr, data = values)

mod_conformity_q <- lm(Conformity ~ ., data = values[,c(2,123:127)])
pred_conformity_q <- predict(mod_conformity_q, data = values)

mod_conformity_traits <- lm(Conformity ~., data = values[,c(2,17:21)])
pred_conformity_traits <- predict(mod_conformity_traits, data = values)

pred_conformity_cors <- c(cor(pred_conformity_sr, pred_conformity_q),
                          cor(pred_conformity_sr, pred_conformity_traits))

# Tradition
mod_tradition_sr <- lm(Tradition ~., data = values[,c(3,12:16)])
pred_tradition_sr <- predict(mod_tradition_sr, data = values)

mod_tradition_q <- lm(Tradition ~ ., data = values[,c(3,123:127)])
pred_tradition_q <- predict(mod_tradition_q, data = values)

mod_tradition_traits <- lm(Tradition ~., data = values[,c(3,17:21)])
pred_tradition_traits <- predict(mod_tradition_traits, data = values)

pred_tradition_cors <- c(cor(pred_tradition_sr, pred_tradition_q),
                         cor(pred_tradition_sr, pred_tradition_traits))


# Benevolence
mod_benevolence_sr <- lm(Benevolence ~., data = values[,c(4,12:16)])
pred_benevolence_sr <- predict(mod_benevolence_sr, data = values)

mod_benevolence_q <- lm(Benevolence ~ ., data = values[,c(4,123:127)])
pred_benevolence_q <- predict(mod_benevolence_q, data = values)

mod_benevolence_traits <- lm(Benevolence ~., data = values[,c(4,17:21)])
pred_benevolence_traits <- predict(mod_benevolence_traits, data = values)

pred_benevolence_cors <- c(cor(pred_benevolence_sr, pred_benevolence_q),
                           cor(pred_benevolence_sr, pred_benevolence_traits))

# Universalism
mod_universalism_sr <- lm(Universalism ~., data = values[,c(5,12:16)])
pred_universalism_sr <- predict(mod_universalism_sr, data = values)

mod_universalism_q <- lm(Universalism ~ ., data = values[,c(5,123:127)])
pred_universalism_q <- predict(mod_universalism_q, data = values)

mod_universalism_traits <- lm(Universalism ~., data = values[,c(5,17:21)])
pred_universalism_traits <- predict(mod_universalism_traits, data = values)

pred_universalism_cors <- c(cor(pred_universalism_sr, pred_universalism_q),
                            cor(pred_universalism_sr, pred_universalism_traits))

# Self direction
mod_selfdirection_sr <- lm(Self_Direction ~., data = values[,c(6,12:16)])
pred_selfdirection_sr <- predict(mod_selfdirection_sr, data = values)

mod_selfdirection_q <- lm(Self_Direction ~ ., data = values[,c(6,123:127)])
pred_selfdirection_q <- predict(mod_selfdirection_q, data = values)

mod_selfdirection_traits <- lm(Self_Direction ~., data = values[,c(6,17:21)])
pred_selfdirection_traits <- predict(mod_selfdirection_traits, data = values)

pred_selfdirection_cors <- c(cor(pred_selfdirection_sr, pred_selfdirection_q),
                             cor(pred_selfdirection_sr, pred_selfdirection_traits))

# Stimulation
mod_stimulation_sr <- lm(Stimulation ~., data = values[,c(7,12:16)])
pred_stimulation_sr <- predict(mod_stimulation_sr, data = values)

mod_stimulation_q <- lm(Stimulation ~ ., data = values[,c(7,123:127)])
pred_stimulation_q <- predict(mod_stimulation_q, data = values)

mod_stimulation_traits <- lm(Stimulation ~., data = values[,c(7,17:21)])
pred_stimulation_traits <- predict(mod_stimulation_traits, data = values)

pred_stimulation_cors <- c(cor(pred_stimulation_sr, pred_stimulation_q),
                           cor(pred_stimulation_sr, pred_stimulation_traits))

# Hedonism
mod_hedonism_sr <- lm(Hedonism ~., data = values[,c(8,12:16)])
pred_hedonism_sr <- predict(mod_hedonism_sr, data = values)

mod_hedonism_q <- lm(Hedonism ~ ., data = values[,c(8,123:127)])
pred_hedonism_q <- predict(mod_hedonism_q, data = values)

mod_hedonism_traits <- lm(Hedonism ~., data = values[,c(8,17:21)])
pred_hedonism_traits <- predict(mod_hedonism_traits, data = values)

pred_hedonism_cors <- c(cor(pred_hedonism_sr, pred_hedonism_q),
                        cor(pred_hedonism_sr, pred_hedonism_traits))


# Achievement
mod_achievement_sr <- lm(Achievement ~., data = values[,c(9,12:16)])
pred_achievement_sr <- predict(mod_achievement_sr, data = values)

mod_achievement_q <- lm(Achievement ~ ., data = values[,c(9,123:127)])
pred_achievement_q <- predict(mod_achievement_q, data = values)

mod_achievement_traits <- lm(Achievement ~., data = values[,c(9,17:21)])
pred_achievement_traits <- predict(mod_achievement_traits, data = values)

pred_achievement_cors <- c(cor(pred_achievement_sr, pred_achievement_q),
                           cor(pred_achievement_sr, pred_achievement_traits))

# Power 
mod_power_sr <- lm(Power ~., data = values[,c(10,12:16)])
pred_power_sr <- predict(mod_power_sr, data = values)

mod_power_q <- lm(Power ~ ., data = values[,c(10,123:127)])
pred_power_q <- predict(mod_power_q, data = values)

mod_power_traits <- lm(Power ~., data = values[,c(10,17:21)])
pred_power_traits <- predict(mod_power_traits, data = values)

pred_power_cors <- c(cor(pred_power_sr, pred_power_q),
                     cor(pred_power_sr, pred_power_traits))

# Security
mod_security_sr <- lm(Security ~., data = values[,c(11,12:16)])
pred_security_sr <- predict(mod_security_sr, data = values)

mod_security_q <- lm(Security ~ ., data = values[,c(11,123:127)])
pred_security_q <- predict(mod_security_q, data = values)

mod_security_traits <- lm(Security ~., data = values[,c(11,17:21)])
pred_security_traits <- predict(mod_security_traits, data = values)

pred_security_cors <- c(cor(pred_security_sr, pred_security_q),
                        cor(pred_security_sr, pred_security_traits))

#### Combine correlations
pred_cors <- matrix(c(pred_swl_cors,
                      pred_conformity_cors,
                      pred_tradition_cors,
                      pred_benevolence_cors,
                      pred_universalism_cors,
                      pred_selfdirection_cors,
                      pred_stimulation_cors,
                      pred_hedonism_cors,
                      pred_achievement_cors,
                      pred_power_cors,
                      pred_security_cors), byrow = T, ncol = 2)
colnames(pred_cors) <- c("Nuance", "Trait")
rownames(pred_cors) <- c("SWL", "Conformity", "Tradition", 
                         "Benevolence", "Universalism", "Self-Direction",
                         "Stimulation", "Hedonism", "Achievement", "Power", "Security")

pred_cors <- as.data.frame(pred_cors)
pred_cors <- rownames_to_column(pred_cors)
pred_cors <- pred_cors %>% 
  mutate(NuanceMore = if_else(Nuance > Trait, "Item-Level Predicts Better", "Aggregate-Level Predicts Better"))

