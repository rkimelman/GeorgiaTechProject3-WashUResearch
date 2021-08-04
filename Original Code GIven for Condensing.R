## code to create RSA correlations
# version 2.0 - started 3/13/2021 corrects for symmetry issues by rearranging larger subj # to come first when specified (i.e., cor94 sub 9 comes before 4)
# run script from a top-level project folder that contains the data
# this script requires tidy data that have been wrangled by band (RSA Wrangling script will do this)

# load relevant libraries
library(here) #this will reference all directories to the top folder in the working directory
library(tidyverse) #all purpose data wrangling tool
library(writexl) #writing to excel
library(corrplot) #plot correlation matrix

#get data ready in wide form to correlate as entire matrix
#since we are correlating across sites, we want each site to have its own row
#each possible subject/conditions/time combo is then a column to be correlated with each other

#select subject pair from dataset, restructure to wide form then correlate

#delta for subj 3 & 4
delta_corr34 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P004") %>% #select pair
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% #restructure subj, condition, epoch to one column
  select(-band, -site) %>% #remove band and site column to prepare to run through cor() function
  cor() #correlate all columns

#theta for subj 3 & 4
theta_corr34 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P004") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

#alpha for subj 3 & 4
alpha_corr34 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P004") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#beta for subj 3 & 4
beta_corr34 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P004") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#gamma for subj 3 & 4
gamma_corr34 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P004") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 3&5
delta_corr35 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr35 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr35 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr35 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr35 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 3&6
delta_corr36 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr36 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr36 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr36 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr36 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 3&7
delta_corr37 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr37 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr37 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr37 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr37 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 3&8
delta_corr38 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr38 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr38 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr38 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr38 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 3&9
delta_corr39 <- delta_tidy %>% 
  filter(subject == "P003" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr39 <- theta_tidy %>% 
  filter(subject == "P003" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr39 <- alpha_tidy %>% 
  filter(subject == "P003" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr39 <- beta_tidy %>% 
  filter(subject == "P003" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr39 <- gamma_tidy %>% 
  filter(subject == "P003" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&3
delta_corr43 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr43 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr43 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr43 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr43 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&5
delta_corr45 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr45 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr45 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr45 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr45 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P005") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&6
delta_corr46 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr46 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr46 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr46 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr46 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&7
delta_corr47 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr47 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr47 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr47 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr47 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&8
delta_corr48 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr48 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr48 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr48 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr48 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 4&9
delta_corr49 <- delta_tidy %>% 
  filter(subject == "P004" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr49 <- theta_tidy %>% 
  filter(subject == "P004" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr49 <- alpha_tidy %>% 
  filter(subject == "P004" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr49 <- beta_tidy %>% 
  filter(subject == "P004" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr49 <- gamma_tidy %>% 
  filter(subject == "P004" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&3
delta_corr53 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr53 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr53 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P003") %>%
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr53 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr53 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P003") %>%
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&4
delta_corr54 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr54 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr54 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr54 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr54 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&6
delta_corr56 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr56 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr56 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr56 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr56 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P006") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&7
delta_corr57 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr57 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr57 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr57 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr57 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&8
delta_corr58 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr58 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr58 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr58 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr58 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 5&9
delta_corr59 <- delta_tidy %>% 
  filter(subject == "P005" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr59 <- theta_tidy %>% 
  filter(subject == "P005" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr59 <- alpha_tidy %>% 
  filter(subject == "P005" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr59 <- beta_tidy %>% 
  filter(subject == "P005" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr59 <- gamma_tidy %>% 
  filter(subject == "P005" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&3
delta_corr63 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr63 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr63 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr63 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr63 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&4
delta_corr64 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr64 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr64 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr64 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P004") %>%
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr64 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&5
delta_corr65 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr65 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr65 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr65 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr65 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&7
delta_corr67 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr67 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr67 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr67 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr67 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&8
delta_corr68 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr68 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr68 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr68 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr68 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 6&9
delta_corr69 <- delta_tidy %>% 
  filter(subject == "P006" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr69 <- theta_tidy %>% 
  filter(subject == "P006" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr69 <- alpha_tidy %>% 
  filter(subject == "P006" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr69 <- beta_tidy %>% 
  filter(subject == "P006" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr69 <- gamma_tidy %>% 
  filter(subject == "P006" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&3
delta_corr73 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr73 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr73 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr73 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr73 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&4
delta_corr74 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr74 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr74 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr74 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr74 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&5
delta_corr75 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr75 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr75 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr75 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr75 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&6
delta_corr76 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr76 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr76 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr76 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr76 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&8
delta_corr78 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr78 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr78 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr78 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr78 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P008") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 7&9
delta_corr79 <- delta_tidy %>% 
  filter(subject == "P007" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr79 <- theta_tidy %>% 
  filter(subject == "P007" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr79 <- alpha_tidy %>% 
  filter(subject == "P007" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr79 <- beta_tidy %>% 
  filter(subject == "P007" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr79 <- gamma_tidy %>% 
  filter(subject == "P007" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&3
delta_corr83 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr83 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr83 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr83 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr83 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&4
delta_corr84 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr84 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr84 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr84 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr84 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&5
delta_corr85 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr85 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr85 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr85 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr85 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P005") %>%
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&6
delta_corr86 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr86 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr86 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr86 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr86 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&7
delta_corr87 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr87 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr87 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr87 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr87 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 8&9
delta_corr89 <- delta_tidy %>% 
  filter(subject == "P008" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr89 <- theta_tidy %>% 
  filter(subject == "P008" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr89 <- alpha_tidy %>% 
  filter(subject == "P008" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr89 <- beta_tidy %>% 
  filter(subject == "P008" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr89 <- gamma_tidy %>% 
  filter(subject == "P008" | subject == "P009") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&3
delta_corr93 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr93 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr93 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr93 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr93 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P003") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&4
delta_corr94 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr94 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr94 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr94 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr94 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P004") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&5
delta_corr95 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr95 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr95 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr95 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr95 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P005") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&6
delta_corr96 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr96 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P006") %>%
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr96 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr96 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr96 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P006") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&7
delta_corr97 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr97 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr97 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr97 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P007") %>% 
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr97 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P007") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#repeat for subj 9&8
delta_corr98 <- delta_tidy %>% 
  filter(subject == "P009" | subject == "P008") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor() 

theta_corr98 <- theta_tidy %>% 
  filter(subject == "P009" | subject == "P008") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>% 
  cor()

alpha_corr98 <- alpha_tidy %>% 
  filter(subject == "P009" | subject == "P008") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

beta_corr98 <- beta_tidy %>% 
  filter(subject == "P009" | subject == "P008") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

gamma_corr98 <- gamma_tidy %>% 
  filter(subject == "P009" | subject == "P008") %>% 
  arrange(desc(subject)) %>%
  pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>% 
  select(-band, -site) %>%
  cor()

#take avg of subj-pair correlations
delta_corr_sum <- list(delta_corr34, delta_corr35, delta_corr36, delta_corr37, delta_corr38, delta_corr39, 
                       delta_corr43, delta_corr45, delta_corr46, delta_corr47, delta_corr48, delta_corr49,
                       delta_corr53, delta_corr54, delta_corr56, delta_corr57, delta_corr58, delta_corr59,
                       delta_corr63, delta_corr64, delta_corr65, delta_corr67, delta_corr68, delta_corr69,
                       delta_corr73, delta_corr74, delta_corr75, delta_corr76, delta_corr78, delta_corr79,
                       delta_corr83, delta_corr84, delta_corr85, delta_corr86, delta_corr87, delta_corr89,
                       delta_corr93, delta_corr94, delta_corr95, delta_corr96, delta_corr97, delta_corr98)
delta_corr_avg <- Reduce('+', delta_corr_sum)/length(delta_corr_sum)

theta_corr_sum <- list(theta_corr34, theta_corr35, theta_corr36, theta_corr37, theta_corr38, theta_corr39, 
                       theta_corr43, theta_corr45, theta_corr46, theta_corr47, theta_corr48, theta_corr49,
                       theta_corr53, theta_corr54, theta_corr56, theta_corr57, theta_corr58, theta_corr59,
                       theta_corr63, theta_corr64, theta_corr65, theta_corr67, theta_corr68, theta_corr69,
                       theta_corr73, theta_corr74, theta_corr75, theta_corr76, theta_corr78, theta_corr79,
                       theta_corr83, theta_corr84, theta_corr85, theta_corr86, theta_corr87, theta_corr89,
                       theta_corr93, theta_corr94, theta_corr95, theta_corr96, theta_corr97, theta_corr98)
theta_corr_avg <- Reduce('+', theta_corr_sum)/length(theta_corr_sum)

alpha_corr_sum <- list(alpha_corr34, alpha_corr35, alpha_corr36, alpha_corr37, alpha_corr38, alpha_corr39, 
                       alpha_corr43, alpha_corr45, alpha_corr46, alpha_corr47, alpha_corr48, alpha_corr49,
                       alpha_corr53, alpha_corr54, alpha_corr56, alpha_corr57, alpha_corr58, alpha_corr59,
                       alpha_corr63, alpha_corr64, alpha_corr65, alpha_corr67, alpha_corr68, alpha_corr69,
                       alpha_corr73, alpha_corr74, alpha_corr75, alpha_corr76, alpha_corr78, alpha_corr79,
                       alpha_corr83, alpha_corr84, alpha_corr85, alpha_corr86, alpha_corr87, alpha_corr89,
                       alpha_corr93, alpha_corr94, alpha_corr95, alpha_corr96, alpha_corr97, alpha_corr98)
alpha_corr_avg <- Reduce('+', alpha_corr_sum)/length(alpha_corr_sum)

beta_corr_sum <- list(beta_corr34, beta_corr35, beta_corr36, beta_corr37, beta_corr38, beta_corr39, 
                      beta_corr43, beta_corr45, beta_corr46, beta_corr47, beta_corr48, beta_corr49,
                      beta_corr53, beta_corr54, beta_corr56, beta_corr57, beta_corr58, beta_corr59,
                      beta_corr63, beta_corr64, beta_corr65, beta_corr67, beta_corr68, beta_corr69,
                      beta_corr73, beta_corr74, beta_corr75, beta_corr76, beta_corr78, beta_corr79,
                      beta_corr83, beta_corr84, beta_corr85, beta_corr86, beta_corr87, beta_corr89,
                      beta_corr93, beta_corr94, beta_corr95, beta_corr96, beta_corr97, beta_corr98)
beta_corr_avg <- Reduce('+', beta_corr_sum)/length(beta_corr_sum)

gamma_corr_sum <- list(gamma_corr34, gamma_corr35, gamma_corr36, gamma_corr37, gamma_corr38, gamma_corr39, 
                       gamma_corr43, gamma_corr45, gamma_corr46, gamma_corr47, gamma_corr48, gamma_corr49,
                       gamma_corr53, gamma_corr54, gamma_corr56, gamma_corr57, gamma_corr58, gamma_corr59,
                       gamma_corr63, gamma_corr64, gamma_corr65, gamma_corr67, gamma_corr68, gamma_corr69,
                       gamma_corr73, gamma_corr74, gamma_corr75, gamma_corr76, gamma_corr78, gamma_corr79,
                       gamma_corr83, gamma_corr84, gamma_corr85, gamma_corr86, gamma_corr87, gamma_corr89,
                       gamma_corr93, gamma_corr94, gamma_corr95, gamma_corr96, gamma_corr97, gamma_corr98)
gamma_corr_avg <- Reduce('+', gamma_corr_sum)/length(gamma_corr_sum)

#plot correlations using squares
corrplot(delta_corr_avg, method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE)  #turn off labels, adjust corr range & color, add grids

corrplot(theta_corr_avg, method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE) 

corrplot(alpha_corr_avg, method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE) 

corrplot(beta_corr_avg, method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE) 

corrplot(gamma_corr_avg, method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE) 
