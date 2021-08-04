library(here) #this will reference all directories to the top folder in the working directory
library(tidyverse) #all purpose data wrangling tool
library(writexl) #writing to excel
library(corrplot)

here::here()


## code to wrangle EEG RSA data
# version 1.0 - started 3/3/2021
# run script from a top-level project folder that contains the data

# load fastmerge function to quickly merge dataframes

# read data into R

delta <- read.csv('eeg_data/RSA_Delta_full.csv')
theta <- read.csv('eeg_data/RSA_Theta_full.csv')
alpha <- read.csv('eeg_data/RSA_Alpha_full.csv')
beta <- read.csv('eeg_data/RSA_Beta_full.csv')
gamma <- read.csv('eeg_data/RSA_Gamma_full.csv')

# add appropriate prefix to column labels
colnames(delta) <- paste("delta", colnames(delta), sep = "_")
colnames(theta) <- paste("theta", colnames(theta), sep = "_")
colnames(alpha) <- paste("alpha", colnames(alpha), sep = "_")
colnames(beta) <- paste("beta", colnames(beta), sep = "_")
colnames(gamma) <- paste("gamma", colnames(gamma), sep = "_")

#wrangle data to tidyformat
delta_tidy <- delta %>%
  pivot_longer(cols = -delta_File, names_to = "electrode", values_to = "power") %>% #pivot to long
  separate(electrode, c("band", "site", "discard", "epoch"), sep = "_")  %>% #separate info in electrode
  separate(site, c("site", "discard2")) %>% #separate useless FFT suffix
  separate(delta_File, c("subject", "condition")) %>% #separate subject and condition
  select(-c("discard", "discard2")) %>%  #drop discard and discard2
  mutate(subject = str_sub(subject, 2, -1)) %>% #remove P
  mutate(subject = as.numeric(subject)) %>% #change to numeric
  mutate(power = log(power)) #take natural log to normalize

theta_tidy <- theta %>%
  pivot_longer(cols = -theta_File, names_to = "electrode", values_to = "power") %>% #pivot to long
  separate(electrode, c("band", "site", "discard", "epoch"), sep = "_")  %>% #separate info in electrode
  separate(site, c("site", "discard2")) %>% #separate useless FFT suffix
  separate(theta_File, c("subject", "condition")) %>% #separate subject and condition
  select(-c("discard", "discard2")) %>% #drop discard and discard2
  mutate(subject = str_sub(subject, 2, -1)) %>% #remove P
  mutate(subject = as.numeric(subject)) %>% #change to numeric
  mutate(power = log(power)) #take natural log to normalize

alpha_tidy <- alpha %>%
  pivot_longer(cols = -alpha_File, names_to = "electrode", values_to = "power") %>% #pivot to long
  separate(electrode, c("band", "site", "discard", "epoch"), sep = "_")  %>% #separate info in electrode
  separate(site, c("site", "discard2")) %>% #separate useless FFT suffix
  separate(alpha_File, c("subject", "condition")) %>% #separate subject and condition
  select(-c("discard", "discard2")) %>% #drop discard and discard2
  mutate(subject = str_sub(subject, 2, -1)) %>% #remove P
  mutate(subject = as.numeric(subject)) %>% #change to numeric
  mutate(power = log(power)) #take natural log to normalize

beta_tidy <- beta %>%
  pivot_longer(cols = -beta_File, names_to = "electrode", values_to = "power") %>% #pivot to long
  separate(electrode, c("band", "site", "discard", "epoch"), sep = "_")  %>% #separate info in electrode
  separate(site, c("site", "discard2")) %>% #separate useless FFT suffix
  separate(beta_File, c("subject", "condition")) %>% #separate subject and condition
  select(-c("discard", "discard2")) %>% #drop discard and discard2
  mutate(subject = str_sub(subject, 2, -1)) %>% #remove P
  mutate(subject = as.numeric(subject)) %>% #change to numeric
  mutate(power = log(power)) #take natural log to normalize

gamma_tidy <- gamma %>%
  pivot_longer(cols = -gamma_File, names_to = "electrode", values_to = "power") %>% #pivot to long
  separate(electrode, c("band", "site", "discard", "epoch"), sep = "_")  %>% #separate info in electrode
  separate(site, c("site", "discard2")) %>% #separate useless FFT suffix
  separate(gamma_File, c("subject", "condition")) %>% #separate subject and condition
  select(-c("discard", "discard2")) %>% #drop discard and discard2
  mutate(subject = str_sub(subject, 2, -1)) %>% #remove P
  mutate(subject = as.numeric(subject)) %>% #change to numeric
  mutate(power = log(power)) #take natural log to normalize

#merge band dataframes for full band
fullband_tidy <- delta_tidy %>%
  rbind(theta_tidy) %>%
  rbind(alpha_tidy) %>%
  rbind(beta_tidy) %>%
  rbind(gamma_tidy)

#wrangle data to tidyformat

listSums <- list()
sums <- list()
list_1_all <- list()

band_corr_sum <- list()
band_corr_sum_novices <- list()
band_corr_sum_experts <- list()

band_wsub_mwmw <- list()
band_wsub_mwpc <- list()
band_wsub_pcmw <- list()
band_wsub_pcpc <- list()
band_bsub_mwmw <- list()
band_bsub_mwpc <- list()
band_bsub_pcmw <- list()
band_bsub_pcpc <- list()

band_wsub_all <- list()
band_wsub_novices <- list()
band_wsub_experts <- list()

band_bsub_all <- list()
band_bsub_novices <- list()
band_bsub_experts <- list()

band_wsub_mwmw_novices <- list()
band_wsub_mwpc_novices <- list()
band_wsub_pcmw_novices <- list()
band_wsub_pcpc_novices <- list()
band_bsub_mwmw_novices <- list()
band_bsub_mwpc_novices <- list()
band_bsub_pcmw_novices <- list()
band_bsub_pcpc_novices <- list()

band_wsub_mwmw_experts <- list()
band_wsub_mwpc_experts <- list()
band_wsub_pcmw_experts <- list()
band_wsub_pcpc_experts <- list()
band_bsub_mwmw_experts <- list()
band_bsub_mwpc_experts <- list()
band_bsub_pcmw_experts <- list()
band_bsub_pcpc_experts <- list()

#wrangle data to tidyformat

bands <- list(gamma_tidy, beta_tidy, alpha_tidy, delta_tidy, theta_tidy)
COPY <- gamma_tidy
sum <- 0
sum_1 <- 0
list_avg <- list()
list_avg_novices <- list()
list_avg_experts <- list()
list_sums <- list()
list_band_corrs <- list()
list_band_corrs_novices <- list()
list_band_corrs_experts <- list()

for(r in 1:5){
  sum <- 0
  sum_1 <- 0
  sum_experts <- 0
  sum_e <- 0
  sum_novices <- 0
  sum_n <- 0
  list_band_corrs[[r]] <- list()
  list_band_corrs_novices[[r]] <- list()
  list_band_corrs_experts[[r]] <- list()
  list_sums[[r]] <- list()
  list_avg[[r]] <- list()
  list_avg_novices[[r]] <- list()
  list_avg_experts[[r]] <- list()
  COPY <- bands[[r]]
  for(i in 3:20){
    list_band_corrs[[r]][[i]] <- list()
    if(i < 12){
      list_band_corrs_novices[[r]][[i]] <- list()
    }
    if(i > 11){
      list_band_corrs_experts[[r]][[i]] <- list()
    }
    for(j in 3:20){
      if(i!=j & i > j){
        save <- bands[[r]] %>%
          filter(subject == i  | subject == j) %>%
          arrange(desc(subject)) %>%
          pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>%
          select(-band,-site) %>%
          cor()
        list_band_corrs[[r]][[i]][[j]] <- save
        if(i < 12 & j < 12){
          list_band_corrs_novices[[r]][[i]][[j]] <- save
          sum_novices <- save + sum_novices
          sum_n <- sum_n + 1
        }
        if(i > 11 & j > 11){
          list_band_corrs_experts[[r]][[i]][[j]] <- save
          sum_experts <- save + sum_experts
          sum_e <- sum_e + 1
        }
        sum_1 <- save + sum_1
        sum <- sum + 1
      }
      if(i!=j & i < j){
        save <- bands[[r]] %>%
          filter(subject == i  | subject == j) %>%
          pivot_wider(id_cols = c("band", "site"), names_from = c("subject", "condition", "epoch"), values_from = "power") %>%
          select(-band,-site) %>%
          cor()
        list_band_corrs[[r]][[i]][[j]] <- save
        if(i < 12 & j < 12){
          list_band_corrs_novices[[r]][[i]][[j]] <- save
          sum_novices <- save + sum_novices
          sum_n <- sum_n + 1
        }
        if(i > 11 & j > 11){
          list_band_corrs_experts[[r]][[i]][[j]] <- save
          sum_experts <- save + sum_experts
          sum_e <- sum_e + 1
        }
        sum_1 <- save + sum_1
        sum <- sum + 1
      }
    }
  }
  list_avg[[r]] <- sum_1/sum
  list_avg_experts[[r]] <- sum_experts/sum_e
  list_avg_novices[[r]] <- sum_novices/sum_n
  list_sums[[r]] <- sum_1
  
  band_corr_sum[[r]] <- lapply(rapply(list_band_corrs[[r]], enquote, how = "unlist"), eval)
  band_corr_sum_novices[[r]] <- lapply(rapply(list_band_corrs_novices[[r]], enquote, how = "unlist"), eval)
  band_corr_sum_experts[[r]] <- lapply(rapply(list_band_corrs_experts[[r]], enquote, how = "unlist"), eval)
  
  #extract avg value of entire within sub square (60x60 upper left quadrant) for all band subj-pair correlations
  band_wsub_all[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:60, 1:60])} ) %>% 
    unlist()
  
  band_wsub_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:60, 1:60])} ) %>% 
    unlist()
  
  band_wsub_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:60, 1:60])} ) %>% 
    unlist()
  
  #entire between sub square (60x60 upper right quadrant)
  band_bsub_all[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:60, 61:120])} ) %>% 
    unlist()
  
  band_bsub_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:60, 61:120])} ) %>% 
    unlist()
  
  band_bsub_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:60, 61:120])} ) %>% 
    unlist()
  
  #within sub mwmw square (30x30)
  band_wsub_mwmw[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:30, 1:30])} ) %>% 
    unlist()
  
  band_wsub_mwmw_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:30, 1:30])} ) %>% 
    unlist()
  
  band_wsub_mwmw_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:30, 1:30])} ) %>% 
    unlist()
  
  #within sub mwpc square (30x30)
  band_wsub_mwpc[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:30, 31:60])} ) %>% 
    unlist()
  
  band_wsub_mwpc_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:30, 31:60])} ) %>% 
    unlist()
  
  band_wsub_mwpc_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:30, 31:60])} ) %>% 
    unlist()
  
  #within sub pcmw square (30x30) this is identical as mwpc.. but lets just compute for full transparency
  band_wsub_pcmw[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[31:60, 1:30])} ) %>% 
    unlist()
  
  band_wsub_pcmw_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[31:60, 1:30])} ) %>% 
    unlist()
  
  band_wsub_pcmw_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[31:60, 1:30])} ) %>% 
    unlist()
  
  #within sub pcpc square (30x30) 
  band_wsub_pcpc[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[31:60, 31:60])} ) %>% 
    unlist()
  
  band_wsub_pcpc_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[31:60, 31:60])} ) %>% 
    unlist()
  
  band_wsub_pcpc_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[31:60, 31:60])} ) %>% 
    unlist()
  
  #between sub mwmw square (30x30)
  band_bsub_mwmw[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:30, 61:90])} ) %>% 
    unlist()
  
  band_bsub_mwmw_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:30, 61:90])} ) %>% 
    unlist()
  
  band_bsub_mwmw_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:30, 61:90])} ) %>% 
    unlist()
  
  #between sub mwpc square (30x30)
  band_bsub_mwpc[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[1:30, 91:120])} ) %>% 
    unlist()
  
  band_bsub_mwpc_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[1:30, 91:120])} ) %>% 
    unlist()
  
  band_bsub_mwpc_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[1:30, 91:120])} ) %>% 
    unlist()
  
  #between sub pcmw square (30x30) this is identical as mwpc.. but lets just compute for full transparency
  band_bsub_pcmw[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[31:60, 61:90])} ) %>% 
    unlist()
  
  band_bsub_pcmw_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[31:60, 61:90])} ) %>% 
    unlist()
  
  band_bsub_pcmw_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[31:60, 61:90])} ) %>% 
    unlist()
  
  #between sub pcpc square (30x30) 
  band_bsub_pcpc[[r]] <- lapply(band_corr_sum[[r]], function(x) {mean(x[31:60, 91:120])} ) %>% 
    unlist()
  
  band_bsub_pcpc_novices[[r]] <- lapply(band_corr_sum_novices[[r]], function(x) {mean(x[31:60, 91:120])} ) %>% 
    unlist()
  
  band_bsub_pcpc_experts[[r]] <- lapply(band_corr_sum_experts[[r]], function(x) {mean(x[31:60, 91:120])} ) %>% 
    unlist()
  
  
  #conduct wsub ttests
  print(r)
  print(t.test(band_wsub_mwmw[[r]], band_wsub_mwpc[[r]])) #wsub mwmw vs mwpc
  print(t.test(band_wsub_mwmw_novices[[r]], band_wsub_mwpc_novices[[r]])) #wsub mwmw vs mwpc
  print(t.test(band_wsub_mwmw_experts[[r]], band_wsub_mwpc_experts[[r]])) #wsub mwmw vs mwpc
  
  print(t.test(band_wsub_pcpc[[r]], band_wsub_mwpc[[r]])) #wsub pcpc vs mwpc
  print(t.test(band_wsub_pcpc_novices[[r]], band_wsub_mwpc_novices[[r]])) #wsub pcpc vs mwpc
  print(t.test(band_wsub_pcpc_experts[[r]], band_wsub_mwpc_experts[[r]])) #wsub pcpc vs mwpc
  
  print(t.test(band_wsub_mwmw[[r]], band_wsub_pcpc[[r]])) #wsub mwmw vs pcpc
  print(t.test(band_wsub_mwmw_novices[[r]], band_wsub_pcpc_novices[[r]])) #wsub mwmw vs pcpc
  print(t.test(band_wsub_mwmw_experts[[r]], band_wsub_pcpc_experts[[r]])) #wsub mwmw vs pcpc
  
  
  #conduct bsub ttests
  print(t.test(band_bsub_mwmw[[r]], band_bsub_mwpc[[r]])) #bsub mwmw vs mwpc
  print(t.test(band_bsub_mwmw_novices[[r]], band_bsub_mwpc_novices[[r]])) #bsub mwmw vs mwpc
  print(t.test(band_bsub_mwmw_experts[[r]], band_bsub_mwpc_experts[[r]])) #bsub mwmw vs mwpc
  
  print(t.test(band_bsub_pcpc[[r]], band_bsub_mwpc[[r]])) #bsub pcpc vs mwpc
  print(t.test(band_bsub_pcpc_novices[[r]], band_bsub_mwpc_novices[[r]])) #bsub pcpc vs mwpc
  print(t.test(band_bsub_pcpc_experts[[r]], band_bsub_mwpc_experts[[r]])) #bsub pcpc vs mwpc
  
  print(t.test(band_bsub_mwmw[[r]], band_bsub_pcpc[[r]])) #bsub mwmw vs pcpc
  print(t.test(band_bsub_mwmw_novices[[r]], band_bsub_pcpc_novices[[r]])) #bsub mwmw vs pcpc
  print(t.test(band_bsub_mwmw_experts[[r]], band_bsub_pcpc_experts[[r]])) #bsub mwmw vs pcpc
  
  #corrplots
  corrplot(list_avg[[r]], method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE)
  corrplot(list_avg_novices[[r]], method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE)
  corrplot(list_avg_experts[[r]], method = "color", tl.pos = "n", cl.lim = c(0,1), is.corr = FALSE, addgrid.col = TRUE)
}