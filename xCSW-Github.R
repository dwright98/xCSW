# xCSW - Github Version
library(tidyverse)
library(baseballr)
library(ggplot2)
library(mlbench)
library(Metrics)
library(RMariaDB)
library(DBI)
library(caret)
library(MASS)
library(class)
library(Boruta)
library(randomForest)

data19 = read.csv("data19.csv",stringsAsFactors = F)
player_pos = read.csv("player_pos.csv",stringsAsFactors = F)

# filter out position players pitching
data19_2 <- data19 %>%
  left_join(player_pos,by = c("pitcher" = "player_id")) %>%
  filter(primary_position %in% c(1,10,NA))

fb_avgs <- data19_2 %>%
  filter(pitch_class %in% c("4-Seam Fastball","Sinker")) %>%
  group_by(pitcher, pitcher_name) %>%
  summarise(pitches = n(),
            avg_fb_velo = mean(release_speed,na.rm = T),
            avg_fb_hmov = mean(pfx_x,na.rm = T),
            avg_fb_vmov = mean(pfx_z,na.rm = T))

total_pitches <- data19_2 %>%
  dplyr::select(pitcher,pitches_thrown)%>%
  distinct()

tp_pitchclass <- data19_2 %>%
  group_by(pitcher,pitch_class)%>%
  summarise(pitches_by_pitchclass = n())%>%
  inner_join(total_pitches, by = "pitcher")

data19_3 <- data19_2 %>%
  inner_join(tp_pitchclass %>% dplyr::select(pitcher,pitch_class,pitches_by_pitchclass), by = c("pitcher","pitch_class"))%>%
  mutate(pfx_x_2 = ifelse(p_throws == "L",pfx_x * -1,pfx_x),
         release_pos_x_2 = ifelse(p_throws == "L",release_pos_x * -1,release_pos_x))

data19_3 %>%
  dplyr::select(p_throws,pfx_x,pfx_x_2,release_pos_x,release_pos_x_2)

CSW <- data19_3 %>%
  filter(description %in% c("called_strike","swinging_strike"))%>%
  group_by(pitcher,pitcher_name,pitch_class,description)%>%
  summarise(count = n()) %>%
  spread(description,count)%>%
  inner_join(tp_pitchclass, by = c("pitch_class","pitcher"))%>%
  mutate(pitch_pct = round(pitches_by_pitchclass/pitches_thrown,5),
         called_strike = ifelse(is.na(called_strike),0,called_strike),
         swinging_strike = ifelse(is.na(swinging_strike),0,swinging_strike),
         CSWs = called_strike + swinging_strike,
         pitch_CSW = (called_strike+swinging_strike)/pitches_by_pitchclass,
         pitch_CSW = round(pitch_CSW,3))%>%
  dplyr::select(pitcher,pitcher_name,pitch_class,pitches_thrown,pitches_by_pitchclass,CSWs,pitch_CSW) 

non_fb_profiles <- data19_3 %>%
  filter(pitch_class != "4-Seam Fastball" & pitch_class != "Sinker")%>%
  group_by(pitcher,pitcher_name,pitch_class)%>%
  summarize(pitches = n(),
            avg_velo = mean(release_speed,na.rm = T),
            avg_hmov = mean(pfx_x_2,na.rm = T),
            avg_vmov = mean(pfx_z,na.rm = T),
            avg_release_x = mean(release_pos_x_2,na.rm = T),
            avg_release_z = mean(release_pos_z,na.rm = T),
            avg_release_ext = mean(release_extension,na.rm = T),
            avg_spin = mean(release_spin_rate,na.rm = T),
            avg_loc_x = mean(plate_x,na.rm =T),
            avg_loc_z = mean(plate_z,na.rm = T)) %>%
  left_join(fb_avgs %>% dplyr::select(pitcher,pitcher_name,avg_fb_velo,
                                      avg_fb_hmov,avg_fb_vmov),
            by = c("pitcher","pitcher_name"))%>%
  mutate(velo_difference = avg_velo - avg_fb_velo,
         hmov_difference = avg_hmov - avg_fb_hmov,
         vmov_difference = avg_vmov - avg_fb_vmov)%>%
  dplyr::select(-avg_fb_velo,-avg_fb_hmov,-avg_fb_vmov)%>%
  left_join(CSW %>% dplyr::select(pitcher,pitcher_name,pitch_class,pitch_CSW),
            by = c("pitcher","pitcher_name","pitch_class"))%>%
  mutate(pitch_CSW = ifelse(is.na(pitch_CSW),0,pitch_CSW))

#FC Profiles
FC_profiles <- non_fb_profiles %>%
  filter(pitch_class == "Cutter" & pitches >= 75)
colSums(is.na(FC_profiles))
FC_profiles$velo_difference[is.na(FC_profiles$velo_difference)]<- 0
FC_profiles$hmov_difference[is.na(FC_profiles$hmov_difference)]<- 0
FC_profiles$vmov_difference[is.na(FC_profiles$vmov_difference)]<- 0

# CH Profiles
CH_profiles <- non_fb_profiles %>%
  filter(pitch_class == "Changeup" & pitches >= 65)
colSums(is.na(CH_profiles))
CH_profiles$velo_difference[is.na(CH_profiles$velo_difference)]<- (CH_profiles$avg_velo[CH_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_velo[FC_profiles$pitcher_name == "Bryan Shaw"])
CH_profiles$hmov_difference[is.na(CH_profiles$hmov_difference)]<- (CH_profiles$avg_hmov[CH_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_hmov[FC_profiles$pitcher_name == "Bryan Shaw"])
CH_profiles$vmov_difference[is.na(CH_profiles$vmov_difference)]<- (CH_profiles$avg_vmov[CH_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_vmov[FC_profiles$pitcher_name == "Bryan Shaw"])

#CU Profiles
CU_profiles <- non_fb_profiles %>%
  filter(pitch_class == "Curveball" & pitches >= 75)
colSums(is.na(CU_profiles))
CU_profiles$velo_difference[is.na(CU_profiles$velo_difference)]<- (CU_profiles$avg_velo[CU_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_velo[FC_profiles$pitcher_name == "Bryan Shaw"])
CU_profiles$hmov_difference[is.na(CU_profiles$hmov_difference)]<- (CU_profiles$avg_hmov[CU_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_hmov[FC_profiles$pitcher_name == "Bryan Shaw"])
CU_profiles$vmov_difference[is.na(CU_profiles$vmov_difference)]<- (CU_profiles$avg_vmov[CU_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_vmov[FC_profiles$pitcher_name == "Bryan Shaw"])

#SL Profiles
SL_profiles <- non_fb_profiles %>%
  filter(pitch_class == "Slider" & pitches >= 70)
colSums(is.na(SL_profiles))
SL_profiles$velo_difference[is.na(SL_profiles$velo_difference)]<- (SL_profiles$avg_velo[SL_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_velo[FC_profiles$pitcher_name == "Bryan Shaw"])
SL_profiles$hmov_difference[is.na(SL_profiles$hmov_difference)]<- (SL_profiles$avg_hmov[SL_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_hmov[FC_profiles$pitcher_name == "Bryan Shaw"])
SL_profiles$vmov_difference[is.na(SL_profiles$vmov_difference)]<- (SL_profiles$avg_vmov[SL_profiles$pitcher_name == "Bryan Shaw"] - FC_profiles$avg_vmov[FC_profiles$pitcher_name == "Bryan Shaw"])


fb_profiles <- data19_3 %>%
  filter(pitch_class == "4-Seam Fastball" | pitch_class == "Sinker")%>%
  group_by(pitcher,pitcher_name,pitch_class)%>%
  summarize(pitches = n(),
            avg_velo = mean(release_speed,na.rm = T),
            avg_hmov = mean(pfx_x_2,na.rm = T),
            avg_vmov = mean(pfx_z,na.rm = T),
            avg_release_x = mean(release_pos_x_2,na.rm = T),
            avg_release_z = mean(release_pos_z,na.rm = T),
            avg_release_ext = mean(release_extension,na.rm = T),
            avg_spin = mean(release_spin_rate,na.rm = T),
            avg_loc_x = mean(plate_x,na.rm =T),
            avg_loc_z = mean(plate_z,na.rm = T)) %>%
  left_join(CSW %>% dplyr::select(pitcher,pitcher_name,pitch_class,pitch_CSW),
            by = c("pitcher","pitcher_name","pitch_class"))%>%
  mutate(pitch_CSW = ifelse(is.na(pitch_CSW),0,pitch_CSW))

FF_profiles <- fb_profiles %>%
  filter(pitch_class == "4-Seam Fastball" & pitches >= 100)

colSums(is.na(FF_profiles))

SI_profiles <- fb_profiles %>%
  filter(pitch_class == "Sinker" & pitches >= 100)
colSums(is.na(SI_profiles))


threshold <- c(100,65,75,79,100,79)

pitch_threshold <- data.frame(x_2,threshold) %>%
  rename('Pitch Class' = x_2,"Minimum Pitches Thrown" = threshold)

# Baseline Model - Linear Regression - Fastballs
ff_lm_smp_size <- floor(0.6 * nrow(FF_profiles))
ff_lm_train_indexes <-sample(seq_len(nrow(FF_profiles)), size = ff_lm_smp_size)
ff_lm_train <- FF_profiles[ff_lm_train_indexes, ]                                
ff_lm_test <- head(FF_profiles[-ff_lm_train_indexes, ],ff_lm_smp_size)

ff_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z, data = ff_lm_train)

ff_lm_preds <- predict(ff_lm,newdata = ff_lm_test)
ff_lm_preds_all <- predict(ff_lm,newdata = FF_profiles)
ff_lm_rmse <- rmse(ff_lm_preds,ff_lm_test$pitch_CSW)
ff_lm_rmse_all <- rmse(ff_lm_preds_all,FF_profiles$pitch_CSW)
ff_lm_rmse
ff_lm_rmse_all
sd(ff_lm_test$pitch_CSW)
sd(FF_profiles$pitch_CSW)

# Baseline Model - Linear Regression - Sinkers
si_lm_smp_size <- floor(0.6 * nrow(SI_profiles))
si_lm_train_indexes <-sample(seq_len(nrow(SI_profiles)), size = si_lm_smp_size)
si_lm_train <- SI_profiles[si_lm_train_indexes, ]                                
si_lm_test <- head(SI_profiles[-si_lm_train_indexes, ],si_lm_smp_size)

si_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z, data = si_lm_train)

si_lm_preds <- predict(si_lm,newdata = si_lm_test)
si_lm_preds_all <- predict(si_lm,newdata = SI_profiles)
si_lm_rmse <- rmse(si_lm_preds,si_lm_test$pitch_CSW)
si_lm_rmse_all <- rmse(si_lm_preds_all,SI_profiles$pitch_CSW)
si_lm_rmse
si_lm_rmse_all
sd(si_lm_test$pitch_CSW)
sd(SI_profiles$pitch_CSW)

# Baseline Model - Linear Regression - Changeups
ch_lm_smp_size <- floor(0.6 * nrow(CH_profiles))
ch_lm_train_indexes <-sample(seq_len(nrow(CH_profiles)), size = ch_lm_smp_size)
ch_lm_train <- CH_profiles[ch_lm_train_indexes, ]                                
ch_lm_test <- head(CH_profiles[-ch_lm_train_indexes, ],ch_lm_smp_size)

ch_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
            + hmov_difference + vmov_difference, data = ch_lm_train)

ch_lm_preds <- predict(ch_lm,newdata = ch_lm_test)
ch_lm_preds_all <- predict(ch_lm,newdata = CH_profiles)
ch_lm_rmse <- rmse(ch_lm_preds,ch_lm_test$pitch_CSW)
ch_lm_rmse_all <- rmse(ch_lm_preds_all,CH_profiles$pitch_CSW)
ch_lm_rmse
ch_lm_rmse_all
sd(ch_lm_test$pitch_CSW)
sd(CH_profiles$pitch_CSW)

# Baseline Model - Linear Regression - Curveballs
cu_lm_smp_size <- floor(0.6 * nrow(CU_profiles))
cu_lm_train_indexes <-sample(seq_len(nrow(CU_profiles)), size = cu_lm_smp_size)
cu_lm_train <- CU_profiles[cu_lm_train_indexes, ]                                
cu_lm_test <- head(CU_profiles[-cu_lm_train_indexes, ],cu_lm_smp_size)

cu_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
            + hmov_difference + vmov_difference, data = cu_lm_train)

cu_lm_preds <- predict(cu_lm,newdata = cu_lm_test)
cu_lm_preds_all <- predict(cu_lm,newdata = CU_profiles)
cu_lm_rmse <- rmse(cu_lm_preds,cu_lm_test$pitch_CSW)
cu_lm_rmse_all <- rmse(cu_lm_preds_all,CU_profiles$pitch_CSW)
cu_lm_rmse
cu_lm_rmse_all
sd(cu_lm_test$pitch_CSW)
sd(CU_profiles$pitch_CSW)

# Baseline Model - Linear Regression - Sliders
sl_lm_smp_size <- floor(0.6 * nrow(SL_profiles))
sl_lm_train_indexes <-sample(seq_len(nrow(SL_profiles)), size = sl_lm_smp_size)
sl_lm_train <- SL_profiles[sl_lm_train_indexes, ]                                
sl_lm_test <- head(SL_profiles[-sl_lm_train_indexes, ],sl_lm_smp_size)

sl_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
            + hmov_difference + vmov_difference, data = sl_lm_train)

sl_lm_preds <- predict(sl_lm,newdata = sl_lm_test)
sl_lm_preds_all <- predict(sl_lm,newdata = SL_profiles)
sl_lm_rmse <- rmse(sl_lm_preds,sl_lm_test$pitch_CSW)
sl_lm_rmse_all <- rmse(sl_lm_preds_all,SL_profiles$pitch_CSW)
sl_lm_rmse
sl_lm_rmse_all
sd(sl_lm_test$pitch_CSW)
sd(SL_profiles$pitch_CSW)

# Baseline Model - Linear Regression - Cutters
fc_lm_smp_size <- floor(0.6 * nrow(FC_profiles))
fc_lm_train_indexes <-sample(seq_len(nrow(FC_profiles)), size = fc_lm_smp_size)
fc_lm_train <- FC_profiles[fc_lm_train_indexes, ]                                
fc_lm_test <- head(FC_profiles[-fc_lm_train_indexes, ],fc_lm_smp_size)

fc_lm <- lm(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
              avg_release_x + avg_release_z + avg_release_ext
            + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
            + hmov_difference + vmov_difference, data = fc_lm_train)

fc_lm_preds <- predict(fc_lm,newdata = fc_lm_test)
fc_lm_preds_all <- predict(fc_lm,newdata = FC_profiles)
fc_lm_rmse <- rmse(fc_lm_preds,fc_lm_test$pitch_CSW)
fc_lm_rmse_all <- rmse(fc_lm_preds_all,FC_profiles$pitch_CSW)
fc_lm_rmse
fc_lm_rmse_all
sd(fc_lm_test$pitch_CSW)
sd(FC_profiles$pitch_CSW)

w = c("pitch_class","rmse_all_preds","sd_csw")
x = c("FF","SI","CH","CU","SL","FC")
y = c(ff_lm_rmse_all,si_lm_rmse_all,ch_lm_rmse_all,
      cu_lm_rmse_all,sl_lm_rmse_all,fc_lm_rmse_all)
z = c(sd(FF_profiles$pitch_CSW),sd(SI_profiles$pitch_CSW),sd(CH_profiles$pitch_CSW),
      sd(CU_profiles$pitch_CSW),sd(SL_profiles$pitch_CSW),sd(FC_profiles$pitch_CSW))

baseline_results <- data.frame(x,y,z) %>%
  rename('Pitch Class' = x, 'xCSW RMSE' = y, 'CSW Standard Deviation' = z)

# Feature Selection - FF _sampled
Boruta_FF <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z ,data = FF_profiles)
plot(Boruta_FF)
print(Boruta_FF)
FF_fs <- data.frame(attStats(Boruta_FF))
FF_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Feature Selection - SI #_sampled
#SI_profiles_sampled = SI_profiles[sample(nrow(SI_profiles), size = nrow(SI_profiles)*.9),]
Boruta_SI <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z ,data = SI_profiles)

plot(Boruta_SI)
print(Boruta_SI)
SI_fs <- data.frame(attStats(Boruta_SI))
SI_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Feature Selection - CH _sampled
#CH_profiles_sampled = CH_profiles[sample(nrow(CH_profiles), size = nrow(CH_profiles)*.9),]
Boruta_CH <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
                    + hmov_difference + vmov_difference,data = CH_profiles)
plot(Boruta_CH)
print(Boruta_CH)
CH_fs <- data.frame(attStats(Boruta_CH))
CH_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Feature Selection - CU _sampled
#CU_profiles_sampled = CU_profiles[sample(nrow(CU_profiles), size = nrow(CU_profiles)*.9),]
Boruta_CU <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
                    + hmov_difference + vmov_difference,data = CU_profiles)
plot(Boruta_CU)
print(Boruta_CU)
CU_fs <- data.frame(attStats(Boruta_CU))
CU_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Feature Selection - SL _sampled
#SL_profiles_sampled = SL_profiles[sample(nrow(SL_profiles), size = nrow(SL_profiles)*.9),]
Boruta_SL <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
                    + hmov_difference + vmov_difference,data = SL_profiles)
plot(Boruta_SL)
print(Boruta_SL)
SL_fs <- data.frame(attStats(Boruta_SL))
SL_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Feature Selection - FC _sampled
#FC_profiles_sampled = FC_profiles[sample(nrow(FC_profiles), size = nrow(FC_profiles)*.9),]
Boruta_FC <- Boruta(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + 
                      avg_release_x + avg_release_z + avg_release_ext
                    + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
                    + hmov_difference + vmov_difference,data = FC_profiles)
plot(Boruta_FC)
print(Boruta_FC)
FC_fs <- data.frame(attStats(Boruta_FC))
FC_fs%>%
  dplyr::select(meanImp, decision)%>%
  arrange(desc(meanImp))

# Split All Datasets into Train / Test
## Fastballs 
ff_smp_size <-   floor(0.7 * nrow(FF_profiles))
ff_train_indexes <-sample(seq_len(nrow(FF_profiles)), size = ff_smp_size)
ff_train <- FF_profiles[ff_train_indexes, ]                                
ff_test <-   head(FF_profiles[-ff_train_indexes, ],ff_smp_size)
ff_forest <- randomForest(pitch_CSW ~ avg_hmov + avg_vmov + avg_release_x + avg_release_z + avg_release_ext + avg_spin, 
                          data = ff_train)
summary(ff_forest)
ff_preds <- predict(ff_forest,newdata = ff_test)
ff_preds_all <- predict(ff_forest,newdata = FF_profiles)
ff_rmse <- rmse(ff_preds,ff_test$pitch_CSW)
ff_rmse_all <- rmse(ff_preds_all,FF_profiles$pitch_CSW)
ff_rmse
ff_rmse_all
sd(ff_test$pitch_CSW)
sd(FF_profiles$pitch_CSW)

FF_profiles_2 <- FF_profiles
FF_profiles_2$xCSW <- 0 
FF_profiles_2$xCSW <- round(ff_preds_all,3)
FF_profiles_2$velo_difference <- NA
FF_profiles_2$hmov_difference <- NA
FF_profiles_2$vmov_difference <- NA


#Sinkers
si_smp_size <-   floor(0.7 * nrow(SI_profiles))
si_train_indexes <-sample(seq_len(nrow(SI_profiles)), size = si_smp_size)
si_train <- SI_profiles[si_train_indexes, ]                                
si_test  <- head(SI_profiles[-si_train_indexes, ],si_smp_size)
si_forest <- randomForest(pitch_CSW ~ avg_hmov + avg_vmov + avg_release_x + avg_release_z + avg_release_ext + avg_spin, 
                          data = si_train)
si_preds <- predict(si_forest,newdata = si_test)
si_preds_all <- predict(si_forest, newdata = SI_profiles)
si_rmse <- rmse(si_preds,si_test$pitch_CSW)
si_rmse_all <- rmse(si_preds_all,SI_profiles$pitch_CSW)
si_rmse
si_rmse_all
sd(si_test$pitch_CSW)
sd(SI_profiles$pitch_CSW)

SI_profiles_2 <- SI_profiles
SI_profiles_2$xCSW <- 0 
SI_profiles_2$xCSW <- round(si_preds_all,3)
SI_profiles_2$velo_difference <- NA
SI_profiles_2$hmov_difference <- NA
SI_profiles_2$vmov_difference <- NA

#Changeups
ch_smp_size <-   floor(0.7 * nrow(CH_profiles))
ch_train_indexes <-sample(seq_len(nrow(CH_profiles)), size = ch_smp_size)
ch_train <- CH_profiles[ch_train_indexes, ]                                
ch_test  <- head(CH_profiles[-ch_train_indexes, ],ch_smp_size)

ch_forest <- randomForest(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + avg_spin + avg_loc_x + avg_loc_z + velo_difference 
                          + hmov_difference + vmov_difference, data = ch_train)

ch_preds <- predict(ch_forest,newdata = ch_test)
ch_preds_all <- predict(ch_forest, newdata = CH_profiles)
ch_rmse <- rmse(ch_preds,ch_test$pitch_CSW)
ch_rmse_all <- rmse(ch_preds_all,CH_profiles$pitch_CSW)
ch_rmse
ch_rmse_all
sd(ch_test$pitch_CSW)
sd(CH_profiles$pitch_CSW)

CH_profiles_2 <- CH_profiles
CH_profiles_2$xCSW <- 0 
CH_profiles_2$xCSW <- round(ch_preds_all,3)

#Curveballs
cu_smp_size <-   floor(0.7 * nrow(CU_profiles))
cu_train_indexes <-sample(seq_len(nrow(CU_profiles)), size = cu_smp_size)
cu_train <- CU_profiles[cu_train_indexes, ]                                
cu_test  <- head(CU_profiles[-cu_train_indexes, ],cu_smp_size)

cu_forest <- randomForest(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + avg_release_x + avg_spin + avg_loc_x 
                          + avg_loc_z + velo_difference + vmov_difference,data = cu_train)

cu_preds <- predict(cu_forest,newdata = cu_test)
cu_preds_all <- predict(cu_forest, newdata = CU_profiles)
cu_rmsle <- rmse(cu_preds,cu_test$pitch_CSW)
cu_rmse_all <- rmse(cu_preds_all,CU_profiles$pitch_CSW)
cu_rmsle
cu_rmse_all
sd(cu_test$pitch_CSW)
sd(CU_profiles$pitch_CSW)

CU_profiles_2 <- CU_profiles
CU_profiles_2$xCSW <- 0 
CU_profiles_2$xCSW <- round(cu_preds_all,3)

#Sliders
sl_smp_size <-   floor(0.7 * nrow(SL_profiles))
sl_train_indexes <-sample(seq_len(nrow(SL_profiles)), size = sl_smp_size)
sl_train <- SL_profiles[sl_train_indexes, ]                                
sl_test  <- head(SL_profiles[-sl_train_indexes, ],sl_smp_size)

sl_forest <- randomForest(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + avg_release_x + avg_release_z + avg_release_ext
                          + avg_spin + avg_loc_x + avg_loc_z + velo_difference + hmov_difference + vmov_difference,
                          data = sl_train)

sl_preds <- predict(sl_forest,newdata = sl_test)
sl_preds_all <- predict(sl_forest, newdata = SL_profiles)
sl_rmse <- rmse(sl_preds,sl_test$pitch_CSW)
sl_rmse_all <- rmse(sl_preds_all,SL_profiles$pitch_CSW)
sl_rmse
sl_rmse_all
sd(sl_test$pitch_CSW)
sd(SL_profiles$pitch_CSW)

SL_profiles_2 <- SL_profiles
SL_profiles_2$xCSW <- 0 
SL_profiles_2$xCSW <- round(sl_preds_all,3)

#Cutters
fc_smp_size <-   floor(0.7 * nrow(FC_profiles))
fc_train_indexes <-sample(seq_len(nrow(FC_profiles)), size = fc_smp_size)
fc_train <- FC_profiles[fc_train_indexes, ]                                
fc_test  <- head(FC_profiles[-fc_train_indexes, ],fc_smp_size)

fc_forest <- randomForest(pitch_CSW ~ avg_velo + avg_hmov + avg_vmov + avg_spin + avg_loc_x + avg_loc_z
                          + velo_difference + hmov_difference + vmov_difference, data = fc_train)

fc_preds <- predict(fc_forest,newdata = fc_test)
fc_preds_all <- predict(fc_forest, newdata = FC_profiles)
fc_rmse <- rmse(fc_preds,fc_test$pitch_CSW)
fc_rmse_all <- rmse(fc_preds_all,FC_profiles$pitch_CSW)
fc_rmse
fc_rmse_all
sd(fc_test$pitch_CSW)
sd(FC_profiles$pitch_CSW)

FC_profiles_2 <- FC_profiles
FC_profiles_2$xCSW <- 0 
FC_profiles_2$xCSW <- round(fc_preds_all,3)

All_Profiles <- rbind(FF_profiles_2,SI_profiles_2,CH_profiles_2,
                      CU_profiles_2,SL_profiles_2,FC_profiles_2)

pitcher_teams <- data19%>%
  dplyr::select(pitcher,pitcher_name,pitch_team)%>%
  distinct()%>%
  arrange(pitcher_name)%>%
  mutate(team_num = NA)%>%
  dplyr::select(pitcher,pitcher_name,team_num,pitch_team)

for (i in 1:nrow(pitcher_teams)){
  ifelse(pitcher_teams$pitcher[i] == pitcher_teams$pitcher[i+1],pitcher_teams$team_num[i] <- "team2",pitcher_teams$team_num[i] <- "team1")
}

for (i in 1:nrow(pitcher_teams)){
  ifelse(pitcher_teams$pitcher[i] == pitcher_teams$pitcher[i+1] & pitcher_teams$team_num[i] == pitcher_teams$team_num[i+1],pitcher_teams$team_num[i] <- "team3",pitcher_teams$team_num[i] <- pitcher_teams$team_num[i])
}

pitcher_teams_2 <- pitcher_teams %>%
  spread(key = team_num,value = pitch_team)

pitcher_teams_2$team1[pitcher_teams_2$pitcher_name == "Zack Wheeler"] <- "NYM"

pitcher_teams_3 <- pitcher_teams_2 %>%
  dplyr::select(-'<NA>')%>%
  mutate(team1_2 = team1,
         team2_2 = team2,
         team3_2 = team3,
         team1 = ifelse(!is.na(team2),team2_2,team1),
         team2 = ifelse(!is.na(team2),team1_2,NA))

pitcher_teams_3$team1[pitcher_teams_3$pitcher_name == "Jesse Biddle"] <- "ATL"
pitcher_teams_3$team2[pitcher_teams_3$pitcher_name == "Jesse Biddle"] <- "SEA"
pitcher_teams_3$team3[pitcher_teams_3$pitcher_name == "Jesse Biddle"] <- "TEX"
pitcher_teams_3$team1[pitcher_teams_3$pitcher_name == "Adalberto Mejia"] <- "MIN"
pitcher_teams_3$team2[pitcher_teams_3$pitcher_name == "Adalberto Mejia"] <- "LAA"
pitcher_teams_3$team3[pitcher_teams_3$pitcher_name == "Adalberto Mejia"] <- "STL"
pitcher_teams_3$team1[pitcher_teams_3$pitcher_name == "Ryan Dull"] <- "OAK"
pitcher_teams_3$team2[pitcher_teams_3$pitcher_name == "Ryan Dull"] <- "NYY"
pitcher_teams_3$team3[pitcher_teams_3$pitcher_name == "Ryan Dull"] <- "TOR"
pitcher_teams_3$team1[pitcher_teams_3$pitcher_name == "Zac Rosscup"] <- "SEA"
pitcher_teams_3$team2[pitcher_teams_3$pitcher_name == "Zac Rosscup"] <- "TOR"
pitcher_teams_3$team3[pitcher_teams_3$pitcher_name == "Zac Rosscup"] <- "LAD"
pitcher_teams_3$team1[pitcher_teams_3$pitcher_name == "Wilmer Font"] <- "TB"
pitcher_teams_3$team2[pitcher_teams_3$pitcher_name == "Wilmer Font"] <- "NYM"
pitcher_teams_3$team3[pitcher_teams_3$pitcher_name == "Wilmer Font"] <- "TOR"

pitcher_teams_4 <- pitcher_teams_3 %>%
  dplyr::select(-team1_2,-team2_2,-team3_2)

All_Profiles_2 <- All_Profiles %>%
  left_join(pitcher_teams_4, by = c("pitcher","pitcher_name"))%>%
  dplyr::select(pitcher,pitcher_name,team1,team2,team3,pitch_class,pitches,avg_velo,
                avg_hmov,avg_vmov,avg_release_x,avg_release_z,avg_release_ext,
                avg_spin,avg_loc_x,avg_loc_z,velo_difference,hmov_difference,
                vmov_difference,pitch_CSW,xCSW)

write.csv(All_Profiles_2,"xCSW.csv",row.names = F)

top_10_ff <- All_Profiles_2 %>%
  filter(pitch_class == "4-Seam Fastball" & pitches >= 500)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

top_10_si <- All_Profiles_2 %>%
  filter(pitch_class == "Sinker" & pitches >= 300)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

top_10_ch <- All_Profiles_2 %>%
  filter(pitch_class == "Changeup" & pitches >= 200)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

top_10_cu <- All_Profiles_2 %>%
  filter(pitch_class == "Curveball" & pitches >= 250)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

top_10_sl <- All_Profiles_2 %>%
  filter(pitch_class == "Slider" & pitches >= 300)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

top_10_fc <- All_Profiles_2 %>%
  filter(pitch_class == "Cutter" & pitches >= 320)%>%
  dplyr::select(pitcher_name,pitches,pitch_CSW,xCSW)%>%
  arrange(desc(xCSW))%>%
  head(10)

x_2 = c("FF","SI","CH","CU","SL","FC")
y_2 = c(ff_rmse_all,si_rmse_all,ch_rmse_all,cu_rmse_all,sl_rmse_all,fc_rmse_all)
z_2 = c(sd(FF_profiles$pitch_CSW),sd(SI_profiles$pitch_CSW),sd(CH_profiles$pitch_CSW),
        sd(CU_profiles$pitch_CSW),sd(SL_profiles$pitch_CSW),sd(FC_profiles$pitch_CSW))

forest_results <- data.frame(x_2,y_2,z_2) %>%
  rename('Pitch Class' = x_2,'xCSW RMSE' = y_2,'CSW Standard Deviation' = z_2)