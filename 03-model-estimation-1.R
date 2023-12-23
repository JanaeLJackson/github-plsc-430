# Author: Janae Jackson
# File: 03-model-estimation-1.R

library(tidyverse)
library(sf)
library(knitr)
library(gt)
library(haven)
library(webshot2)
library(stargazer)
library(gt)
library(lme4)

# Working Directory 
setwd("C:/Users/jlj59/Desktop/cmps_final_project")

# CMPS Data
cmps_df <- read_csv("cmps_df.csv")

# Census Data
cens <- read_rds("target_census.rds")

# Data
est_long <- read_dta("cces_by-agg-level.dta") 
st_groupings <- read_csv("states-groupings.csv") 


# MRP (Multilevel Regression with Post-stratification)
pct_cd_race <- est_long |> 
  filter(level == "cd" & year == 2020) |>  
  mutate(race = as_factor(race)) |> 
  mutate(st = str_sub(cd, 1, 2)) |>
  pivot_wider(id_cols = c(year, cd, st), names_from = race, values_from = frac_race_in_geo, names_prefix = "pct_cd_")


pct_cd_race <- left_join(pct_cd_race, st_groupings, by = "st")
cmps_df2 <- left_join(cmps_df, pct_cd_race, by = c("cd", "region", "st"))


#  model (Lawyer)
lawyer_match_model <- glmer(
  def_lawyer_match ~
    (1 | female) + (1 | age) + (1 | educ) + (1 | race) + (1| region / st / cd) + pct_cd_Black + 
    pct_cd_Hispanic + pct_cd_White, data = cmps_df2, family = binomial)
summary(lawyer_match_model) 



# model (Judge)
judge_match_model <- glmer(
  def_judge_match ~
    (1 | female) + (1 | age) + (1 | educ) + (1 | race) + (1| region / st / cd) + pct_cd_Black + 
    pct_cd_Hispanic + pct_cd_White, data = cmps_df2, family = binomial)
summary(judge_match_model)

# model (Prosecutor)
prosecutor_match_model  <- glmer(
  def_prosecutor_match ~
    (1 | female) + (1 | age) + (1 | educ) + (1 | race) + (1| region / st / cd) + pct_cd_Black + 
    pct_cd_Hispanic + pct_cd_White, data = cmps_df2, family = binomial)
summary(prosecutor_match_model)

# Weighted Estimate of Defendant-Lawyer match
cens_2 <- left_join(cens, pct_cd_race, by = c("cd", "st"))


# raw proportions of defendant-lawyer match by race across congressional districts
dir_estimates <- cmps_df2 |>
  group_by(race, cd) |>
  summarize(prop_lawyer_match = mean(def_lawyer_match), 
            prop_judge_match = mean(def_judge_match), 
            prop_prosecutor_match = mean(def_prosecutor_match))


cd_ests <- cens_2 |>
  mutate(pred_lawyer = predict(lawyer_match_model, cens_2, allow.new.levels = TRUE, type = "response"))|>  
  mutate(pred_judge = predict(judge_match_model, cens_2, allow.new.levels = TRUE, type = "response")) |>
  mutate(pred_prosecutor = predict(prosecutor_match_model, cens_2, allow.new.levels = TRUE, type = "response")) |>
  filter(race != "Other")|>
  summarize(pred_lawyer = weighted.mean(pred_lawyer, N), 
            pred_judge = weighted.mean(pred_judge, N),
            pred_prosecutor = weighted.mean(pred_prosecutor, N),
            .by = c(cd, race)) |>
  left_join(dir_estimates)


cd_shp <- read_rds("shp_cd.rds") 
st_shp <- read_rds("shp_st.rds")
fig1 <- left_join(cd_shp, cd_ests, by = "cd")


pal <- "PiYG"
lim <-  c(0, 1)
brk <- seq(0.10, 0.90, by = 0.10)
ut <- "%"
dir <- -1


### -----
# ggplot (1)
fig_attorney <- fig1 |> 
  filter(!is.na(race)) |>
  ggplot(aes(fill = pred_lawyer)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_viridis_c(labels = scales::percent) +
  ggtitle(' ') +
  ggthemes::theme_map() + 
  labs(fill = "Shared Attorney-Defendent Race") + 
  theme(legend.background=element_blank(), 
        legend.position = c(.6, .008),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), 
        legend.direction = 'horizontal',
        strip.text = element_text(size = 15), 
        strip.background = element_blank()) +
  facet_wrap(~ race, nrow = 1) 

ggsave(height = 6, width = 9, "fig_attorney.png")



# ggplot (2)
fig_judge <- fig1 |> 
  filter(!is.na(race)) |>
  ggplot(aes(fill = pred_judge)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_viridis_c(labels = scales::percent) +
  ggtitle('') +
  ggthemes::theme_map() + 
  labs(fill = "Shared Judge-Defendent Race") + 
  theme(legend.background=element_blank(), 
        legend.position = c(.6, .008),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), 
        legend.direction = 'horizontal',
        strip.background = element_blank(), 
        strip.text = element_text(size = 15)) +
  facet_wrap(~ race, nrow = 1) 

ggsave(height = 6, width = 9, "fig_judge.png")


# ggplot (3)
fig_prosecutor <- fig1 |> 
  filter(!is.na(race)) |>
  ggplot(aes(fill = pred_prosecutor)) +
  geom_sf(color = "white", size = 0.1) +
  scale_fill_viridis_c(labels = scales::percent) +
  ggtitle('') +
  ggthemes::theme_map() + 
  labs(fill = "Shared Prosecutor-Defendent Race") + 
  theme(legend.background=element_blank(), 
        legend.position = c(.6, .008),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6), 
        legend.direction = 'horizontal',
        strip.background = element_blank(), 
        strip.text = element_text(size = 15)) +
  facet_wrap(~ race, nrow = 1) 

ggsave(height = 6, width = 9,"fig_prosecutor.png")





### ------
# subset by prop. 
# Defendant-Lawyer Estimates 
lawyer_black <- fig1 |> filter(pred_lawyer > .20 & (race == "Black")) # 16 obs. 
lawyer_black <- arrange(lawyer_black, desc(pred_lawyer)) 


# Defendant-Lawyer Estimates 
lawyer_hispanic <- fig1 |> filter(pred_lawyer > .2 & (race == "Hispanic")) # 38 obs. 
lawyer_hispanic <- arrange(lawyer_hispanic, desc(pred_lawyer)) 
                        
# Defendant-Lawyer Estimates 
lawyer_white <- fig1 |> filter(pred_lawyer > .4 & (race == "White"))


### ---
# subset by prop. 
# Defendant-Prosecutor Estimates 
prosecutor_black <- fig1 |> filter(pred_prosecutor > .2 & (race == "Black")) # 2 obs. 
prosecutor_black <- arrange(prosecutor_black, desc(pred_prosecutor)) 


# Defendant-Prosecutor Estimates 
prosecutor_hispanic <- fig1 |> filter(pred_prosecutor > .2 & (race == "Hispanic")) # 6 obs. 
prosecutor_hispanic <- arrange(prosecutor_hispanic, desc(pred_prosecutor)) 

# Defendant-Prosecutor Estimates 
prosecutor_white <- fig1 |> filter(pred_prosecutor > .4 & (race == "White")) # 428 obs. 
prosecutor_white <- arrange(prosecutor_white, desc(pred_prosecutor)) 

# 428 (black) / 16 (hispanic)


### ---
# subset by prop. 
# Defendant-Judge Estimates 
judge_black <- fig1 |> filter(pred_judge > .2 & (race == "Black")) # 43
judge_black <- arrange(judge_black, desc(pred_judge)) 


# Defendant-Judge Estimates 
judge_hispanic <- fig1 |> filter(pred_judge > .2 & (race == "Hispanic")) # 23
judge_hispanic <- arrange(judge_hispanic, desc(pred_judge)) 

# Defendant-Prosecutor Estimates 
judge_white <- fig1 |> filter(pred_prosecutor > .4 & (race == "White")) # 428
judge_white <- arrange(judge_white, desc(pred_judge)) 

