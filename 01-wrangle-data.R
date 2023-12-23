# Author: Janae Jackson
# File: 01-wrangle-data.R

library(tidyverse)
library(haven)

# Working directory
setwd("C:/Users/jlj59/Desktop/cmps_final_project")

# CMPS Data
cmps <- read_csv("cmps data/survey_cmps.csv")

## Filter out unnecessary columns and remove NAs
cmps <- cmps |>
  select(record,
         S3b,
         S4,
         S5,
         S13,
         S15,
         race,
         weight,
         Q487_Q489r1,
         Q487_Q489r2,
         Q487_Q489r3) |>
  rename(
    state = S4,
    zipcode = S15,
    judge_race = Q487_Q489r1,
    lawyer_race  = Q487_Q489r2,
    prosecutor_race = Q487_Q489r3,
    gender = S3b,
    birthyr = S5,
    educ = S13,
    id = record
  )

table(cmps$race)

cmps <- cmps |>
  filter(!if_all(c(judge_race, lawyer_race, prosecutor_race), is.na)) # 4,626 observations

year = c(rep("2021", times = 4626))
cmps$year = year
cmps <- cmps |>
  relocate(year, .before = id)
# n = 14,988



# CCES Data
## Adding Congressional district to the dataset
cces <- read_dta("./zipcode_cd116_2020cces.dta")
## Sub-setting the data set to the zipcode and congressional district column
cces <- cces |>
  select(zipcode, cd)
## Taking sub-string zip code variable in the main cmps data to remove the "z"
cmps$zipcode <- substring(cmps$zipcode, 2, 6)
## Merge cces and cmps for congressional district
cmps <- merge(cmps, cces, by = c('zipcode'))
rm(cces)

# State
cmps <- tibble(state = state.name) |>
  bind_cols(tibble(abb = state.abb)) |>
  left_join(cmps, st_crosswalk, by = "state") |>
  rename(st = abb)

# Region
region <-
  as.data.frame(state.region[match(cmps$state, state.name)])
division <-
  as.data.frame(state.division[match(cmps$state, state.name)])
cmps <- cmps |>
  cbind(region, division) |>
  rename(region = `state.region[match(cmps$state, state.name)]`) |>
  rename(division = `state.division[match(cmps$state, state.name)]`)
rm(region, division)


# Age
cmps$age <- 2023 - cmps$birthyr
cmps$age <-
  cut(
    as.integer(cmps$age),
    breaks = c(0, 24, 34, 44, 64, 123),
    labels = c("18-24", "25-34", "35-44", "45-64", "65+")
  )

table(cmps$age)

# Education
cmps$educ <- fct_collapse(
  cmps$educ,
  "HS or Less" =
    c(
      "Grades 1-8",
      "Some High School, but did not graduate",
      "High School graduate or GED"
    )
)

cmps$educ <- fct_collapse(cmps$educ,
                          "Some College" =
                            c("Some college", "Associates, 2-year degree"))

cmps$educ <- fct_collapse(cmps$educ, "4-year" =
                            c("Bachelors, 4-year degree"))

cmps$educ <- fct_collapse(cmps$educ, "Post-Grad" =
                            c("Post-graduate degree"))

table(cmps$educ)


# Race
cmps$race <- fct_collapse(cmps$race, "Hispanic" = c("Latino"))
cmps <- cmps |>
  filter(race == "White" | race == "Black" | race == "Hispanic")



# Gender
table(cmps$gender)
## removing "Non-binary", "Something else"
cmps <- cmps |>
  filter(gender == "Man" | gender == "Woman") |>
  rename(female = gender) |>
  mutate(female = if_else(female == "Woman", 1, 0))
table(cmps$female)



# Defendent-Representation Match ---------
## Defendant-Lawyer Match
cmps <- cmps |>
  mutate(def_lawyer_match = ifelse(((race == "White") &
                                      (
                                        lawyer_race == "White"
                                      )), 1, 0))
cmps <- cmps |>
  mutate(def_lawyer_match = ifelse(((race == "Black") &
                                      (
                                        lawyer_race == "Black"
                                      )), 1, def_lawyer_match))
cmps <- cmps |>
  mutate(def_lawyer_match = ifelse(((race == "Hispanic") &
                                      (lawyer_race == "Latino")
  ), 1, def_lawyer_match))

## Defendant-Judge Match
cmps <- cmps |>
  mutate(def_judge_match = ifelse(((race == "White") &
                                     (
                                       judge_race == "White"
                                     )), 1, 0))
cmps <- cmps |>
  mutate(def_judge_match = ifelse(((race == "Black") &
                                     (
                                       judge_race == "Black"
                                     )), 1, def_judge_match))
cmps <- cmps |>
  mutate(def_judge_match = ifelse(((race == "Hispanic") &
                                     (judge_race == "Latino")
  ), 1, def_judge_match))




## Defendant-Prosecutor Match
cmps <- cmps |>
  mutate(def_prosecutor_match = ifelse(((race == "White") &
                                          (prosecutor_race == "White")
  ), 1, 0))
cmps <- cmps |>
  mutate(def_prosecutor_match = ifelse(((race == "Black") &
                                          (prosecutor_race == "Black")
  ), 1, def_prosecutor_match))
cmps <- cmps |>
  mutate(def_prosecutor_match = ifelse(((race == "Hispanic") &
                                          (prosecutor_race == "Latino")
  ), 1, def_prosecutor_match))


table(cmps$def_judge_match, cmps$race)

write.csv(cmps, "cmps_df.csv")

