# Author: Janae Jackson
# File: 02-descriptive-analysis.R

library(tidyverse)
library(gt)
library(lme4)
library(webshot2)

# Working Directory
setwd("C:/Users/jlj59/Desktop/cmps_final_project")

# Data
cmps_df <- read_csv("cmps_df.csv")

# Descriptive Statistics ------
sum(table(cmps_df$judge_race)) # n = 3455respondents
table(cmps_df$race) # White: 799 | Hispanic: 1,172 | Black: 1,484

# Transforming the dependent variables to BINARY OUTCOME variables ---------
cmps_fmt <- cmps_df |>
  mutate(
    white_judge = as.numeric(judge_race == "White"),
    black_judge = as.numeric(judge_race == "Black"),
    hispanic_judge = as.numeric(judge_race == "Latino"),
    asian_judge = as.numeric(judge_race == "Asian"),
    other_judge = as.numeric(judge_race == "Other"),
    unknown_judge = as.numeric(judge_race == "Don\x92t know/Can\x92t remember")
  ) |>
  mutate(
    white_lawyer = as.numeric(lawyer_race == "White"),
    black_lawyer = as.numeric(lawyer_race == "Black"),
    hispanic_lawyer = as.numeric(lawyer_race == "Latino"),
    asian_lawyer = as.numeric(lawyer_race == "Asian"),
    other_lawyer = as.numeric(lawyer_race == "Other"),
    unknown_lawyer = as.numeric(lawyer_race == "Don\x92t know/Can\x92t remember")
  ) |>
  mutate(
    white_prosecutor = as.numeric(prosecutor_race == "White"),
    black_prosecutor = as.numeric(prosecutor_race == "Black"),
    hispanic_prosecutor = as.numeric(prosecutor_race == "Latino"),
    asian_prosecutor = as.numeric(prosecutor_race == "Asian"),
    other_prosecutor = as.numeric(prosecutor_race == "Other"),
    unknown_prosecutor = as.numeric(prosecutor_race == "Don\x92t know/Can\x92t remember")
  )















# SUMMARY STATISTICS (table 1) ------
# breakdown of legal professions by race (table 1)
# What was the proportion of judges who were White, Black, Hispanic, and all other races?
tbl_1 <- cmps_fmt |>
  select(white_judge,
         black_judge,
         hispanic_judge,
         asian_judge,
         other_judge,
         unknown_judge) |>
  summarise(
    white = mean(white_judge),
    black = mean(black_judge),
    hispanic = mean(hispanic_judge),
    asian = mean(asian_judge),
    other = mean(other_judge),
    dont_know = mean(unknown_judge)
  )

# What was the proportion of lawyers who were White, Black, Hispanic, and all other races?
tbl_2 <- cmps_fmt |>
  select(
    white_lawyer,
    black_lawyer,
    hispanic_lawyer,
    asian_lawyer,
    other_lawyer,
    unknown_lawyer
  ) |>
  summarise(
    white = mean(white_lawyer),
    black = mean(black_lawyer),
    hispanic = mean(hispanic_lawyer),
    asian = mean(asian_lawyer),
    other = mean(other_lawyer),
    dont_know = mean(unknown_lawyer)
  )

# What was the proportion of prosecutors who were White, Black, Hispanic, and all other races?
tbl_3 <- cmps_fmt |>
  select(
    white_prosecutor,
    black_prosecutor,
    hispanic_prosecutor,
    asian_prosecutor,
    other_prosecutor,
    unknown_prosecutor
  ) |>
  summarise(
    white = mean(white_prosecutor),
    black = mean(black_prosecutor),
    hispanic = mean(hispanic_prosecutor),
    asian = mean(asian_prosecutor),
    other = mean(other_prosecutor),
    dont_know = mean(unknown_prosecutor)
  )
# bind those three dataframes into one dataframe
table1 <- rbind(tbl_1, tbl_2, tbl_3)
# create a vector called "profession" that includes three elements: "judge", "Lawyer", "Prosecutor"
# I am doing this because I want to add this vector as a column to the "table1" dataframe
profession <- as.data.frame(c("Judge", "Lawyer", "Prosecutor"))
#
table1 <- table1 |> cbind(profession) |>
  rename(profession = 7) |>
  relocate(profession)


# WHITE
tbl_wht_judge <- cmps_fmt |>
  select(
    race,
    white_judge,
    black_judge,
    hispanic_judge,
    asian_judge,
    other_judge,
    unknown_judge
  ) |>
  filter(race == "White") |>
  summarise(
    white = mean(white_judge),
    black = mean(black_judge),
    hispanic = mean(hispanic_judge),
    asian = mean(asian_judge),
    other = mean(other_judge),
    dont_know = mean(unknown_judge)
  )

tbl_wht_lawyer <- cmps_fmt |>
  select(
    race,
    white_lawyer,
    black_lawyer,
    hispanic_lawyer,
    asian_lawyer,
    other_lawyer,
    unknown_lawyer
  ) |>
  filter(race == "White") |>
  summarise(
    white = mean(white_lawyer),
    black = mean(black_lawyer),
    hispanic = mean(hispanic_lawyer),
    asian = mean(asian_lawyer),
    other = mean(other_lawyer),
    dont_know = mean(unknown_lawyer)
  )

tbl_wht_prosecutor <- cmps_fmt |>
  select(
    race,
    white_prosecutor,
    black_prosecutor,
    hispanic_prosecutor,
    asian_prosecutor,
    other_prosecutor,
    unknown_prosecutor
  ) |>
  filter(race == "White") |>
  summarise(
    white = mean(white_prosecutor),
    black = mean(black_prosecutor),
    hispanic = mean(hispanic_prosecutor),
    asian = mean(asian_prosecutor),
    other = mean(other_prosecutor),
    dont_know = mean(unknown_prosecutor)
  )

table2 <- rbind(tbl_wht_judge, tbl_wht_lawyer, tbl_wht_prosecutor)
profession <- as.data.frame(c("Judge", "Lawyer", "Prosecutor"))
table2 <- table2 |> cbind(profession) |>
  rename(profession = 7) |>
  relocate(profession)


# BLACK
tbl_blk_judge <- cmps_fmt |>
  select(
    race,
    white_judge,
    black_judge,
    hispanic_judge,
    asian_judge,
    other_judge,
    unknown_judge
  ) |>
  filter(race == "Black") |>
  summarise(
    white = mean(white_judge),
    black = mean(black_judge),
    hispanic = mean(hispanic_judge),
    asian = mean(asian_judge),
    other = mean(other_judge),
    dont_know = mean(unknown_judge)
  )

tbl_blk_lawyer <- cmps_fmt |>
  select(
    race,
    white_lawyer,
    black_lawyer,
    hispanic_lawyer,
    asian_lawyer,
    other_lawyer,
    unknown_lawyer
  ) |>
  filter(race == "Black") |>
  summarise(
    white = mean(white_lawyer),
    black = mean(black_lawyer),
    hispanic = mean(hispanic_lawyer),
    asian = mean(asian_lawyer),
    other = mean(other_lawyer),
    dont_know = mean(unknown_lawyer)
  )

tbl_blk_prosecutor <- cmps_fmt |>
  select(
    race,
    white_prosecutor,
    black_prosecutor,
    hispanic_prosecutor,
    asian_prosecutor,
    other_prosecutor,
    unknown_prosecutor
  ) |>
  filter(race == "Black") |>
  summarise(
    white = mean(white_prosecutor),
    black = mean(black_prosecutor),
    hispanic = mean(hispanic_prosecutor),
    asian = mean(asian_prosecutor),
    other = mean(other_prosecutor),
    dont_know = mean(unknown_prosecutor)
  )

table3 <- rbind(tbl_blk_judge, tbl_blk_lawyer,
                tbl_blk_prosecutor)
profession <- as.data.frame(c("Judge", "Lawyer", "Prosecutor"))
table3 <- table3 |> cbind(profession) |>
  rename(profession = 7) |>
  relocate(profession)

# HISPANIC
tbl_hisp_judge <- cmps_fmt |>
  select(
    race,
    white_judge,
    black_judge,
    hispanic_judge,
    asian_judge,
    other_judge,
    unknown_judge
  ) |>
  filter(race == "Hispanic") |>
  summarise(
    white = mean(white_judge),
    black = mean(black_judge),
    hispanic = mean(hispanic_judge),
    asian = mean(asian_judge),
    other = mean(other_judge),
    dont_know = mean(unknown_judge)
  )

tbl_hisp_lawyer <- cmps_fmt |>
  select(
    race,
    white_lawyer,
    black_lawyer,
    hispanic_lawyer,
    asian_lawyer,
    other_lawyer,
    unknown_lawyer
  ) |>
  filter(race == "Hispanic") |>
  summarise(
    white = mean(white_lawyer),
    black = mean(black_lawyer),
    hispanic = mean(hispanic_lawyer),
    asian = mean(asian_lawyer),
    other = mean(other_lawyer),
    dont_know = mean(unknown_lawyer)
  )

tbl_hisp_prosecutor <- cmps_fmt |>
  select(
    race,
    white_prosecutor,
    black_prosecutor,
    hispanic_prosecutor,
    asian_prosecutor,
    other_prosecutor,
    unknown_prosecutor
  ) |>
  filter(race == "Hispanic") |>
  summarise(
    white = mean(white_prosecutor),
    black = mean(black_prosecutor),
    hispanic = mean(hispanic_prosecutor),
    asian = mean(asian_prosecutor),
    other = mean(other_prosecutor),
    dont_know = mean(unknown_prosecutor)
  )

table4 <-
  rbind(tbl_hisp_judge, tbl_hisp_lawyer, tbl_hisp_prosecutor)
profession <- as.data.frame(c("Judge", "Lawyer", "Prosecutor"))
table4 <- table4 |> cbind(profession) |>
  rename(profession = 7) |>
  relocate(profession)

FullTable <- rbind(table1, table2, table3, table4)

obs <- as.data.frame(
  c(
    "3455",
    "3455",
    "3455",
    "799",
    "799",
    "799",
    "1482",
    "1482",
    "1482",
    "1172",
    "1172",
    "1172"
  )
)
FullTable <- FullTable |>
  cbind(obs) |>
  rename(n = 8)
# View(FullTable)



# gt table
SummaryStatistics1 <-
  gt(FullTable) |>
  tab_row_group(group = "(d) All",
                rows = 1:3) |>
  tab_row_group(group = "(c) Hispanic",
                rows = 10:12) |>
  tab_row_group(group = "(b) Black",
                rows = 7:9) |>
  tab_row_group(group = "(a) White",
                rows = 4:6) |>
  fmt_number(decimals = 2) |>
  fmt_integer(columns = n) |>
  cols_label(
    white = "White",
    black = "Black",
    hispanic = "Hispanic",
    asian = "Asian",
    other = "Other",
    dont_know = "Can't Remember",
    n = "N",
    profession = " "
  ) |>
  tab_spanner("Court Professional by Race", columns = 2:7)  |>
  tab_header(title = " ",
             subtitle = "Summary Statistics (Cont.)") |>
  tab_footnote(footnote = "Note: This table shows the proportion of judges,
    attorneys, and prosecutors by race. Data: 2020 CMPS")

SummaryStatistics1 |>
  gtsave("SummaryStatistics1.png",
         vwidth = 800,
         vheight = 900)









# SUMMARY STATISTICS (table 2) ------
# co-racial legal representation
table_5_temp <- cmps_fmt |>
  group_by(race) |>
  select(def_judge_match, def_lawyer_match, def_prosecutor_match) |>
  summarise(
    Judge = mean(def_judge_match),
    Lawyer = mean(def_lawyer_match),
    Prosecutor = mean(def_prosecutor_match)
  )


FullTable2 <- as.data.frame(t(table_5_temp))[-1,]



FullTable2$V1 <- as.numeric(FullTable2$V1)
FullTable2$V2 <- as.numeric(FullTable2$V2)
FullTable2$V3 <- as.numeric(FullTable2$V3)

FullTable2 <- rename(FullTable2,
                     black = V1,
                     hispanic = V2,
                     white = V3)

profession <- as.character(c("Judge", "Attorney", "Prosecutor"))
FullTable2 <- FullTable2 |> cbind(profession) |>
  relocate(profession)

obs <- as.data.frame(c(rep("3455", times = 3)))
FullTable2 <- FullTable2 |>
  cbind(obs) |>
  rename(N = 5)



# gt table
SummaryStatistics2  <-
  gt(FullTable2) |>
  tab_spanner("Race", columns = 2:4)  |>
  tab_header(title = "",
             subtitle = "Table 1. Summary Statistics") |>
  fmt_number(decimals = 2) |>
  cols_label(
    white = "White",
    black = "Black",
    hispanic = "Hispanic",
    N = "N",
    profession = " "
  )  |>
  tab_footnote(footnote = "Note: This table shows the proportion of co-racial representation
    by court professional across racial groups. Data: 2020 CMPS")

SummaryStatistics2 |>
  gtsave("SummaryStatistics2.png",
         vwidth = 800,
         vheight = 900)

