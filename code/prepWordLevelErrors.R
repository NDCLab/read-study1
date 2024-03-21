# Real-world Exploration of Adolescent Development | Study 1
# Luc Sahar -- Neural Dynamics of Control Laboratory
# Florida International University

# Report on READ study 1 data by error type as a function of participant,
# passage, and condition.

# last updated 03/21/2024

library(glue)
library(dplyr)
library(rlang)
library(tidyr)

# flag: do we want to stop at each stop and view?
VIEW_MODE=FALSE

# this script does not need to be general to the extent that genScaffolds and
# preproc do
path_to_read_dataset <- ('../read-study1-dataset')

column_name_replacements <-
  c("misproduced_syllable" = "misproduction",
    "omitted_syllable" = "omission",
    "last_pre_correction_syllable" = "word_with_last_pre_correction_syllable")

replacement_fn <- \(.) str_replace_all(., column_name_replacements)

preprocessed_data <-
  path_to_read_dataset %>%
  paste("derivatives",
        "disfluencies_subject-x-passage-x-word_20240315_0110am.csv",
        sep = '/') %>%
  read.csv %>%
  data.frame %>%
  rename_with(replacement_fn)

# we're renaming "misproduced_syllable" to "misproduction", etc., because
# "syllable" is misleading-- this is all at the word level

# rename(misproduction = misproduced_syllable) is insufficient because it won't
# catch `any_upcoming_misproduced_syllable`, `any_prior_misproduced_syllable`,
# `hesitation_with_any_prior_misproduced_syllable`, etc

counterbalance_info <- data.frame(
  participant_id = c(3200002, 3200001, 3200004, 3200007, 3200010, 3200011,
                     3200005, 3200003, 3200006, 3200008, 3200009),
  task_cb = c("A1",
              "A2",
              "B1",
              "B2",
              "A1",
              "A2",
              "B1",
              "B2",
              "A1",
              "A2",
              "B1"),
  list_cb = c("X",
              "X",
              "X",
              "X",
              "X",
              "X",
              "X",
              "X",
              "Y",
              "Y",
              "Y")
)

view_if <- function(df) { if (VIEW_MODE) View(df) }
view_last <- function() { View(.Last.value) }
view_last_if <- function() { view_if(.Last.value) }


counterbalance_data <-
  counterbalance_info %>%
  mutate(alone_first = substr(task_cb, start = 2, stop = 2) == "2",
         flanker_first = substr(task_cb, start = 1, stop = 1) == "A",
         set_a_first = list_cb == "X",
         set_a_alone = alone_first == set_a_first) %>%
  select(-task_cb, -list_cb)

# VIEW_MODE=TRUE
# VIEW_MODE=FALSE
view_if(counterbalance_data)

# first, grand total


# BINGO

percentize <- function(rate) {
  sprintf("%s%%", round(rate * 100, digits = 2))
}

percentize_multiple <- function(df, cols) {
  cols <- df %>% select({{cols}}) %>% colnames
  percents <-
    df %>%
    select(cols) %>%
    map_df(percentize) %>%
    setNames(cols %>% paste0("_percent"))

  df %>%
    mutate(
      percents
    )
}

# pivot_mean_and_sd_longer <- function(df, .to_percent = TRUE) {
#   result <-
#     df %>%
#     pivot_longer(
#       names_to = c("error_type", ".value"),
#       names_pattern = "(.*)_(sd|rate)",
#       cols = c(ends_with("_sd"), ends_with("_rate"))
#     )
#
#   if (.to_percent) {
#     mutate(result, rate = sprintf("%s%%", round(rate * 100, digits = 2)))
#   } else {
#     result
#   }
# }

rates_long <-
  preprocessed_data %>%
  reframe(
    across(misproduction:correction,  # compute the mean for each error type
           \(.) mean(., na.rm = TRUE))) %>%
  pivot_longer(names_to = "error_type",
               values_to = "rate_of_error_type",
               cols = everything())
# but how do we do this with a grouping variable?

# as percents:
rates_long_with_percents <-
  rates_long %>%
  mutate(rate_as_percent = percentize(rate_of_error_type))

# add sd as last row
append_sd_as_last_row <- function(df, cols_to_sd, id_col = NULL) {
  cols <- df %>% select({{cols_to_sd}}) %>% colnames
  id_col <- df %>% select({{id_col}}) %>% colnames %>% first
  if(is.na(id_col) || is.null(id_col)) {
    id_col <- # first col not in passed list
      df %>%
      colnames %>%
      discard(\(colname) colname %in% cols) %>%
      first
  }

  # cols is a tidyselection
  compute_sd_if_in_cols_else_na <- function(col) {
    if (col %in% cols) {
      df %>% pull(col) %>% sd(na.rm = TRUE)
    } else if (col == id_col) {
      "sd"
    } else {
      NA
    }
  }
  sd_row <-
    colnames(df) %>%
    map(compute_sd_if_in_cols_else_na) %>%
    data.frame %>%
    setNames(colnames(df))

  add_row(df, sd_row)
}

rates_long_with_percents %>%
  append_sd_as_last_row(rate_of_error_type)

# todo rewrite w/o sd as column
# long_data <-
#   preprocessed_data %>%
#   reframe(
#     across(misproduction:correction, mean_and_sd, .unpack = TRUE)
#   ) %>%
#   pivot_mean_and_sd_longer()
# todo add sd by participant and sd by passage INTO THIS DF

# todo rewrite w/o sd as column
long_data_by_passage <-
  preprocessed_data %>%
  reframe(
    across(misproduction:correction, mean_and_sd, .unpack = TRUE),
    .by = passage
  ) %>% pivot_mean_and_sd_longer()

preprocessed_data %>%
  reframe(
    across(misproduction:correction,  # compute the mean for each error type
           \(.) mean(., na.rm = TRUE)),
    .by = passage) %>%
  pivot_longer(names_to = "error_type",
               values_to = "rate_of_error_type",
               cols = everything())

# todo rewrite w/o sd as column
long_data_by_participant <- # does sd mean anything meaningful here??
  preprocessed_data %>%
  reframe(
    across(misproduction:correction, mean_and_sd, .unpack = TRUE),
    .by = participant_id
  ) %>% pivot_mean_and_sd_longer()

print("Example: how many hesitations were made for each 11th grade passage?")
long_data_by_passage %>% filter(error_type == 'hesitation' & str_detect(passage, "11"))


# rates of each error type for each person
rates_by_participant <- preprocessed_data %>%
  group_by(participant_id) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(participant_id, ends_with("_rate"))


# better:
preprocessed_data %>%
  reframe(
    across(misproduction:correction, mean_and_sd, .unpack = TRUE),
    .by = participant_id
  )

preprocessed_data %>%
  reframe(
    across(misproduction:correction, mean_and_sd, .unpack = TRUE),
    .by = participant_id
  ) %>%
  pivot_mean_and_sd_longer(.to_percent = TRUE) %>%
  select(-sd)
view_last_if()

# "", sd
rates_by_participant %>%
  summarize(across(ends_with("_rate"),
                   \(x) sd(x, na.rm = TRUE),
                   .names = "{.col}_sd"))

# rates of each error type for each passage
rates_by_passage <- preprocessed_data %>%
  group_by(passage) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(passage, ends_with("_rate"))

# "", sd
rates_by_passage %>%
  summarize(across(ends_with("_rate"),
                   \(x) sd(x, na.rm = TRUE),
                   .names = "{.col}_sd"))


# join participant error data and counterbalance data
preprocessed_data_by_condition <- preprocessed_data %>%
  left_join(counterbalance_data, by = "participant_id") %>%
  mutate(
    which_passage_set = str_sub(category, -1, -1), # "a" or "b"
    alone =
      (which_passage_set == "a" &  set_a_alone) |
      (which_passage_set == "b" & !set_a_alone),
    social = !alone
  ) %>% select(colnames(preprocessed_data), social)



# rates of each error type by condition
rates_by_condition <- preprocessed_data_by_condition %>%
  group_by(social) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(social, ends_with("_rate"))

# "", sd
preprocessed_data_by_condition %>% # nb not working as intended: NAs still here
  group_by(social) %>%
  summarize(across(misproduction:correction,
                   \(x) sd(length(which(x)) / n(), na.rm = TRUE),
                   .names = "{.col}_sd"))

rates_by_participant_and_condition <- preprocessed_data_by_condition %>%
  group_by(participant_id, social) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(social, participant_id, ends_with("_rate"))

rates_by_passage_and_condition <- preprocessed_data_by_condition %>%
  group_by(passage, social) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(social, passage, ends_with("_rate"))


