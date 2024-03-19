# Real-world Exploration of Adolescent Development | Study 1
# Luc Sahar -- Neural Dynamics of Control Laboratory
# Florida International University

# Report on READ study 1 data by error type as a function of participant,
# passage, and condition.

# last updated 03/19/2024

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

apply_fn_to_cols_if_present <- function(df, fn, ...) {
  colstrings <- enquos(...)
  col_vec <- map_vec(colstrings, as_label)
  df %>% mutate(across(any_of(col_vec), fn))
}

convert_cols_to_string_if_present <- function(df, ...) {
  apply_fn_to_cols_if_present(df, as.character, ...)
}

create_dummy_name <- function(df, .start = "_") {
# Find the first dummy column name that won't overwrite our data
  if (!(.start %in% colnames(df))) {
    .start
  } else {
    create_dummy_name(df, .start = paste('_', .start, sep = ''))
  }
}

error_rates_as_percents <- function(df) { # -> human readable output at a glance
  dummy <- create_dummy_name(df)
  df %>%
    ungroup %>%
    apply_fn_to_cols_if_present(as.character, participant_id) %>% # don't want id as %
    mutate({{dummy}} := NA, .before = 1) %>%
    adorn_pct_formatting %>%
    select(-{{dummy}}) %>%
    apply_fn_to_cols_if_present(as.double, participant_id)
  # adorn_pct_formatting will automatically ignore the first column, which we
  # don't necessarily want it to do. There is no way to override this except to
  # explicitly name which columns to turn into percents--exactly what we DON'T
  # want here; we want something general s.t. we can pass any df we end up
  # creating later to this function and it will "just work". Therefore we create
  # a dummy column, for adorn_pct_formatting to ignore as it wants to, then drop

}

# rates_by_participant_and_condition %>%
#   ungroup %>%
#   mutate(across(participant_id, as.character)) %>%
#   adorn_pct_formatting()

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
view_if(counterbalance_data)

# grand total
error_rates <- preprocessed_data %>%
  summarize(across(misproduction:correction,
                   \(x) mean(x, na.rm = TRUE),
                   .names = "{.col}_rate")) %>%
  select(ends_with("_rate")) %>%
  error_rates_as_percents


view_if(error_rates)

mean_and_sd <- function(x) {
  data.frame(rate = mean(x, na.rm = TRUE),
             sd   = sd(x, na.rm = TRUE))
}

# BINGO
preprocessed_data %>%
  reframe(
    across(misproduction:correction, mean_and_sd, .unpack = TRUE)
  ) %>%
  pivot_longer(
    names_to = c("error_type", ".value"),
    names_pattern = "(.*)_(sd|rate)",
    cols = c(ends_with("_sd"), ends_with("_rate"))
  )
view_last_if()

compute_statistics_longer <- function(df) {

}

# preprocessed_data %>%
#   reframe(across(misproduction:correction,
#                  rate = \(col) mean(col, na.rm = TRUE),
#                  sd   = \(col) sd(col, na.rm = TRUE))) %>%
#   pivot_longer(cols = misproduction:correction,
#                names_to = "error_type",
#                values_to = "rate_of_occurrence") %>%
#   mutate()
# preprocessed_data %>%
#   reframe(across(misproduction:correction,
#                  mean_and_sd,
#                  .unpack = TRUE,
#                  )) %>%
#   pivot_longer(cols = misproduction:correction,
#                names_to = "error_type",
#                values_to = "rate_of_occurrence") %>%
#   mutate()


# "", sd
# preprocessed_data %>%
#   summarize(across(misproduction:correction,
#                    \(x) sd(x, na.rm = TRUE),
#                    .names = "{.col}_rate")) %>%
#   select(ends_with("_rate"))


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
  pivot_longer(
    names_to = c(".value", "error_type"),
    names_pattern = "(.*)_(sd|rate)",
    cols = c(ends_with("_sd"), ends_with("_rate"))
  )
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

# sd
# rates_by_participant_and_condition %>%
#   summarize(across(ends_with("_rate"),
#                    \(x) sd(x, na.rm = TRUE),
#                    .names = "{.col}_sd"))

rates_by_passage_and_condition <- preprocessed_data_by_condition %>%
  group_by(passage, social) %>%
  summarize(across(misproduction:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(social, passage, ends_with("_rate"))


# sd
# rates_by_passage_and_condition %>%
#   summarize(across(ends_with("_rate"),
#                    \(x) sd(x, na.rm = TRUE),
#                    .names = "{.col}_sd"))
