# Real-world Exploration of Adolescent Development | Study 1
# Luc Sahar -- Neural Dynamics of Control Laboratory
# Florida International University

# Report on READ study 1 data by error type as a function of participant,
# passage, and condition.

# last updated 03/23/2024

library(glue)
library(dplyr)
library(rlang)
library(tidyr)
library(data.table)

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

preprocessed_data_with_pan_error_col <- # fix NAs, then get union of core errors
  preprocessed_data %>%
  mutate(across(misproduction:missing_prosodic_break, replace_na, FALSE)) %>%
  rowwise %>%
  mutate(
    any_error = any(cols = c_across(misproduction:missing_prosodic_break)),
    any_error_except_omission = # it's fine if omission is marked, but this is
           any(                 # true exactly when non-omission error is marked
             c_across(
               cols = c(misproduction:missing_prosodic_break,-omission)))) %>%
  ungroup


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

# dev utilities for interactive use
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

# first, we want a grand total, collapsing across participants and passages

percentize <- function(rate) {
  sprintf("%s%%", round(rate * 100, digits = 2))
}

percentize_multiple <- function(df, cols) {
  cols <- df %>% select({{cols}}) %>% colnames
  percents <-
    df %>%
    select(all_of(cols)) %>%
    map_df(percentize) %>%
    setNames(cols %>% paste0("_percent"))

  df %>%
    mutate(
      percents
    )
}

# add sd as last row
append_sd_as_last_row <- function(df, cols_to_sd, id_col = NULL) { # todo trim
  cols <- df %>% select({{cols_to_sd}}) %>% colnames

  if(missing(id_col) || is.na(id_col) || is.null({{id_col}})) {
    id_col <- # set it to the first col not in passed list
      df %>%
      colnames %>%
      discard(\(colname) colname %in% cols) %>%
      first
  } else {
    id_col <- df %>% select({{id_col}}) %>% colnames %>% first
  }

  # cols is a tidyselection
  compute_sd_if_in_cols_else_na <- function(col) {
    if (col == id_col) { # cell reflects that this row is the sd row
      "sd" # a label- other cells in this column might have "misprd", "hes", etc
    } else if (col %in% cols) { # or it's what we want to actually compute sd on
      df %>% pull(col) %>% sd(na.rm = TRUE)
    } else { # otherwise truly not applicable
      NA
    }
  }
  sd_row <-
    colnames(df) %>%
    map(compute_sd_if_in_cols_else_na) %>%
    data.frame %>%
    setNames(colnames(df))

  # since we want to put the literal "sd" in it, we need to make sure it's a
  # character column
  df %>%
    mutate(across({{id_col}}, as.character)) %>%
    add_row(sd_row)
}


rates_long <- # compute mean, i.e. rate of occurrence, by error type
  preprocessed_data_with_pan_error_col %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE))) %>%
  pivot_longer(names_to = "error_type",
               values_to = "rate_of_error_type",
               cols = everything())

# as percents:
rates_long_with_percents <-
  rates_long %>%
  mutate(rate_as_percent = percentize(rate_of_error_type)) %>%
  append_sd_as_last_row(rate_of_error_type)

long_data_by_passage <- # compute mean, i.e. rate of occurrence, by error type
  preprocessed_data_with_pan_error_col %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = passage) %>%
  percentize_multiple(where(is.numeric)) %>% # include as %s, for readability
  append_sd_as_last_row(where(is.numeric)) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  transpose(keep.names = "error_type", make.names = "passage") %>%
  as_tibble() # for printing/dev/interactive (this is what it was pre transpose)

print("Example: how many errors were made on passages, by grade?")
print("11th:")
long_data_by_passage %>%
  filter(error_type == "any_error_except_omission") %>%
  select(contains("11g")) %>%
  mutate(across(everything(), as.double)) %>%
  rowMeans()

print("9th:")
long_data_by_passage %>%
  filter(error_type == "any_error_except_omission") %>%
  select(contains("9g")) %>%
  mutate(across(everything(), as.double)) %>%
  rowMeans()


long_data_by_participant <- # rates of each error type for each person
  preprocessed_data_with_pan_error_col %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = participant_id) %>%
  percentize_multiple(c(where(is.numeric), -participant_id)) %>% # include as %s
  append_sd_as_last_row(c(where(is.numeric), -participant_id)) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  transpose(keep.names = "error_type", make.names = "participant_id") %>%
  as_tibble() # for printing/dev/interactive (this is what it was pre transpose)

# join participant error data and counterbalance data
preprocessed_data_by_condition <- preprocessed_data_with_pan_error_col %>%
  left_join(counterbalance_data, by = "participant_id") %>%
  mutate(
    which_passage_set = str_sub(category, -1, -1), # "a" or "b"
    alone =
      (which_passage_set == "a" &  set_a_alone) |
      (which_passage_set == "b" & !set_a_alone),
    social = !alone
  ) %>% select(colnames(preprocessed_data_with_pan_error_col), social)



# rates of each error type by condition- fixme per above
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


