# Real-world Exploration of Adolescent Development | Study 1
# Luc Sahar -- Neural Dynamics of Control Laboratory
# Florida International University

# Report on READ study 1 data by error type as a function of participant,
# passage, and condition. Write results to externally readable CSVs and XLSXes.
# Also track irregularities in stimuli, to monitor for potential future changes.

# last updated 04/02/2024

# library(glue)
library(dplyr)
# library(rlang)
library(tidyr)
library(stringr)
library(purrr)
library(data.table)
library(lubridate) # now

# flag: do we want to stop at each stop and view?
VIEW_MODE=FALSE

# this script does not need to be general to the extent that genScaffolds and
# preproc do
path_to_read_dataset <- ('../read-study1-dataset')
path_to_read_analysis <- ifelse(fs::is_dir('../read-study1')[[1]],
                                '../read-study1', '../read-study1-analysis')
path_to_derivatives <- paste(path_to_read_analysis, 'derivatives', sep = '/')

# info from preproc
path_to_stimuli <-
  paste(path_to_read_dataset, "materials/reading-ranger/stimuli", sep = '/')

path_to_resources <- paste(path_to_stimuli, 'resources', sep = '/')

path_to_passage_creation_data <-
  paste(path_to_resources, "passage-creation-order.csv", sep="/")



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

# track which was made first etc
passage_creation_data <- read.csv(path_to_passage_creation_data)

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

# Now, by condition
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


# rates of each error type by condition
long_data_by_condition <- preprocessed_data_by_condition %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = social) %>%
  percentize_multiple(where(is.numeric)) %>% # include as %s
  append_sd_as_last_row(where(is.numeric)) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  transpose(keep.names = "error_type", make.names = "social") %>%
  as_tibble() # for printing/dev/interactive (this is what it was pre transpose)

# both: 3200001_social, 3200001_alone, 3200002_social, 3200002_alone, ...
rates_by_participant_and_condition <-
  preprocessed_data_by_condition %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = c(participant_id, social)
  ) %>%
  percentize_multiple(c(where(is.numeric), -participant_id)) %>% # include as %s
  mutate(across(c(participant_id), as.character)) %>%
# append_sd_as_last_row(c(where(is.numeric))) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  mutate(id = paste0(participant_id, '_', ifelse(social, "social", "alone"))) %>%
  arrange(participant_id, social) %>%
  transpose(keep.names = "error_type", make.names = "id") %>%
  as_tibble()


# both: boots_11g_social, boots_11g_alone, oak_9g_social, oak_9g_alone, ...
rates_by_passage_and_condition <-
  preprocessed_data_by_condition %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = c(passage, social)
  ) %>%
  percentize_multiple(c(where(is.numeric))) %>% # include as %s
  # append_sd_as_last_row(c(where(is.numeric))) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability %>%
  mutate(id = paste0(passage, '_', ifelse(social, "social", "alone"))) %>%
  arrange(passage, social) %>%
  transpose(keep.names = "error_type", make.names = "id") %>%
  as_tibble()


# Now figure out creation order: does being the first one created for a set
# matter?

# split it longer:
# one row per passage
# original? -> T/F
# name
# and then split it longer _again_
# one row per passage per grade

creation_table <-
  passage_creation_data %>%
  pivot_longer(cols = c(original, derivative),
               names_to = "which_one", values_to = "passage") %>%
  mutate(original_passage = ifelse(which_one == "original",
                                   passage, lag(passage)),
         is_derived = which_one == 'derivative')

# - [x] Get them by grade level (one row per each)

# we just want to plot them

# now we want long_data_by_passage
passage_data_wider_with_passage_name <-
  long_data_by_passage %>%
  transpose(keep.names = 'passage+grade',
            make.names = 'error_type') %>%
  mutate(passage = str_extract(`passage+grade`, '[a-z]+')) %>%
  as_tibble()


preprocessed_data_by_creation_order <- preprocessed_data_with_pan_error_col %>%
  mutate(passage = str_extract(`passage`, '[a-z]+')) %>%
  left_join(creation_table, by = "passage")

long_data_by_creation_order <- preprocessed_data_by_creation_order %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = is_derived) %>%
  percentize_multiple(where(is.numeric)) %>% # include as %s
  append_sd_as_last_row(where(is.numeric)) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  transpose(keep.names = "error_type", make.names = "is_derived") %>%
  as_tibble() # for printing/dev/interactive (this is what it was pre transpose)

# given its name (X_11g, or X_9), i.e. `passage`, figure out is_derived

# Pretty printing:
print('Error types as percents according to passage creation order')
long_data_by_creation_order %>%
  filter(error_type %>% str_detect('percent')) %>%
  select(-sd) %>%
  rename('original_passages' = 'FALSE', 'derived_passages' = 'TRUE')

long_data_by_condition <- preprocessed_data_by_condition %>%
  reframe(
    across(misproduction:correction|any_error:any_error_except_omission,
           \(.) mean(., na.rm = TRUE)),
    .by = social) %>%
  percentize_multiple(where(is.numeric)) %>% # include as %s
  append_sd_as_last_row(where(is.numeric)) %>% # get our sd
  select(-where(is.numeric), where(is.numeric)) %>% # %s first, for readability
  transpose(keep.names = "error_type", make.names = "social") %>%
  as_tibble() # for printing/dev/interactive (this is what it was pre transpose)



# Now, generate externally accessible results (writing to filesystem)
results_and_nicknames <- # for both CSV and sheet outputs
  list(
    total = rates_long_with_percents,
    by_participant = long_data_by_participant,
    by_passage = long_data_by_passage,
    by_condition = long_data_by_condition,
    by_participant_and_condition = rates_by_participant_and_condition,
    by_passage_and_condition = rates_by_passage_and_condition,
    by_passage_creation_order = long_data_by_creation_order
  )

timestamp <- now("America/New_York") %>% format("%Y%m%d_%I%M%P")

# write our results to csv, xlsx
write_to_separate_csvs <- function(path, suffix = timestamp) {

  write_to_path <- function(df, nickname) {
    outpath <- paste0(path, '/', nickname, '_', suffix, '.csv')
    write.csv(df, file = outpath)
  }

  imap(results_and_nicknames, write_to_path)
}

write_to_xlsx_sheets <- function(path) {
  "todo"
}

out_dir <- paste(path_to_derivatives, 'preprocessing', timestamp, sep = '/')
fs::dir_create(out_dir)
# fs::is_dir(out_dir)
write_to_separate_csvs(out_dir)

# todo remove row numbers

# Now assess stimuli for outliers

# figure out word frequencies: repeats of weird words

all_our_words <- preprocessed_data %>%
  select(word_clean, wordFreq, passage, word_id) %>%
  arrange(passage) %>%
  unique %>%
  mutate(grade = as.numeric(str_extract(passage, '\\d+')))

how_many_psgs_have_this_word <- function(word, grade_level) {
  all_our_words %>%
    filter(word_clean == word & grade == grade_level) %>%
    pull(passage) %>%
    unique %>%
    length
}

count_11g_word <- new.env()
count_9g_word <- new.env()

memo_num_psgs_with_word <- function(word, grade_level) { # memoized
  stopifnot(grade_level == 11 || grade_level == 9)
  if (grade_level == 11) {
    env <- count_11g_word
  } else {
    env <- count_9g_word
  }
  result <- env[[word]]

  if(is.null(result)) {
  # print('had to look it up')
    result <- how_many_psgs_have_this_word(word, grade_level)
    env[[word]] <- result
  }

  return(result)
}


all_our_words_with_counts <- # NB has tons of duplicates; see logic below
  all_our_words %>%
  rowwise() %>%
  mutate(num_psgs_with_this_word =
           memo_num_psgs_with_word(word_clean, grade))



all_our_words_with_counts %>%
  filter(wordFreq < quantile(all_our_words$wordFreq) %>% nth(2)
         & num_psgs_with_this_word > 2) %>%
  select(-word_id) %>%
  arrange(wordFreq) %>%
  View


all_our_words_with_counts %>%
  filter(wordFreq < quantile(all_our_words$wordFreq) %>% nth(2)
         & num_psgs_with_this_word > 2) %>%
  select(-word_id) %>%
  arrange(wordFreq) %>%
  select(word_clean, grade, num_psgs_with_this_word) %>%
  unique %>%
  arrange(desc(num_psgs_with_this_word)) %>%
  write.csv('repeat-uncommon-words-by-grade.csv')



