# Luc Sahar -- NDCLab, Florida International University
# last updated 03/15/2024


# counterbalance_data <- data.frame(
#   participant_id = "todo",
#   alone_first = "todo",
#   set_a_first = "todo", # X is a, Y is b,
#   flanker_first = "todo"
# )
library(glue)
path_to_read_dataset = ('../read-study1-dataset')
preprocessed_data = paste(
  path_to_read_dataset,
  "derivatives",
  "disfluencies_subject-x-passage-x-word_20240315_0110am.csv",
  sep = '/'
) %>% read.csv %>% data.frame



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

counterbalance_data <-
  counterbalance_info %>%
  mutate(alone_first = substr(task_cb, start = 2, stop = 2) == "2",
         flanker_first = substr(task_cb, start = 1, stop = 1) == "A",
         set_a_first = list_cb == "X") %>%
  select(-task_cb, -list_cb)

# rates of each error type for each person
rates_by_participant <- preprocessed_data %>%
  group_by(participant_id) %>%
  summarize(across(misproduced_syllable:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(participant_id, ends_with("_rate"))

# "", sd
rates_by_participant %>%
  summarize(across(ends_with("_rate"),
                   \(x) sd(x, na.rm = TRUE),
                   .names = "{.col}_sd"))

# rates of each error type for each passage
rates_by_passage <- preprocessed_data %>%
  group_by(passage) %>%
  summarize(across(misproduced_syllable:correction,
                   \(x) length(which(x)) / n(),
                   .names = "{.col}_rate")) %>%
  select(passage, ends_with("_rate"))

# "", sd
rates_by_passage %>%
  summarize(across(ends_with("_rate"),
                   \(x) sd(x, na.rm = TRUE),
                   .names = "{.col}_sd"))




# compare
data.frame(psg_sd = rates_by_passage %>%
             summarize(across(ends_with("_rate"),
                              \(x) sd(x, na.rm = TRUE))) %>% unlist,
           participant_sd = rates_by_participant %>%
             summarize(across(ends_with("_rate"),
                              \(x) sd(x, na.rm = TRUE))) %>% unlist)

