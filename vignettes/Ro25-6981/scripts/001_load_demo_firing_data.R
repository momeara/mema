
library(plyr)
library(dplyr)
library(readr)

library(R.matlab)
library(fuzzyjoin)

conditions_full <- readr::read_csv(
  file="raw_data/demo_firing_conditions_full_190513.csv",
  col_types=readr::cols(
    condition = readr::col_character(),
    begin = readr::col_double(),
    end = readr::col_double())) %>%
  dplyr::mutate(
    # this is to ensure the correct order of the conditions in plots etc.
    condition = factor(condition, labels=condition, levels=condition))

conditions_trimmed <- readr::read_csv(
  file="raw_data/demo_firing_conditions_trimmed_190513.csv",
  col_types=readr::cols(
    condition = readr::col_character(),
    begin = readr::col_double(),
    end = readr::col_double())) %>%
  dplyr::mutate(
    # this is to ensure the correct order of the conditions in plots etc.
    condition = factor(condition, labels=condition, levels=condition))


# for each neuron x
#    Unit(x).ts     array timestamps (in sec) when that neuron fired during the course of the experiment.
#    Unit(x).mWave  is average voltage waveform (action potential shape) corresponding to that neuron
#                   (the x scaling is 0.0001 sec between points, or a total of 6 ms for the waveform data)
raw_data <- R.matlab::readMat("raw_data/demo_firing_data_190513.mat")

firing_dim <- 1
waveform_dim <- 2
n_neurons <- dim(raw_data$Unit)[3]


firing <- raw_data$Unit[firing_dim,1,1:n_neurons] %>%
  purrr::imap_dfr(function(time_steps, neuron_index){
    tibble::tibble(
      neuron_index = neuron_index,
      time_step = as.numeric(time_steps))
  })

firing_full <- firing %>%
  fuzzy_inner_join(
    conditions_full,
    by=c("time_step"="begin", "time_step"="end"),
    match_fun = list(`>=`, `<`))
save(firing_full, file="intermediate_data/demo_firing_conditions_full.Rdata")

firing_trimmed <- firing %>%
  fuzzy_inner_join(
    conditions_trimmed,
    by=c("time_step"="begin", "time_step"="end"),
    match_fun = list(`>=`, `<`))
save(firing_trimmed, file="intermediate_data/demo_firing_conditions_trimmed.Rdata")



waveform <- raw_data$Unit[waveform_dim,1,1:n_neurons] %>%
  purrr::imap_dfr(function(voltages, neuron_index){
    tibble::tibble(
      neuron_index = neuron_index,
      time_step = 1:length(voltages),
      voltage = as.numeric(voltages))
  })
save(waveform, file="intermediate_data/demo_waveform.Rdata")

