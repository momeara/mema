# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

library(plyr)
library(dplyr)
library(brms)
library(loo)
library(ggplot2)

load("intermediate_data/demo_firing_conditions_trimmed.Rdata")

exposure_counts <- firing_trimmed %>%
  dplyr::group_by(neuron_index, condition) %>%
  dplyr::summarize(
    count = n(),
    exposure = end[1]-begin[1]) %>%
  dplyr::ungroup()


#Not sure why we're having trouble sampling from the model
#try adding a pseudo-count to each neuron:treatment
#
# n_neurons <- experiment$waveform$neuron_index %>% max 
# exposure_counts <- experiment$treatments %>%
#   purrr::pmap_dfr(function(treatment, begin, end){
#     tibble::tibble(
#       treatment=treatment,
#       neuron_index=1:n_neurons,
#       pad_count=1,
#       exposure=end-begin)
#   }) %>%
#   dplyr::left_join(
#     experiment$firing %>%
#       dplyr::group_by(neuron_index, treatment) %>%
#       dplyr::summarize(
#         count = dplyr::n()) %>%
#       dplyr::ungroup(),
#     by=c("neuron_index", "treatment")) %>%
#   dplyr::transmute(
#     treatment=treatment,
#     neuron_index=neuron_index,
#     count=ifelse(is.na(count), 0, count) + pad_count,
#     exposure=exposure)


# fit_poisson
# Including exposure as a coefficient is not right...
# https://github.com/paul-buerkner/brms/issues/146 suggests adding offset(log(n))
#
# Attempt at trying to model the fact that individual neurons may behave differently, yet consistently across conditions
fit_poisson <- brms::brm(
  data=exposure_counts %>% dplyr::filter(condition != "wash3"),
  formula=count ~ offset(log(exposure)) + condition + (1|neuron_index),
  family=poisson("log"))

summary(fit_poisson)
summary(fit_poisson) %>%
  capture.output(file="product/demo_fit_poisson_summary_190514.txt")
brms::posterior_summary(fit_poisson)
brms::marginal_effects(fit_poisson)
ggplot2::ggsave("product/demo_fit_poisson_marginal_effects_190514.pdf", width=6, height=6)
ggplot2::ggsave("product/demo_fit_poisson_marginal_effects_190514.png", width=6, height=6)
brms::LOO(fit_poisson)

hypothesis_tests01 <- brms::hypothesis(
  x=fit_poisson,
  hypothesis=c(
    "conditionDA = conditionwash1",
    "conditionwash1 = conditionRo100nM",
    "conditionwash1 = conditionRo1uM",
    "conditionwash1 = conditionRoPDA",
    "conditionwash1 = conditionwash2",
    "conditionRo100nM = conditionRo1uM",
    "conditionRo1uM = conditionRoPDA",
    "conditionRoPDA = conditionwash2"),
  alpha=0.01)


