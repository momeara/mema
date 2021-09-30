
library(tidyverse)
library(brms)


n_units <- 400
data <- dplyr::bind_rows(
  data.frame(condition = "baseline", exposure = 300, rate = .7, phi = 1.2),
  data.frame(condition = "treatment1", exposure = 200, rate = 1.5, phi = 1.7),
  data.frame(condition = "treatment2", exposure = 200, rate = 1.1, phi = 1.5),
  data.frame(condition = "TTX", exposure = 200, rate = .2, phi = 1.3)) %>%
  dplyr::rowwise() %>%
  dplyr::do({data.frame(., n_firings_per_unit = rnbinom(n_units, mu = .$rate * .$exposure, size = .$phi))}) %>%
  dplyr::ungroup()
  
rate_summary <- data %>% 
  dplyr::group_by(condition) %>%
  dplyr::summarize(
    mean_n_firings = mean(n_firings_per_unit),
    exposure = exposure[1],
    rate_est = mean(n_firings_per_unit) / exposure[1])

# data + means
base_plot <- ggplot2::ggplot(data = data) +
  ggplot2::ggtitle("Firing density by condition") +
  ggplot2::theme_bw() +
  ggplot2::geom_histogram(
    mapping = ggplot2::aes(
      x = n_firings_per_unit / exposure,
      y = ..ncount..),
    bins = 100,
    fill = "grey60") +
  ggplot2::geom_vline(
    data = rate_summary,
    mapping = ggplot2::aes(
      xintercept = rate_est,
      group = condition),
      color = "blue") +
  ggplot2::geom_rug(
    mapping = ggplot2::aes(
      x = n_firings_per_unit / exposure)) +
  ggplot2::facet_wrap(facets = dplyr::vars(condition), ncol = 1) +
  ggplot2::scale_x_continuous("Firing rate (seconds)", expand=c(.01, 0)) +
  ggplot2::scale_y_continuous("Scaled firing density")
base_plot

model <- brms::brm(
  formula = brms::brmsformula(
    n_firings_per_unit ~ 0 + condition),
  data = data)

rate_summary %>%
  dplyr::bind_cols(
    model %>%
      tidybayes::gather_draws(`b_.*`, regex=TRUE) %>%
      tidybayes::mean_qi()) %>%
  dplyr::mutate(
    model_rate = .value / exposure)

model_fits <-  dplyr::inner_join(
    model %>%
      tidybayes::gather_draws(`b_.*`, regex = TRUE) %>%
      dplyr::ungroup() %>%
      dplyr::rename(firing_mean = .value) %>%
      dplyr::mutate(condition = .variable %>% stringr::str_replace("b_condition", "")) %>%
      dplyr::select(-.variable),
    model %>%
      tidybayes::gather_draws(sigma) %>%
      dplyr::ungroup() %>%
      dplyr::rename(firing_sigma = .value) %>%
      dplyr::select(-.variable),
    by = c(".chain", ".iteration", ".draw")) %>%
  dplyr::left_join(
    rate_summary,
    by = "condition") %>%
  dplyr::group_by(condition) %>%
  dplyr::sample_n(30) %>%
  dplyr::ungroup() %>%
  dplyr::rowwise() %>%
  dplyr::do({
    params <- .
    data.frame(
      draw = params$.draw,
      condition = params$condition,
      rate_mean = params$firing_mean / params$exposure,
      rate_sigma = params$firing_sigma / params$exposure,
      rate = seq(0, 5, length.out=200)) %>%
      dplyr::mutate(
        density = dnorm(rate, mean = rate_mean, sd = rate_sigma),
        scaled_density = density / max(density))
  }) %>%
  dplyr::ungroup()


model_plot <- base_plot +
  ggplot2::ggtitle(
    label = "Firing density by condition",
    subtitle = "Gaussian fit") +
  ggplot2::geom_line(
    data = model_fits,
    mapping = ggplot2::aes(
      x = rate,
      y = scaled_density,
      group = draw),
    alpha = .3,
    color = "orange")
model_plot
    
model_negbinom_p <- brms::brm(
  formula = brms::brmsformula(
    n_firings_per_unit ~ 0 + condition + offset(log(exposure)),
    shape ~ 0 + condition),
  family = negbinomial("log"),
  prior = c(
    brms::prior(normal(0, 3), class="b"),
    brms::prior(normal(0, 1), dpar="shape")),
  data = data,
  cores = 4)

model_negbinom_p %>% MPStats::prior_posterior_plot()

params_summary <- dplyr::inner_join(
  model_negbinom_p %>%
    tidybayes::gather_draws(`b_condition.*`, regex = TRUE) %>%
    ggdist::mean_qi() %>%
    dplyr::transmute(
      condition = .variable %>% stringr::str_replace("b_condition", ""),
      mu = exp(.value),
      mu.lower = exp(.lower),
      mu.upper = exp(.upper)),
  model_negbinom_p %>%
    tidybayes::gather_draws(`b_shape_condition.*`, regex = TRUE) %>%
    ggdist::mean_qi() %>%
    dplyr::transmute(
      condition = .variable %>% stringr::str_replace("b_shape_condition", ""),
      shape = exp(.value),
      shape.lower = exp(.lower),
      shape.upper = exp(.upper)),
  by = c("condition"))
  


model_negbinom_fits <-  dplyr::inner_join(
  model_negbinom_p %>%
    tidybayes::gather_draws(`b_condition.*`, regex = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::rename(firing_mu = .value) %>%
    dplyr::mutate(condition = .variable %>% stringr::str_replace("b_condition", "")) %>%
    dplyr::select(-.variable),
  model_negbinom_p %>%
    tidybayes::gather_draws(`b_shape_condition.*`, regex = TRUE) %>%
    dplyr::ungroup() %>%
    dplyr::rename(firing_shape = .value) %>%
    dplyr::mutate(condition = .variable %>% stringr::str_replace("b_shape_condition", "")) %>%
    dplyr::select(-.variable),
  by = c(".chain", ".iteration", ".draw", "condition")) %>%
  dplyr::group_by(condition) %>%
  dplyr::sample_n(100, replace=FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(rate_summary %>% dplyr::select(condition, exposure), by = "condition") %>%
  dplyr::rowwise() %>%
  dplyr::do({
    params <- .
    z <- data.frame(
      draw = params$.draw,
      condition = params$condition,
      exposure = params$exposure,
      mu = exp(params$firing_mu) * params$exposure,
      phi = exp(params$firing_shape),
      rate = seq(0, 5, length.out=200)) %>%
      dplyr::mutate(
        firings = floor(rate * params$exposure),
        density = dnbinom(firings, mu = mu, size = phi),
        scaled_density = density / max(density))
  }) %>%
  dplyr::ungroup()



model_nbinom_plot <- base_plot +
  ggplot2::ggtitle(
    label = "Simulated firing density by condition (400 units)",
    subtitle = "Green: Gaussian, Blue: Negative-binomial") +
  ggplot2::geom_line(
    data = model_negbinom_fits,
    mapping = ggplot2::aes(
      x = rate,
      y = scaled_density,
      group = draw),
    alpha = .3,
    color = "darkblue") +
  ggplot2::geom_line(
    data = model_fits,
    mapping = ggplot2::aes(
      x = rate,
      y = scaled_density,
      group = draw),
    alpha = .3,
    color = "darkgreen") +
  ggplot2::facet_wrap(facets = dplyr::vars(condition), ncol=1)
model_nbinom_plot



