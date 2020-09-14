# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

library(rstan)

#'Model average firing as a function treatment using log-poisson regression
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' extra ... arguments are passed to brms::brm(...)
#'
#' returns:
#'   brmsfit from the brms package
#'
#'Build a bayesian heirarchical model for the number of firings as a function of
#'
#'  exposure with a weight fixed at 1
#'    => offset(log(exposure))
#'    https://github.com/paul-buerkner/brms/issues/146
#'
#'  treatment as a fixed effect
#'    => treatment
#'
#'  average neuron firing rate as a random effet
#'    => (1|neuron_index)
#'
#'  assume a general linear model with log-poisson link
#'
#'This uses the brms package front end to the stan bayesian modeling framework
#'
#'@export
model_treatment_log_poisson <- function(
	experiment,
	output_base="intermediate_data/models",
	verbose=TRUE,
	...){

	exposure_counts <- experiment$firing %>%
		dplyr::group_by(neuron_index, treatment) %>%
		dplyr::summarize(
			count = dplyr::n(),
			exposure = end[1]-begin[1]) %>%
		dplyr::ungroup()


	if(verbose){
		cat("fitting log-poisson model for firing counts for experiment '", experiment$tag, "'\n", sep="")
	}

	if(!is.na(output_base)){
		model_path <- paste0(output_base, "/model_log_poisson_", experiment$tag, ".stan")
		cat("Saving stan model to '", model_path, "'\n", sep="")
	} else {
		model_path <- NULL
	}

	fit_log_poisson <- brms::brm(
		data=exposure_counts,
		formula=count ~ offset(log(exposure)) + treatment + (1|neuron_index),
		family=stats::poisson(link="log"),
		save_model=model_path,
		...)

	if(!is.null(output_base)){
		if(!dir.exists(output_base)){
			if(verbose){
				cat("creating output directory '", output_base, "'\n", sep="")
			}
			dir.create(
					output_base,
					showWarnings = FALSE,
					recursive = TRUE)
		}

		fit_path <- paste0(output_base, "/model_log_poisson_fit_", experiment$tag, ".Rdata")
		if(verbose){
			cat("Saving log-poisson model fit to '", fit_path, "'\n", sep="")
		}
		save(fit_log_poisson, file=fit_path)

		summary_path <- paste0(output_base, "/model_log_poisson_summary_", experiment$tag, ".txt")
		if(verbose){
			cat("Saving log-poisson model summary to '", summary_path, "'\n", sep="")
		}
		summary(fit_log_poisson) %>%
			capture.output(file=summary_path)
	}

	invisible(fit_log_poisson)
}
