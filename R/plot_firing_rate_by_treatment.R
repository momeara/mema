# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Dot/boxplot plot of average firing rate by treatment
#'
#'  Make a plot where
#'      the x-axis are the treatments and
#'      the y-axis is the average firing rate on the log scale
#'      show a box (and wisker) plot for each treatment summarizing across neurons in the experiment
#'      overlay points for each neuron jittering horizontally to allow them to be seen more easily
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/firing_rate_by_treatment_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_firing_rate_by_treatment <- function(
	experiment,
	plot_width=6,
	plot_height=6,
	output_base="product/plots",
	verbose=TRUE){

	exposure_counts <- experiment$firing %>%
		dplyr::group_by(neuron_index, treatment) %>%
		dplyr::summarize(
			count = dplyr::n(),
			exposure = end[1]-begin[1]) %>%
		dplyr::ungroup()

	p <- ggplot2::ggplot(data=exposure_counts) +
		ggplot2::theme_bw() +
		ggplot2::geom_boxplot(
			mapping=ggplot2::aes(x=treatment, y=count/exposure)) +
		ggplot2::geom_jitter(
			mapping=ggplot2::aes(x=treatment, y=count/exposure), width=.15, height=0) +
		ggplot2::ggtitle("Neuron Firing Rate by Condition", subtitle=experiment$tag) +
		ggplot2::scale_x_discrete("Treatment") +
		scale_y_log_firing_rate()

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

	  pdf_path <- paste0(output_base, "/firing_rate_by_treatment_", experiment$tag, "_", date_code(), ".pdf")
	  if(verbose){
	    cat("Saving firing_rate_by_treatment  plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
	  }
	  ggplot2::ggsave(pdf_path, width=plot_width, height=plot_height)

	  png_path <- paste0(output_base, "/firing_rate_by_treatment_", experiment$tag, "_", date_code(), ".png")
	  if(verbose){
	    cat("Saving firing_rate_by_treatment plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
	  }
	  ggplot2::ggsave(png_path, width=plot_width, height=plot_height)
	}

	invisible(p)
}
