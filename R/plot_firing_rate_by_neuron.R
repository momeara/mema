# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Firing rate by neuron
#'
#'  histogram of average firing rate
#'     x-axis firing rate on the log-scale
#'     y-axis number of neurons in the histogram bin
#'
#'  if there is spread in the average firing rate then this variation should be
#'  accounted for in a model of effects of the treatment on the average firing rate
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/firing_rate_by_neuron_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_firing_rate_by_neuron <- function(
	experiment,
	plot_width=7,
	plot_height=4,
	output_base="product/plots",
	verbose=TRUE){

	total_exposure <- experiment$treatment %>%
		dplyr::mutate(exposure = end[1] - begin[1]) %>%
		dplyr::summarize(total_exposure=sum(exposure)) %>%
		magrittr::extract2("total_exposure")

	data <- experiment$firing %>%
		dplyr::group_by(neuron_index) %>%
		dplyr::summarize(mean_firing_rate = dplyr::n() / total_exposure)

	p <- ggplot2::ggplot(data=data) +
		ggplot2::theme_bw() +
		ggplot2::geom_histogram(
			mapping=ggplot2::aes(x=log(mean_firing_rate)),
			bins=30) +
		ggplot2::ggtitle("Per-neuron firing rate", subtitle=experiment$tag) +
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

		pdf_path <- paste0(output_base, "/firing_rate_by_neuron_", experiment$tag, "_", date_code(), ".pdf")
		if(verbose){
			cat("Saving firing_rate_by_neuron  plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
		}
		ggplot2::ggsave(pdf_path, width=plot_width, height=plot_height)

		png_path <- paste0(output_base, "/firing_rate_by_neuron_", experiment$tag, "_", date_code(), ".png")
		if(verbose){
			cat("Saving firing_rate_by_neuron plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
		}
		ggplot2::ggsave(png_path, width=plot_width, height=plot_height)
	}

	invisible(p)
}
