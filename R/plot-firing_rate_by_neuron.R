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
#' firing:
#'   data.frame with columns [neuron_index, timestamp, treatment]
#'
#' experiment_tag:
#'   identifier for the experiment used in the figure subtitle and output filename
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/figures/firing_rate_by_neuron_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
firing_rate_by_neuron <- function(
	firing,
	experiment_tag,
	plot_width=7,
	plot_height=4,
	output_base="product/figures",
	verbose=TRUE){

	total_exposure <- firing %>%
		dplyr::distinct(treatment) %>%
		dplry::mutate(exposure = end[1] - begin[1]) %>%
		dplyr::summarize(total_exposure=sum(exposure)) %>%
		magrittr::extract2("total_exposure")

	data <- firing %>%
		dplyr::group_by(neuron_index) %>%
		dplyr::summarize(mean_firing_rate = n() / total_exposure)

	ggplot2::ggplot(data=data) +
		ggplot2::theme_bw() +
		ggplot2::geom_histogram(
			aes(x=log(mean_firing_rate)),
			bins=30) +
		ggplot2::ggtitle("Per-neuron firing rate", subtitle=experiment_tag) +
		ggplot2::scale_x_continuous(
			"Log(Firing / second)")

	pdf_path <- paste0(output_base, "/firing_rate_by_neuron_", experiment_tag, "_", date_code(), ".pdf")
	if(verbose){
		cat("Saving firing_rate_by_neuron  plot for experiment '", experiment_tag, "' to '", pdf_path, "'\n", sep="")
	}
	ggplot2::ggsave(pdf_path, width=10, height=10)

	png_path <- paste0(output_base, "/firing_rate_by_neuron_", experiment_tag, "_", date_code(), ".png")
	if(verbose){
		cat("Saving firing_rate_by_neuron plot for experiment '", experiment_tag, "' to '", png_path, "'\n", sep="")
	}
	ggplot2::ggsave(png_path, width=plot_width, height=plot_height)

	p
}
