# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Simple lattice plot of the waveforms
#'
#'   Plot a grid of figures, with x-axis in microseconds and y-axis in voltage
#'   One for each waveform in the waveform data.frame
#'
#' waveform:
#'   data.frame with columns [neuron_index, timestamp, voltage]
#'
#' experiment_tag:
#'   identifier for the experiment used in the figure subtitle and output filename
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/figures/waveform_lattice_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
waveform_lattice <- function(
	waveform,
	experiment_tag,
	plot_width=10,
	plot_height=10,
	output_base="product/figures",
	verbose=TRUE){

	p <- ggplot2::ggplot(data=waveform) +
		ggplot2::theme_bw() +
		ggplot2::geom_line(mapping=aes(x=time_step, y=voltage)) +
		ggplot2::facet_wrap(~neuron_index) +
		ggplot2::ggtitle("Neuron waveform cluster mean", subtitle=experiment_tag) +
		ggplot2::scale_x_continuous("microsecond") +
		ggplot2::scale_y_continuous("Voltage")

	pdf_path <- paste0(output_base, "/waveform_lattice_", experiment_tag, "_", date_code(), ".pdf")
	if(verbose){
		cat("Saving waveform_lattice plot for experiment '", experiment_tag, "' to '", pdf_path, "'\n", sep="")
	}
	ggplot2::ggsave(pdf_path, width=10, height=10)

	png_path <- paste0(output_base, "/waveform_lattice_", experiment_tag, "_", date_code(), ".png")
	if(verbose){
		cat("Saving waveform_lattice plot for experiment '", experiment_tag, "' to '", png_path, "'\n", sep="")
	}
	ggplot2::ggsave(png_path, width=plot_width, height=plot_height)

	p
}
