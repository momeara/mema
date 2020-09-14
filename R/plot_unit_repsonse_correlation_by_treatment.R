# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Unit Response Correlation by Treatment
#'
#'  Grid of treatment vs. treatment plots, in each cell
#'      scatter plot for each neuronal unit
#'        the x-axis is average firing rate for treatment of column
#'        the y-axis is average firing rate for treatment of row
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' highlight_units:
#'   which units to highlight in the plot
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/unit_response_correlation_by_treatment_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_unit_response_by_treatment <- function(
	experiment,
	highlight_units=NULL,
	plot_width=10,
	plot_height=10,
	output_base="product/plots",
	verbose=TRUE){

	exposure_counts <- experiment$firing %>%
		dplyr::group_by(neuron_index, treatment) %>%
		dplyr::summarize(
			log_firing_rate = log(dplyr::n() / (end[1]-begin[1]))) %>%
		dplyr::ungroup() %>%
		tidyr::spread(key="treatment", value="log_firing_rate", fill=0) %>%
		dplyr::select(-neuron_index)

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

		p <- GGally::ggpairs(
			data=exposure_counts,
			title=paste0("Correlation of unit response by treatment: ", experiment$tag),
			xlab="Log(Firing Rate)",
			ylab="Log(Firing Rate)")

	  pdf_path <- paste0(output_base, "/plot_unit_response_by_treatment_", experiment$tag, "_", date_code(), ".pdf")
	  if(verbose){
	    cat("Saving plot_unit_response plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
	  }
		pdf(pdf_path, height=plot_height, width=plot_width)
		print(p)
		dev.off()

	  png_path <- paste0(output_base, "/plot_unit_response_by_treatment_", experiment$tag, "_", date_code(), ".png")
	  if(verbose){
	    cat("Saving plot_unit_response_by_treatment plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
	  }
		png(png_path, units="in", res=72, height=plot_height, width=plot_width)
		print(p)
		dev.off()

	}

}
