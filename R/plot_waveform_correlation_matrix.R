# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' plot Correlation matrix of waveforms
#'
#'   Plot a correlation matrix as a heatmap between all waveforms in the experiment
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/waveform_correlation_matrix_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_waveform_correlation_matrix <- function(
	experiment,
	plot_width=10,
	plot_height=10,
	output_base="product/plots",
	verbose=TRUE){


	correlations <- experiment$waveform %>%
		reshape2::acast(time_step ~ neuron_index, value.var="voltage") %>%
		cor()

	d <- dist(correlations)
	o_row <- seriation::seriate(d, method="OLO", control=NULL)[[1]]
	args=list(
		trace="none",
		density.info="none",
		col=viridis::viridis(100),
		cexRow=1,
		cexCol=1,
		dendrogram="none",
		key=FALSE,
		keysize=0.03,
		x=correlations,
		Colv=as.dendrogram(o_row),
		Rowv=as.dendrogram(o_row))

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

	  pdf_path <- paste0(output_base, "/waveform_correlation_matrix_", experiment$tag, "_", date_code(), ".pdf")
	  if(verbose){
	    cat("Saving waveform_correlation_matrix plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
	  }
	  pdf(pdf_path, heigh=6, width=6)
	  do.call(gplots::heatmap.2,args=args)
	  dev.off()

	  png_path <- paste0(output_base, "/waveform_correlation_matrix_", experiment$tag, "_", date_code(), ".png")
	  if(verbose){
	    cat("Saving waveform_correlation_matrix plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
	  }
	  png(png_path, units="in", res=72, heigh=plot_height, width=plot_width)
	  do.call(gplots::heatmap.2,args=args)
	  dev.off()
	}
}
