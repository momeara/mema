# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Firing QQ-plot by treatment
#'
#'  Lattice plot for each treatment, where in each cell
#'      the x-axis is percent of the treatment (by time)
#'      the y-axis is percent of the total number of counts observed
#'      a line for each neuron in the experiment with
#'         the left endpoint anchored at the lower left
#'         the right endpoint anchored at the upper right
#'
#'  If the counts occure independently through time as is assumed in a poisson process model
#'  then for each neuron, the line be diagonal (following the guide blue line)
#'    If there is some lag to reach a stead state then the lines will look like they have a kink in it
#'    if there is bursty or irregular spacing, then the spead off the diagonal will be substantial but symmetric
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/firing_qqplot_by_treatment_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_firing_qqplot_by_treatment <- function(
	experiment,
	highlight_units=NULL,
	plot_width=7,
	plot_height=4,
	output_base="product/plots",
	verbose=TRUE){

	data <- experiment$firing %>%
		dplyr::group_by(neuron_index, treatment) %>%
		dplyr::arrange(time_step) %>%
		dplyr::mutate(cum_dist = dplyr::row_number()/dplyr::n()) %>%
		dplyr::ungroup()

	# add points to anchor begin and end 
	data <- dplyr::bind_rows(
			data %>%
					dplyr::distinct(neuron_index, treatment, .keep_all=TRUE) %>%
							dplyr::mutate(
									time_step = begin,
									cum_dist = 0),
			data,
			data %>%
					dplyr::distinct(neuron_index, treatment, .keep_all=TRUE) %>%
							dplyr::mutate(
									time_step = end,
									cum_dist = 1))



	p <- ggplot2::ggplot(data=data) +
		ggplot2::theme_bw() +
		ggplot2::geom_abline(
			mapping=ggplot2::aes(
				slope=1,
				intercept=0),
			color="blue",
			size=2) +
		ggplot2::geom_line(
			mapping=ggplot2::aes(
				x=(time_step-begin)/(end-begin),
				y=cum_dist,
				group=neuron_index),
			alpha=.8) +
		ggplot2::facet_wrap(~treatment) +
		ggplot2::ggtitle("QQ-plot of firing events over exposure", subtitle=experiment$tag) +
		ggplot2::scale_x_continuous("Percent exposure", labels=scales::percent) +
		ggplot2::scale_y_continuous("Percent counts observed", labels=scales::percent)

	if(!is.null(highlight_units)){
			p <- p +
				ggplot2::geom_line(
					data=data %>%
						dplyr::filter(neuron_index %in% highlight_units),
					mapping=ggplot2::aes(
						x=(time_step-begin)/(end-begin),
						y=cum_dist,
						group=neuron_index),
					color="red",
					size=2.5)
	}


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

	  pdf_path <- paste0(output_base, "/firing_qqplot_by_treatment_", experiment$tag, "_", date_code(), ".pdf")
	  if(verbose){
	    cat("Saving firing_qqplot_by_treatment  plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
	  }
	  ggplot2::ggsave(pdf_path, width=plot_width, height=plot_height)

	  png_path <- paste0(output_base, "/firing_qqplot_by_treatment_", experiment$tag, "_", date_code(), ".png")
	  if(verbose){
	    cat("Saving firing_qqplot_by_treatment plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
	  }
	  ggplot2::ggsave(png_path, width=plot_width, height=plot_height)
	}

	invisible(p)
}
