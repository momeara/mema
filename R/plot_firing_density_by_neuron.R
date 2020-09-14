# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#' Firing smoothed per-neuron firing density by neuron across the experiment
#'
#'  heat-map average firing rate
#'     x-axis experiment in seconds
#'     y-axis neuron index
#'     cell KDE smoothe estimate of the firing rate
#'
#'
#' experiment:
#'   experiment dataset loaded with mema::load_experiment(...)
#'
#' plot_width/plot_height:
#'   dimensions of the output plot
#'
#' returns:
#'   the ggplot2
#'   Saves the result to product/plots/firing_rate_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_firing_density_by_neuron <- function(
	experiment,
	plot_width=10,
	plot_height=4,
	output_base="product/plots",
	verbose=TRUE){


  data <- experiment$firing %>%
    dplyr::select(neuron_index, time_step) %>%
    plyr::ddply("neuron_index", function(data){
      density_estimate <- stats::density(
        x=data$time_step,
        from=experiment$treatments$begin %>% min,
        to=experiment$treatments$end %>% max,
        adjust=.01,
        n=1000)
      tibble::tibble(
        time_step=density_estimate$x,
        firing_density=density_estimate$y) %>%
        dplyr::mutate(
          normalized_log_firing_density=log(firing_density/max(firing_density)+1)
        )
    })

  p <- ggplot2::ggplot(data=data) +
    ggplot2::theme_bw() +
    ggplot2::geom_tile(
      mapping=ggplot2::aes(
        x=time_step,
        y=neuron_index,
        fill=normalized_log_firing_density)) +
    ggplot2::geom_rect(
      data= experiment$treatments,
      mapping=ggplot2::aes(xmin=begin, xmax=end, ymin=-Inf, ymax=Inf, group=treatment),
      color="white", alpha=0) +
    ggplot2::ggtitle("Per-neuron firing density across experiment", subtitle=experiment$tag) +
    ggplot2::scale_x_continuous(
      name="Seconds",
      breaks=experiment$treatments$begin,
      expand=c(0,0),
      sec.axis = ggplot2::dup_axis(
        name=NULL,
        breaks=with(experiment$treatments, begin + (end - begin)/2),
        labels=experiment$treatments$treatment)) +
    ggplot2::scale_y_continuous(
      name='Neuron Index',
      expand=c(0,0)) +
    ggplot2::scale_fill_viridis_c("Per-neuron normalized log firing density") +
    ggplot2::theme(legend.position="bottom") +
    ggplot2::theme()

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

		pdf_path <- paste0(output_base, "/firing_density_by_neuron_", experiment$tag, "_", date_code(), ".pdf")
		if(verbose){
			cat("Saving firing density by neuron plot for experiment '", experiment$tag, "' to '", pdf_path, "'\n", sep="")
		}
		ggplot2::ggsave(pdf_path, width=plot_width, height=plot_height)

		png_path <- paste0(output_base, "/firing_density_by_neuron_", experiment$tag, "_", date_code(), ".png")
		if(verbose){
			cat("Saving firing density by neuron plot for experiment '", experiment$tag, "' to '", png_path, "'\n", sep="")
		}
		ggplot2::ggsave(png_path, width=plot_width, height=plot_height)
	}

	invisible(p)
}
