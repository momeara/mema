# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

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
plot_model_marginal_effects <- function(
	model_fit,
	model_tag,
	plot_width=6,
	plot_height=6,
	output_base="product/plots",
	verbose=TRUE,
	...){

	marginal_effects <- brms::marginal_effects(model_fit, ...)

	p <- plot(
	  marginal_effects,
	  point_args=list(width=.2),
	  ask=FALSE)$treatment +
	  ggplot2::theme_bw() +
		ggplot2::ggtitle("Model Fit Marginal Effects", subtitle=model_tag)

	if(!is.null(output_base)){
		if(!dir.exists(output_base)){
			if(verbose){
				cat("creating output directory '", output_base, "'\n", sep="")
			}
			dir.create(
					output_base,
					showWarnings = FALSE,
					recusrive = TRUE)
		}

		pdf_path <- paste0(output_base, "/marginal_effects_", model_tag, "_", date_code(), ".pdf")
		if(verbose){
			cat("Saving marginal effects plot for model fit '", model_tag, "' to '", pdf_path, "'\n", sep="")
		}
		ggplot2::ggsave(pdf_path, width=plot_width, height=plot_height)

		png_path <- paste0(output_base, "/firing_rate_by_neuron_", model_tag, "_", date_code(), ".png")
		if(verbose){
			cat("Saving marginal effects plot for model fit '", model_tag, "' to '", png_path, "'\n", sep="")
		}
		ggplot2::ggsave(png_path, width=plot_width, height=plot_height)
	}

	invisible(p)
}
