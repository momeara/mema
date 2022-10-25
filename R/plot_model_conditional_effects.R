#' Plot conditional effects for model
#'
#' @param model \code{brmsfit} object
#' @param model_tag \code{character} tag for the model (Default: \code{NULL})
#' @param effects \code{character} main effects to plot (see \code{brms::conditional_effects})
#' @param plot_width \code{numeric} dimensions of the output plot width (Default: \code{6})
#' @param plot_height \code{numeric} dimensions of the output plot height (Default: \code{6})
#' @param output_base \code{character} path to saveplots (Default: \code{"product/plots"})
#' @param verbose \code{logical} verbose output (Default: FALSE)
#' @param ... additional arguments passed to \code{brms::conditional_effects}
#'
#' @return ggplot2 object
#'   Saves the result to product/plots/firing_rate_by_neuron_<experiment_tag>_<date_code>.(pdf|png)
#'   It save both .pdf and .png because it's easier to email etc small pngs
#'   while for use in an a manuscript having the vector version means that it can be tweaked with illustrator
#'
#'@export
plot_model_conditional_effects <- function(
	model,
	model_tag = NULL,
	effect = "treatment",
	plot_width = 6,
	plot_height = 6,
	output_base = "product/plots",
	verbose = FALSE,
	...) {

	conditional_effects <- brms::conditional_effects(
		x = model,
		effects = effects,
    ...)

	p <- plot(
	  conditional_effects,
	  point_args = list(width = .2),
	  ask = FALSE)[[effects]] +
	  ggplot2::theme_bw() +
		ggplot2::ggtitle("Model Fit Marginal Effects", subtitle = model_tag)

	if (!is.null(output_base)) {
		if (!dir.exists(output_base)) {
			if (verbose) {
				cat("creating output directory '", output_base, "'\n", sep = "")
			}
			dir.create(
					output_base,
					showWarnings = FALSE,
					recursive = TRUE)
		}

		pdf_path <- paste0(output_base, "/conditional_effects_", model_tag, "_", date_code(), ".pdf")
		if (verbose) {
			cat("Saving conditional effects plot for model fit '", model_tag, "' to '", pdf_path, "'\n", sep = "")
		}
		ggplot2::ggsave(pdf_path, width = plot_width, height = plot_height)

		png_path <- paste0(output_base, "/firing_rate_by_neuron_", model_tag, "_", date_code(), ".png")
		if (verbose) {
			cat("Saving conditional effects plot for model fit '", model_tag, "' to '", png_path, "'\n", sep = "")
		}
		ggplot2::ggsave(png_path, width = plot_width, height = plot_height)
	}

	invisible(p)
}
