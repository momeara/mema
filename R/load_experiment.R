# -*- tab-width:2;indent-tabs-mode:t;show-trailing-whitespace:t;rm-trailing-spaces:t -*-
# vi: set ts=2 noet:

#'Load firing data for an experiment
#'
#' units_fname:
#'   a file should be a .mat file exported from
#'   the Margolis lab multi-electrod expirmental matlab processing pipeline
#'   containing the following information for each neuron that was detected
#'
#'     1) the time steps (in seconds) when it fired during the experiment
#'     2) the waveform of the cluster center across all firings
#'
#' treatments:
#'   Either 
#'     a data.frame: with columns [treatment, begin, end] for each treatment in the experiment
#'   or:
#'      a .csv file with columns [treatment, begin, end] for each treatment in the experiment
#'
#'   to help detect problems, an warning is given if the treatments are not disjoint and given chronologically
#'
#' experiment_tag:
#'   identifier for the experiment, set in the return data structure and path to save to disk
#'   if null (default), then use treatments %>% basename %>% str_replace(".mat$", "")
#'
#' save_path:
#'    save dataset to <save_path>/<experiment_tag>
#'    Default value: intermediate_data/experiment_datasets
#'
#' returns:
#'    a mema_experiment S3 class with the following elements
#'      tag: <experiment_tag>
#'      treatments: tibble::tibble with columns [treatment, begin, end]
#'      firings: tibble::tibble with columns [nuron_index, time_step, treatment, begin, end] and a row for each detected firing
#'      waveform: tibble::tibble with columns [neuron_index, time_step, voltage] for each neuron
#'      class attribute: mema_experiment
#'@export
load_experiment <- function(
	units_fname,
	treatments,
	experiment_tag=NULL,
	save_path = "intermediate_data/experiment_datasets",
	verbose=TRUE) {
  
  # If requesting to save the dataset exists, make sure it does before trying to read it in.
  if(!is.null(save_path)) {
    if (!dir.exists(save_path)){
      if (verbose){
        cat("Creating save path '", save_path, "' ...\n", sep = "")
      }
      dir.create(save_path)
    }
  }
  
  
	### LOAD TREATMENTS
	if(!is.null(treatments)){
	  if(class(treatments) == "character"){
	    
  		if(verbose){
  			cat("Loading treatment schedule from '", treatments, "' ... ", sep="")
  		}
  
  		if(!stringr::str_detect(treatments, ".csv$")){
  			cat("WARNING: treatments='", treatments, "' should have '.csv' extension.\n", sep="")
  		}
  
  		treatments <- readr::read_csv(
  			file=treatments,
  			col_types=readr::cols(
  			  treatment = readr::col_character(),
  			  begin = readr::col_double(),
  			  end = readr::col_double())) %>%
  			dplyr::transmute(
  				# this is to ensure the correct order of the conditions in plots etc.
  				treatment = factor(treatment, labels=treatment, levels=treatment),
  				begin,
  				end)
	  }
	  
		if(verbose){
			cat("found '", nrow(treatments), "' treatments\n", sep="")
		}

		# check each treatment is well formed
		for(i in 1:nrow(treatments)){
			if(treatments$begin[i] >= treatments$end[i]){
				stop(paste0(
					"treatment '", i, "'='", treatments$treatment[i], "'",
					"has begin='", treatments$begin[i], "' >= end='", treatments$end[i], "'"))
			}
		}

		# check treatments are chronological
		for(i in 1:(nrow(treatments)-1)){
			if(treatments$end[i] > treatments$begin[i+1]){
				stop(paste0(
					"Treatments are out of chronological order:\n",
					"  treatment ", i, "='", treatments$treatment[i], "' with begin='", treatments$begin[i], "', end='", treatments$end[i], "'\n",
					"  treatment ", i+1, "='", treatments$treatment[i+1], "' with begin='", treatments$begin[i+1], "', end='", treatments$end[i+1],"'"))
			}
		}
	}

	if(verbose){
		cat("Reading in units from file '", units_fname, "' ... ", sep="")
	}

	if(!stringr::str_detect(units_fname, ".mat$")){
		cat("WARNING: units_fname='", units_fname, "' should have extension .mat\n", sep="")
	}

	raw_data <- R.matlab::readMat(units_fname)


	### LOAD UNITS
	firing_dim <- 1
	waveform_dim <- 2
	n_neurons <- dim(raw_data$Unit)[3]

	if(verbose){
		cat(" found firing data for '", n_neurons, "' neurons\n", sep="")
	}

	firing <- raw_data$Unit[firing_dim,1,1:n_neurons] %>%
		purrr::imap_dfr(function(time_steps, neuron_index){
			tibble::tibble(
				neuron_index = neuron_index,
				time_step = as.numeric(time_steps))
		})

	if(!is.na(treatments)){
		firing <- firing %>%
			fuzzyjoin::fuzzy_inner_join(
				treatments,
				by=c("time_step"="begin", "time_step"="end"),
				match_fun = list(`>=`, `<`))
	}

	### LOAD WAVEFORM
	waveform <- raw_data$Unit[waveform_dim,1,1:n_neurons] %>%
		purrr::imap_dfr(function(voltages, neuron_index){
			tibble::tibble(
				neuron_index = neuron_index,
				time_step = 1:length(voltages),
				voltage = as.numeric(voltages))
		})

	if(is.null(experiment_tag)){
		experiment_tag <- units_fname %>% basename %>% stringr::str_replace(".mat$", "")
	}

	experiment <- list(
	    tag=experiment_tag,
	    treatments=treatments,
	    firing=firing,
	    waveform=waveform) %>%
	  structure(class="mema_experiment")

	if (!is.null(save_path)){
	  if (!dir.exists(save_path)){
	    if (verbose){
	      cat("Creating save path '", save_path, "' ...\n", sep = "")
	    }
	    dir.create(save_path)
	  }
		path <- paste0(save_path, "/", experiment_tag, ".Rdata")
		if(verbose){
			cat("Saving experiment data to '", path, "'\n", sep="")
		}
		save(experiment, file=path)
	}

	invisible(experiment)
}
