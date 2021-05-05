# mema: Multi-Electrode Midbrain Analysis

This is a set of tools for analysing multi-electrode electrophysiology recordings
The package has been developed by the O'Meara lab at UMich and the Margolis Lab at UCSF.

## To install
From within R,

    install.packages("remotes")
    remotes::install_github("momeara/mema")

The `mema` package requires a working C++ complier. If you have issues
installing on windows due to Rtools errors, follow the
instructions here: https://cran.r-project.org/bin/windows/Rtools/

## Usage

From the output generated from the MED64 matlab analysis code e.g.

    <project_directory>/<experiment_tag>/units.<timestamp>.mat
    <project_directory>/<experiment_tag>/treatmentinfo.<timestamp>.csv

From within `R` load the `mema` package

	library(tidyverse)
    library(mema)

Load an experiment

	setwd("<project_directory>")
	experiment <- mema::load_experiment(
	    units_fname = "<experiment_tag>/units.<timestamp>.mat",
		treatments_fname = "<experiment_tag>/treatmentinfo.<timestamp>.csv",
		experiment_tag = "<experiment_tag")

