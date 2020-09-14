#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(plyr)
library(dplyr)
library(shiny)

find_loaded_experiments <- function(){
    z <- tibble::tibble(
            path = list.files("/Users/momeara/Google Drive/Shoichet Lab Projects/Ro 25-6981 MOA/electrophysiology/Ro_vs_DA_experiment/intermediate_data/experiment_datasets", full.names=TRUE)) %>%
        dplyr::filter(path %>% stringr::str_detect(".Rdata$")) %>%
        dplyr::mutate(
            label = path %>% basename() %>% stringr::str_replace(".Rdata$", ""))
    split(z$path, z$label)
}
loaded_experiments <- find_loaded_experiments()

test_experiments <- list(
    "20190405_12h28m32s"=
    "/Users/momeara/Google Drive/Shoichet Lab Projects/Ro 25-6981 MOA/electrophysiology/Ro_vs_DA_experiment/intermediate_data/experiment_datasets/20190405_12h28m32s.Rdata")

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Curate Neuronal Units"),
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput(
                inputId="experiment_path",
                label="Experiment",
                choices=loaded_experiments),
            sliderInput("unit",
                        "Unit",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type="tabs",
                tabPanel("Waveform Lattice", plotOutput("plot_waveform_lattice", height="8in")),
                tabPanel("Waveform Correlation Matrix", plotOutput("plot_waveform_correlation_matrix", height="8in")),
                tabPanel("Firing Density By Neuron", plotOutput("plot_firing_density_by_neuron", height="8in")),
                tabPanel("Firing Rate By Neuron", plotOutput("plot_firing_rate_by_neuron", height="8in")),
                tabPanel("Firing Rate by Treatment", plotOutput("plot_firing_rate_by_treatment", height="8in")),
                tabPanel("Firing QQ-plot by Treatment", plotOutput("plot_firing_qqplot_by_treatment", height="8in")))
        )
    )
))
