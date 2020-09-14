#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mema)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$plot_waveform_lattice <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_waveform_lattice(output_base=NULL)
        p
    })
    output$plot_waveform_correlation_matrix <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_waveform_correlation_matrix(output_base=NULL)
        p
    })

    output$plot_firing_density_by_neuron <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_firing_density_by_neuron(output_base=NULL)
        p
    })

    output$plot_firing_rate_by_neuron <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_firing_rate_by_neuron(output_base=NULL)
        p
    })
    output$plot_firing_rate_by_treatment <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_firing_rate_by_treatment(output_base=NULL)
        p
    })
    output$plot_firing_qqplot_by_treatment <- renderPlot({
        load(input$experiment_path)
        p <- experiment %>% mema::plot_firing_qqplot_by_treatment(
            highlight_units=input$unit,
            output_base=NULL)
        p
    })

})
