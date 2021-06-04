# Load packages ----
library(shiny)
library(viridis)
library(ggplot2)


# Source helpers ----
source("R/read_echem.R")
source("R/plot_echem.R")
source("R/plot_dQdV.R")
source("R/plot_dQdV_stack_discharge.R")
source("R/plot_dQdV_stack_charge.R")
source("R/plot_cycles.R")
source("R/plot_CE.R")
source("R/plot_energy.R")
source("R/plot_voltage.R")
source('R/movingaverage.R')

# files.sources = file.path('R',list.files("R/"))
# sapply(files.sources, source)


# User interface ----
ui <- fluidPage(

    # Application title
    titlePanel("MaccorPloto: Visualize Maccor Battery Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            # Input: Select a file ----
            fileInput("file1",
                      "Choose ASCII File",
                      multiple = FALSE,
                      accept = c("asc")
                     ),

            numericInput(inputId ="mass",
                         "Active sample mass (mg)",
                         value = 5.0),

            # Horizontal line ----
            tags$hr(),

            # Plot details ----
            sliderInput(inputId ="cycleno",
                        label = "Cycle number:",
                        min = 1,max = 100,
                        step = 1, animate=TRUE,
                        value = 1),

            sliderInput(inputId ="max_capacity",
                        label = "Max. Capacity to plot:",
                        min = 100,max = 500,
                        step = 50,
                        value = 250),

            sliderInput("CE_range",
                        label = "Range of Coulombic Efficiency:",
                        min = 0, max = 100, value = c(0, 100)
                        ),

            strong("Save the Charge/Discharge Figure"),
            downloadButton("echem_figure", "Download"),


            # Horizontal line ----
            tags$hr(),
            sliderInput("increment",
                        label = "Range of Increment in dQ/dV Stack:",
                        min = 0, max = 50, value = 5),
            strong("Save the dQdV Stack Figure"),
            downloadButton("dQdV_figure", "Download"),

            tags$hr(),
            sliderInput("energy",
                        label = "Max. Specific Energy to Plot",
                        min = 0, max = 1500, step = 100,value = 800),
            sliderInput("hysteresis",
                        label = "Range of Voltage Hysteresis",
                        min = 0, max = 1.5, step = 0.1,value = c(0.3,1.5)),

            strong("Save the Energy/Hysteresis Figure"),
            downloadButton("energy_figure", "Download"),

            tags$hr(),
            selectInput("dataset","Choose a dataset:",
                        choice = c("Charge_Discharge","Cycle","Energy")),
            downloadButton("downloadData", "Download")
        ),

        #_________________________________________
        # Show a plot of the generated distribution
        mainPanel(

          tabsetPanel(
            tabPanel("Charge/Discharge",
                      fluidRow(
                         column(width = 6,  plotOutput("dis_charge_plot")),
                         column(width = 6,   plotOutput("plot_dQdV")),
                       ),
                       fluidRow(
                         column(width = 6,  plotOutput("cycles_plot")),
                         column(width = 6,   plotOutput("plot_CE")),
                       )
                     ),

            tabPanel("dQ/dV Stack",
                     fluidRow(
                         column(width = 10,  plotOutput("plot_dQdV_stack_charge")),
                         column(width = 10,   plotOutput("plot_dQdV_stack_discharge")),
                        )
                     ),

            tabPanel("Energy/Hysteresis",
                         fluidRow(
                             column(width = 10,  plotOutput("plot_energy")),
                             column(width = 10,   plotOutput("plot_voltage")),
                         )
                     ),

            tabPanel("Cycle",
                     tableOutput("cycles")
                     ),

            tabPanel("Energy",
                     tableOutput("energy")
            )

          ),

        )
    )
)

# Increase upload file limit to 30 MB
options(shiny.maxRequestSize=30*1024^2)

# Define server logic required to draw a histogram
server <- function(input, output) {

    dataInput <- reactive({
        req(input$file1,input$mass)
        tryCatch(
            {
                read_echem(input$file1$datapath,
                                    input$mass)
            },
            error = function(e) {
                # return a safeError if a parsing error occurs
                stop(safeError(e))
            }
        )
    })


    #_____________________
    # Discharge charge curves
    output$dis_charge_plot <- renderPlot({
        req(input$cycleno)
        plot_echem(dataInput(),input$cycleno,input$max_capacity)
    })

    # dQ/dV plot
    output$plot_dQdV <- renderPlot({
        plot_dQdV(dataInput(),input$cycleno)
    })

    # cycling
    output$cycles_plot <- renderPlot({
        plot_cycles(dataInput(),input$max_capacity)
    })

    # CE
    output$plot_CE <- renderPlot({
        plot_CE(dataInput(),input$CE_range[1],input$CE_range[2])
    })

    #_______________
    # dQdV stack
    output$plot_dQdV_stack_charge <- renderPlot({
        plot_dQdV_stack_charge(dataInput(),input$cycleno,
                               increment = input$increment, height = 150)
    })
    output$plot_dQdV_stack_discharge <- renderPlot({
        plot_dQdV_stack_discharge(dataInput(),input$cycleno,
                                  increment = input$increment, height = 150)
    })

    #_________________
    # energy/power and voltage
    output$plot_energy <- renderPlot({
        plot_energy(dataInput(),ymax=input$energy)
    })

    output$plot_voltage <- renderPlot({
        plot_voltage(dataInput(),
                     ymin=input$hysteresis[1],ymax=input$hysteresis[2])
    })

    #_____________________
    # Details of each cycles
    output$cycles <- renderTable({
        dataInput()$df_cycle
    })

    # Details of each cycles
    output$energy <- renderTable({
        dataInput()$df_energy
    })

    #_______________
    # Downloadable csv of selected dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Charge_Discharge" = dataInput()$df_echem,
               "Cycle" = dataInput()$df_cycle,
               "Energy" = dataInput()$df_energy)
    })

    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$file1,"_", input$dataset,".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    #_______________
    # Download processed figures
    output$echem_figure <- downloadHandler(
        filename = function(){
            paste(input$file1,"_echem.pdf", sep = "")
        },
        content = function(file){
            pdf(file,width = 7, height = 6)
            par(mfrow=c(2,2))
            par(cex=0.8)
            par(mar=c(5,5,1,1))

            print(plot_echem(dataInput(),input$cycleno,input$max_capacity))
            print(plot_dQdV(dataInput(),input$cycleno))
            print(plot_cycles(dataInput(),input$max_capacity))
            print(plot_CE(dataInput(),input$CE_range[1],input$CE_range[2]))
            dev.off()
        }
    )

    # Download dQdV figures
    output$dQdV_figure <- downloadHandler(
        filename = function(){
            paste(input$file1,"_dQdV.pdf", sep = "")
        },
        content = function(file){
            pdf(file,width = 6, height = 4)
            par(mfrow=c(1,2))
            par(cex=0.8)
            par(mar=c(5,5,1,1))

            print(plot_dQdV_stack_charge(dataInput(),input$cycleno,
                                         increment = input$increment, height = 150))
            print(plot_dQdV_stack_discharge(dataInput(),input$cycleno,
                                            increment = input$increment, height = 150))
           dev.off()
        }
    )

    # Download energy figures
    output$energy_figure <- downloadHandler(
        filename = function(){
            paste(input$file1,"_energy.pdf", sep = "")
        },
        content = function(file){
            pdf(file,width = 8, height = 4)
            par(mfrow=c(1,2))
            par(mar = c(5, 4, 1, 5))
            par(cex=0.8)

            print(plot_energy(dataInput(),ymax=input$energy))
            print(plot_voltage(dataInput(),
                               ymin=input$hysteresis[1],ymax=input$hysteresis[2]))
            dev.off()
        }
    )
}

# Run the application
shinyApp(ui = ui, server = server)

