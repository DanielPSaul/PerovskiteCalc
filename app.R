# Import packages
library(shiny) # Package required for shiny
library(rsconnect) # Connection to shinyapps.io servers
library(shinythemes) # UI themes for shiny
library(shinydashboard) # Dashboard functionality
library(DT) # Data table functionality
library(tidyverse) # Data wrangling/transformation
library(shinyalert) # Used to create the popups/alerts
library(shinyBS) # Tooltip descriptions
library(shinyjs) # 'Quit' page to prevent freezes on local machine
library(leaflet) # Location and mapping
library(zipcodeR) # Zipcode input functionality
library(plotly) # Nice plots
library(lubridate) # Dealing with time series data
library(httr) # API calls
library(forecast)
library(jsonlite)
library(reshape2)
library(writexl) # Download excel files
library(xlsx)
library(readxl)
library(lpSolve) # Linear programming for financial model

ui <- fluidPage(
  
  theme = shinytheme("lumen"),
  navbarPage(title="Perovskite Cell Model", 
             windowTitle = "Perovskite Cell Model",
             
             
             tabPanel(
               'Calculator',
               icon = icon("calculator"),
               
               h2('Instructions'),
               p('1. Enter your location inputs to account for differences in regional prices and solar radiation. The current default values are for Athens, Georgia, United States.'),
               p('2. Select whether or not you want to calculate the LCOE of a solar project/panel or just compare your LCOE to electricity prices.'),
               p('3. Keep or update the default values (based on NREL PVWatts).'),
               p('4. Press "Calculate" to run the simulation.'),
               p('5. Download the table or visual outputs if necessary, or navigate to the "Restart Session" tab to reset inputs and clear outputs.'),
               hr(),
               
               sidebarLayout(
                 sidebarPanel(
                   titlePanel("Inputs"),
                   width = 3,
                   
                   selectInput(inputId = "location_type", 
                               label = "Location Input Type:", 
                               width = "200px", 
                               choices = c("Latitude/Longitude", "Zipcode")),
                   
                   conditionalPanel(condition = "input.location_type == 'Latitude/Longitude'",
                                    numericInput(inputId = "lat", 
                                                 label = "Latitude:", 
                                                 width = '200px', 
                                                 value = '33.94', 
                                                 min=-90, 
                                                 max=90),
                                    bsTooltip('lat', 
                                              "Latitude of weather location. Please round to two decimals and provide a value between -90 and 90.",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    numericInput(inputId = "lon", 
                                                 label = "Longitude:", 
                                                 width = '200px', 
                                                 value = '-83.37', 
                                                 min=-180, 
                                                 max=180),
                                    bsTooltip('lon', 
                                              "Longitude of weather location. Please round to two decimals and provide a value between -180 and 180.",
                                              'right',
                                              options = list(container = 'body'))),
                   
                   conditionalPanel(condition = "input.location_type == 'Zipcode'",
                                    
                                    numericInput(inputId = "zipcode",
                                                 label = "Zipcode:", 
                                                 width = '200px',
                                                 value = '30601',
                                                 min=00000,
                                                 max=99999),
                                    bsTooltip('zipcode', 
                                              "The 5-digit zipcode of the weather location. Please do not add any spaces or dashes.",
                                              'right',
                                              options = list(container = 'body'))
                                    
                                    ),
                  
                   selectInput(inputId = "calc_type", 
                               label = "Calculation Type:", 
                               width = "200px", 
                               choices = c("Find LCOE", "Compare LCOE")),
                   
                   conditionalPanel(condition = "input.calc_type == 'Find LCOE'",
                                    
                                    
                                    numericInput(inputId = "initial_investment",
                                                 label = "Total Installation Cost ($):", 
                                                 width = '200px',
                                                 value = '20000',
                                                 min=0),
                                    bsTooltip('initial_investment', 
                                              "The total initial investment to install the PV system (including cost of PV modules, racking, interconnects, labor, and permits).",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    numericInput(inputId = "operating_cost",
                                                 label = "Annual Operating Cost ($):", 
                                                 width = '200px',
                                                 value = '10000',
                                                 min=0),
                                    bsTooltip('operating_cost', 
                                              "The average annual cost for operation and maintenance.",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    numericInput(inputId = "system_lifetime",
                                                 label = "System Lifetime (years):", 
                                                 width = '200px',
                                                 value = '25',
                                                 min=0),
                                    bsTooltip('system_lifetime', 
                                              "The system lifetime in years (researchers usually assume PSC could last more than 10 years, which is impossible based on current technique, or there is no way to be cost effective).",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    numericInput(inputId = "degradation_rate",
                                                 label = "Degradation Rate (%):", 
                                                 width = '200px',
                                                 value = '0.5',
                                                 min=0),
                                    bsTooltip('degradation_rate', 
                                              "The annual module efficiency degradation rate.",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    numericInput(inputId = "discount_rate",
                                                 label = "Discount Rate (%):", 
                                                 width = '200px',
                                                 value = '5',
                                                 min=0),
                                    bsTooltip('discount_rate', 
                                              "The nominal discount rate.",
                                              'right',
                                              options = list(container = 'body')),
                                    
                                    # Sections for whether or not user has energy output
                                    radioButtons(inputId = "has_energy_output", 
                                                 label = "Do you have the annual energy output?:", 
                                                 width = "200px", 
                                                 choices = c("Yes", "No")),
                                    
                                    conditionalPanel(condition = "input.has_energy_output == 'Yes'",
                                                     
                                                     numericInput(inputId = "energy_output",
                                                                  label = "Annual Energy Output (kWh):", 
                                                                  width = '200px',
                                                                  value = '18000',
                                                                  min=0),
                                                     bsTooltip('energy_output', 
                                                               "The un-degraded, annual energy output by the system as electricity, could be calculated by the PCE of PSC.",
                                                               'right',
                                                               options = list(container = 'body'))
                                    ),
                                    
                                    conditionalPanel(condition = "input.has_energy_output == 'No'",
                                                     
                                                     # More inputs button
                                                     actionButton(inputId = "more_inputs",
                                                                  label = "More Inputs",
                                                                  width = '100px'),
                                                     helpText("If you do not want to use the default energy calculation inputs, you must click the 'More Inputs' button and fill in those inputs before calculating.")
                                                     
                                    ),
                                    
                                    bsModal(id = 'modal1', title = 'Energy Inputs', trigger = "more_inputs", size = 'large',
                                            
                                            fluidRow(
                                              column(4,
                                                     
                                                     # Module efficiency input
                                                     numericInput(inputId = "module_efficiency",
                                                                  label = "Module Efficiency (%):", 
                                                                  width = '200px',
                                                                  value = '18',
                                                                  min=0),
                                                     bsTooltip('module_efficiency', 
                                                               "The module efficiency (how much radiation is converted to energy).",
                                                               'right',
                                                               options = list(container = 'body')),
                                                     
                                                     # Losses input
                                                     numericInput(inputId = 'losses',
                                                                  label = 'Losses (%):',
                                                                  width = '200px',
                                                                  value = '14',
                                                                  min=-5,
                                                                  max=99),
                                                     bsTooltip('losses', 
                                                               "Estimated total system losses.",
                                                               'right',
                                                               options = list(container = 'body'))

                                                     
                                                     ),
                                              
                                              column(4,
                                                     
                                                     helpText('For perovskite solar sells, please select the thin film module type.'),
                                                     
                                                     # Array type input
                                                     radioButtons(inputId = 'array_type', 
                                                                  label = 'Array Type:', 
                                                                  choiceNames = c('Fixed - Open Rack','Fixed - Roof Mounted','1-Axis','Backtracked','2-Axis'), 
                                                                  choiceValues = c(0,1,2,3,4)),
                                                     
                                                     # Module type input
                                                     radioButtons(inputId = 'module_type', 
                                                                  label = 'Module Type:', 
                                                                  choiceNames = c('Standard: Polycrystalline','Premium: Monocrystalline','Thin film'), 
                                                                  choiceValues = c(0,1,2))
                                                     
                                                     ),
                                              
                                              column(4,
                                                     
                                              
                                                     
                                                     # Array area input
                                                     numericInput(inputId = 'array_area',
                                                                  label = 'Array Area (m\u00B2):',
                                                                  width = '200px',
                                                                  value = '40',
                                                                  min=0,
                                                                  max=100000),
                                                     bsTooltip('array_area', 
                                                               "The total estimate area of the array, inlcuding all panel faces. It can also be a value of roof or ground area.",
                                                               'right',
                                                               options = list(container = 'body')),
                                                     
                                                     # Panel tilt input
                                                     numericInput(inputId = 'tilt',
                                                                  label = 'Panel Tilt (degrees):',
                                                                  width = '200px',
                                                                  value = '0',
                                                                  min=0,
                                                                  max=90),
                                                     bsTooltip('tilt', 
                                                               "Degree tilt of the solar panels.",
                                                               'right',
                                                               options = list(container = 'body')),
                                                     
                                                     # Azimuth input
                                                     numericInput(inputId = 'azimuth',
                                                                  label = 'Azimuth (degrees):',
                                                                  width = '200px',
                                                                  value = '180',
                                                                  min=0,
                                                                  max=360),
                                                     bsTooltip('azimuth', 
                                                               "Azimuth is the angle that the solar panels are facing and is measured in a clockwise direction from north.",
                                                               'right',
                                                               options = list(container = 'body'))
                                                     
                                                     )
                                              
                                              
                                            )
                                            
                                            
                                          
                                    ),
                                    
                                    br(),
                                
                                    ), # End of find LCOE conditional panel
                   
                   
                   conditionalPanel(condition = "input.calc_type == 'Compare LCOE'",
                                    
                                    # LCOE input
                                    numericInput(inputId = 'lcoe_in',
                                                 label = 'LCOE ($/kWh):',
                                                 width = '200px',
                                                 value = '',
                                                 min=0),
                                    bsTooltip('lcoe_in', 
                                              "The estimated levelized cost of electricity of your solar project.",
                                              'right',
                                              options = list(container = 'body'))
                                    
                                    ),
                   
                   # LCOE submit button
                   actionButton(inputId = "lcoe_button",
                                label = "Calculate",
                                width = '100px',
                                icon("arrows-rotate"))
                   
              
                   
                   
                 ),
                 mainPanel(
                   
                   fluidRow(
                     column(4,
                            HTML(paste0("<b>","Utility Company:","</b>")),
                            textOutput('utility_name_out'),
                            br(),
                            br(),
                            HTML(paste0("<b>","Commercial Electricty Rate:","</b>")),
                            textOutput('commercial_rate_out')),
                     column(4,
                            HTML(paste0("<b>","Industrial Electricty Rate:","</b>")),
                            textOutput('industrial_rate_out'),
                            br(),
                            br(),
                            HTML(paste0("<b>","Residential Electricty Rate:","</b>")),
                            textOutput('residential_rate_out')),
                     conditionalPanel(condition = "input.calc_type == 'Find LCOE'",
                     column(4,
                            HTML(paste0("<b>","Levelized Cost of Electricity (LCOE):","</b>")),
                            textOutput('lcoe_out')))
                   ),
                   
                   br(),
                   br(),
                   conditionalPanel(condition = "input.has_energy_output == 'No'",
                   fluidRow(
                     column(4,
                            HTML(paste0("<b>","Solar Radiation:","</b>")),
                            textOutput('solrad_annual_out'),
                            br(),
                            br(),
                            HTML(paste0("<b>","Capacity Factor:","</b>")),
                            textOutput('capacity_factor_out')),
                     column(4,
                            HTML(paste0("<b>","State of Weather Station:","</b>")),
                            textOutput('station_state_out'),
                            br(),
                            br(),
                            HTML(paste0("<b>","Elevation of Weather Station:","</b>")),
                            textOutput('station_elev_out'))
                   )
                   
                   ),
                   
                   
                   conditionalPanel(condition = "input.calc_type == 'Find LCOE'",
                   hr(),
                   downloadButton('download_table', 'Download Table'),
                   br(),
                   br(),
                   DT::dataTableOutput("output_table"),
                   br(),
                   br(),
                   plotlyOutput(outputId = "cost_plot", height = 'auto', width = 'auto'),
                   br()
                   )

                   
                 )
               )
             ),
             
             
             
             # Restart session tab
             tabPanel(title = "Restart Session", 
                      icon = icon("circle-xmark"),
                      actionButton("reset_session", "Restart Session")
             )
             
  )
  
)

server <- function(input, output, session) {
  
  observeEvent(input$reset_session, {
    session$reload()
  })
  
  
  # Initialize variables for API calls
  api_key <- '9UUX1nAVhZoj90XY9R1QHD4U5foWHVABoQxlbnxt'
  timeframe <- 'hourly'
  dataset <- 'nsrdb'
  interval <- '60'
  email <- 'danielsaul@uga.edu'
  names <- 'tmy-2020'
  use_wf_albedo <- 1
  
  
  
  # Run LCOE simulation
  observeEvent(input$lcoe_button, {
    shinyalert("Calculating...", showConfirmButton = FALSE, timer = 0)
    
    # Convert Zipcode input to latitude and longitude
    if (input$location_type == "Zipcode") {
      
      codes <- geocode_zip(input$zipcode)
      
      lat <- codes[[2]]
      lon <- codes[[3]]
      
      
    } else {
      
      lat = input$lat
      lon = input$lon
      
    }
    
    
    # Establish inputs
    initial_investment <- as.numeric(isolate(input$initial_investment))
    operating_cost <- as.numeric(isolate(input$operating_cost))
    system_lifetime <- as.numeric(isolate(input$system_lifetime))
    degradation_rate <- as.numeric(isolate(input$degradation_rate))/100
    discount_rate <- as.numeric(isolate(input$discount_rate))/100
    
    # If user has energy output number inputs
    if (input$has_energy_output == 'Yes'){
      
      energy_output <- as.numeric(isolate(input$energy_output))
      
    } else {
      
      module_efficiency <- as.numeric(isolate(input$module_efficiency))/100
      tilt <- isolate(input$tilt) 
      azimuth <- isolate(input$azimuth) 
      array_type <- as.numeric(isolate(input$array_type))
      module_type <- as.numeric(isolate(input$module_type))
      array_area <- as.numeric(isolate(input$array_area))
      losses <- as.numeric(isolate(input$losses))/100
      
      system_capacity <- array_area * 1 * module_efficiency
      
      # Run the PVWatts simulation
      solar_data_api_outputs <- jsonlite::fromJSON(paste0('https://developer.nrel.gov/api/pvwatts/v8.json?api_key=',api_key,'&lat=',lat,'&lon=',lon,'&system_capacity=',system_capacity,'&azimuth=',azimuth,'&tilt=',tilt,'&array_type=',array_type,'&module_type=',module_type,'&losses=',losses,'&timeframe=',timeframe,'&dataset=',dataset,'&use_wf_albedo=',use_wf_albedo))
      
      # Get annual outputs
      station_elev <- round(solar_data_api_outputs[["station_info"]][["elev"]], 2)
      station_state <- solar_data_api_outputs[["station_info"]][["state"]]
      energy_output <- round(solar_data_api_outputs[["outputs"]][["ac_annual"]], 2)
      solrad_annual <- round(solar_data_api_outputs[["outputs"]][["solrad_annual"]], 2)
      capacity_factor <- round(solar_data_api_outputs[["outputs"]][["capacity_factor"]], 2)
      
      # Show general api outputs
      output$solrad_annual_out <- renderText({
        paste0(solrad_annual, " kWh/m\u00B2/day")
      })
      
      output$capacity_factor_out <- renderText({
        paste0(capacity_factor, "%")
      })
      
      output$station_state_out <- renderText({
        paste0(station_state, ", United States")
      })
      
      output$station_elev_out <- renderText({
        paste0(station_elev, " meters")
      })
      
    }
    
    
    # # Get the most recent inflation rate data from FRED API
    # fred_url <- "https://api.stlouisfed.org/fred/series/observations?series_id=CPALTT01USM657N&api_key=5b303891d1c1292239660c62b36805a4&file_type=json&limit=1&sort_order=desc"
    # response <- GET(fred_url)
    # inflation_data <- jsonlite::fromJSON(rawToChar(response$content))$observations
    # 
    # # Extract the most recent inflation rate value
    # inflation_rate <- round(as.numeric(inflation_data$value),4)
    
    
    # Electricity/utility rate apis
    utility_outputs <- jsonlite::fromJSON(paste0('https://developer.nrel.gov/api/utility_rates/v3.json?api_key=',api_key,'&lat=',lat,'&lon=',lon))
    utility_name <- utility_outputs[["outputs"]][["utility_info"]][["utility_name"]]
    commercial_rate <- utility_outputs[["outputs"]][["commercial"]]
    industrial_rate <- utility_outputs[["outputs"]][["industrial"]]
    residential_rate <- utility_outputs[["outputs"]][["residential"]]
    
    output$utility_name_out <- renderText({
      paste0(utility_name)
    })
    
    output$commercial_rate_out <- renderText({
      paste0('$', commercial_rate, "/kWh")
    })
    
    output$industrial_rate_out <- renderText({
      paste0('$', industrial_rate, "/kWh")
    })
    
    output$residential_rate_out <- renderText({
      paste0('$', residential_rate, "/kWh")
    })
    
    
    
    # LCOE calculations/outputs
    years <- 1:system_lifetime
    energy_outputs <- energy_output * (1 - as.numeric(degradation_rate))^years
    operating_costs <- operating_cost / (1 + discount_rate)^years
    
    output_table <- data.frame(year = years, energy_output = energy_outputs, install_cost = initial_investment, operating_cost = operating_costs, degradation_rate = degradation_rate, discount_rate = discount_rate)
    output_table <- round(output_table,4)
    
    overall_lcoe <- round((initial_investment + sum(output_table$operating_cost))/sum(output_table$energy_output),4)
    
    colnames(output_table) <- c('Lifetime Year', 'Energy Output (kWh)', 'Installation Cost ($)', 'Operating Cost ($)', 'Degradation Rate', 'Discount Rate')
    
    output$lcoe_out <- renderText({
      paste0('$', overall_lcoe, "/kWh")
    })

    
    
    
    output$output_table <- DT::renderDataTable(output_table,
                                                 options = list(autoWidth = TRUE,
                                                                pageLength = 5),
                                               rownames = FALSE)
    
    # Cost and energy plot
    output$cost_plot <- renderPlotly({

      theme_set(theme_classic())
      p1 <- ggplot(data = output_table, aes(x = `Lifetime Year`, y = `Operating Cost ($)`)) + geom_line() + geom_point()
      p2 <- ggplot(data = output_table, aes(x = `Lifetime Year`, y = `Energy Output (kWh)`)) + geom_line() + geom_point()
      p <- subplot(p1,p2, titleX = TRUE, titleY = TRUE)
      
      p <- ggplotly(p, tooltip = "text")
      
    })
    
    
    output$download_table <- downloadHandler(
      filename = function() {
        paste("LCOE-Data-", Sys.Date(), ".xlsx", sep="")
      },
      content = function(file) {
        
        
        write.xlsx(output_table, file, col.names = TRUE, row.names = FALSE, showNA = FALSE)
        
      }
    )
    
  
    
    

    
    shinyalert("Calculation Complete.", showConfirmButton = TRUE, type = "success", immediate = TRUE)
    
  })
  
 
  
} # End of server

shinyApp(ui = ui, server = server)