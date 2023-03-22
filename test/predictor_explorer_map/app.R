
all_data = read.csv(file = "all_predictor_1981_2019_45cuencas.csv")
cod_cuencas = unique(all_data$catchment_code)

#sort variables
all_data$var = factor(all_data$var, levels=c("STORAGE","NINO1.2","ONI","PDO","SOI"))
#sort month names
months_wy <- c("abr", "may", "jun", "jul", "ago", "sep","oct", "nov", "dic", "ene", "feb", "mar")

all_data$month_wy = factor(all_data$month_wy, levels = paste0("1˚",months_wy) )

library(shiny)
library(ggplot2)
library(sf)
library(shinyWidgets)

# Function to plot all catchments and highlight target catchments
plot_catchments <- function(shapefile_path, target_catchment = NULL, simplify = TRUE, tolerance = 1000, make_valid = TRUE) {
  # Read the shapefile
  shapefile <- read_sf(shapefile_path)
  
  # Make the shapefile valid if requested
  if (make_valid) {
    shapefile <- st_make_valid(shapefile)
  }
  
  # Simplify the shapefile if requested
  if (simplify) {
    shapefile <- st_simplify(shapefile, dTolerance = 4000)
  }
  
  # Plot all catchments
  plot <- ggplot() +
    geom_sf(data = shapefile, fill = "grey") +
    scale_x_continuous(breaks = seq(-69,-72,by = -1),labels = seq(-69,-72,by = -1)) +
    scale_y_continuous(breaks = seq(-27, -37, by = -1),labels = seq(-27, -37, by = -1))+
    labs(title = "")+
    theme_minimal()+
    theme(
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent'),
      legend.box.background = element_rect(fill='transparent')
    )
  
  # Highlight target catchments if provided
  if (!is.null(target_catchment)) {
    target_data <- shapefile[shapefile$gauge_id %in% target_catchment,]
    plot <- plot + geom_sf(data = target_data, fill = "red")
  }
  
  return(plot)
}

ui <- fluidPage(
  titlePanel("Exploración de predictores por cuencas"),
  sidebarLayout(
    sidebarPanel(
      sliderTextInput("catchment_code",
                      "Código de cuenca:",
                      choices = unique(cod_cuencas),
                      selected = sample(cod_cuencas, 1)
      ),
      sliderInput("target_wy",
                  "Año objetivo:",
                  min = min(unique(all_data$wy)),
                  max = max(unique(all_data$wy)),
                  step = 1,
                  value = sample(unique(all_data$wy), 1)
      ),
      plotOutput("catchments_plot")
    ),
    mainPanel(
      plotOutput("predictores_plot"),
      
    )
  )
)


server <- function(input, output) {
# Render the predictor plot
  output$predictores_plot <- renderPlot({
    # Subset the data based on user inputs
    data_input <- subset(all_data, catchment_code == input$catchment_code)
    target_wy <- input$target_wy
    
    # Create the plot
    plot <- ggplot(data = data_input) +
      # Add historical records
      geom_line(aes(x = month_wy, y = predictor_value, col = "1981-2020", group = wy)) +
      scale_x_discrete(expand = c(0, 0)) +
      # Add median
      geom_line(aes(x = month_wy, y = median, col = "median", group = "wy")) +
      # Add a target year
      geom_line(data = subset(data_input, wy == target_wy),
                aes(x = month_wy, y = predictor_value, col = "target_wy", group = "wy")) +
      scale_color_manual(values = c("median" = "red",
                                    "1981-2020" = "grey50",
                                    "target_wy" = "blue"),
                         labels=c('1981-2019','Mediana', target_wy)) +
      facet_wrap(~var, scales = "free_y", ncol = 1) +
      labs(
        title = unique(data_input$gauge_name),
        x = "fecha de emisión",
        y = "valor del predictor",
        col = "Años hidrológicos"
      )
    
    return(plot)
  }, height = 700, width = 600)
  
  # Render the catchments plot
  output$catchments_plot <- renderPlot({
    plot_catchments(shapefile_path = "shapefile_cuencas/cuencas_fondef-dga.shp", 
                    target_catchment = input$catchment_code)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server, options = list(display.mode = 'showcase'))



