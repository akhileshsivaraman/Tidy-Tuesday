#---- Tidy Tuesday Feeder Watch Shiny App----

#---- load packages ----
library(shiny)
library(tidytuesdayR)
library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggpubr)


#---- load data ----
data <- tt_load("2023-01-10")
feederwatch <- data$PFW_2021_public
site <- data$PFW_count_site_data_public_2021
species <- read.csv("2023-1-10 PFW-species-translation-table.csv")

#---- cleaning data ----
# select relevant columns
speciesObserved <- feederwatch %>%
  select(c(species_code,
           how_many,
           valid,
           reviewed,
           latitude,
           longitude,
           Month,
           Year))


# select relevant columns
speciesNames <- species %>%
  select(c(species_code, scientific_name))


# join the relevant columns using species_code as the key
speciesData <- inner_join(speciesObserved,
                          speciesNames,
                          by = "species_code")


# filter for species that were observed and the observations validated by an expert reviewer
validatedObservations <- speciesData %>%
  filter(valid == 1,
         reviewed == 1) %>%
  select(c(scientific_name, how_many)) %>%
  group_by(scientific_name) %>%
  tally(how_many) %>%
  `colnames<-`(c("Species Name", "Number of Observations"))


#---- which are the top 10 species ----
top10Species <- validatedObservations %>%
  arrange(desc(`Number of Observations`)) %>%
  slice(1:10)

top10SpeciesNames <- top10Species$`Species Name`


#---- where were those 10 species observed ----
world <- ne_countries(scale = "medium", returnclass = "sf")

# filter for reviewed and valid data
reviewedData <- speciesData %>%
  filter(reviewed == 1,
         valid == 1)

# create colour palette
colourPalette <- get_palette(palette = "jco", 10)

# function to find a species' location
find_species <- function(species) {
  filter(reviewedData, scientific_name == species)
}

# function to plot the species' location
plot_species <- function(species, location, colour_number) {
  ggplot(data = world) +
    geom_sf(fill = "white") +
    geom_point(data = location, aes(longitude, latitude, size = how_many), shape = 21, alpha = 0.4, fill = colourPalette[colour_number]) +
    coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
    theme(panel.background = element_rect(fill = "white"),
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          legend.position = c(0.1, 0.15),
          legend.key = element_rect(fill = "white")) +
    labs(size = "Number observed") +
    ggtitle(paste0("Sites where ", species, " has been observed"))
}


#---- UI ----
ui <- fluidPage(
  titlePanel("Tidy Tuesday Feeder Watch"),
  
  sidebarLayout(
    mainPanel(
      h4("Which were the 10 most observed species?"),
      plotOutput("top10plot"),
      br(),
      br(),
      plotOutput("species_location")
    ),
    
    sidebarPanel(
      selectInput(inputId = "selected_species", 
                  label = "Select a species to display where they were spotted",
                  choices = top10SpeciesNames)
    )
    
  )
)


#---- server ----
server <- function(input, output) {
  output$top10plot <- renderPlot({
    ggplot(top10Species) +
      geom_col(aes(`Species Name`, `Number of Observations`)) +
      theme_classic() +
      scale_y_continuous(expand = c(0,0)) +
      scale_x_discrete(labels = function(`Species Name`) str_wrap(`Species Name`, width = 4))
  })
  
  location_species <- reactive({
    find_species(input$selected_species)
  })
  
  species_plot <- reactive({
    plot_species(input$selected_species, location_species(), 1)
  })
  
  output$species_location <- renderPlot({
    species_plot()
  })
  
}


#---- create app ----
shinyApp(ui, server)