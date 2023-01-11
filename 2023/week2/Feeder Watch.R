### -------- Feeder Watch 10-1-2023 -------- ###

#---- load libraries -----
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

# ---- which were the 10 most observed species? ----
top10Species <- validatedObservations %>%
  arrange(desc(`Number of Observations`)) %>%
  slice(1:10)

ggplot(top10Species) +
  geom_col(aes(`Species Name`, `Number of Observations`)) +
  theme_classic() +
  scale_y_continuous(expand = c(0,0)) +
  scale_x_discrete(labels = function(`Species Name`) str_wrap(`Species Name`, width = 7))


# ---- where were those top 10 species observed? ----
world <- ne_countries(scale = "medium", returnclass = "sf")

for (i in 1:nrow(top10Species)) {
  assign(paste0("species", i), top10Species$`Species Name`[i])
} # assign species names to a series of variables in the format species1, species2, etc

colourPalette <- get_palette(palette = "jco", 10)

# Spinus pinus
locationSpecies1 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species1)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies1, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[1], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Spinus pinus has been observed")

# Acanthis flammea
locationSpecies2 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species2)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies2, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[2], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Acanthis flammea has been observed")

# Zenaida macroura
locationSpecies3 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species3)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies3, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[3], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed")  +
  ggtitle("Sites where Zenaida macroura has been observed")


# Cyanocitta cristata
locationSpecies4 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species4)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies4, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[4], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed")  +
  ggtitle("Sites where Cyanocitta cristata has been observed")


# Cardinalis cardinalis
locationSpecies5 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species5)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies5, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[5], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed")  +
  ggtitle("Sites where Cardinalis cardinalis has been observed")


# Branta canadensis
locationSpecies6 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species6)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies6, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[6], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Branta canadensis has been observed")

# Haemorhous purpureus
locationSpecies7 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species7)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies7, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[7], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Haemorhous purpureus has been observed")

# Spinus tristis
locationSpecies8 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species8)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies8, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[8], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Spinus tristis has been observed")

# Haemorhous mexicanus
locationSpecies9 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species9)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies9, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[9], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Haemorhous mexicanus has been observed")

# Anas platyrhynchos
locationSpecies10 <- speciesData %>%
  filter(reviewed == 1,
         valid == 1,
         scientific_name == species10)

ggplot(data = world) +
  geom_sf(fill = "white") +
  geom_point(data = locationSpecies10, aes(longitude, latitude, size = how_many), shape = 21, fill = colourPalette[10], alpha = 0.4) +
  coord_sf(xlim = c(-150, -50), ylim = c(20, 70), expand = FALSE, clip = "on") +
  theme(panel.background = element_rect(fill = "white"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = c(0.1, 0.15),
        legend.key = element_rect(fill = "white")) +
  labs(size = "Number observed") +
  ggtitle("Sites where Anas platyrhynchos has been observed")

# see about doing an HTML or shiny app

