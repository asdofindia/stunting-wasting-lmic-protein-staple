library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(terra)
library(ggrepel)
library(here)

here::i_am("map/map.R")
source(here("map/functions.R"))

tryCatch(
  {
    df <- load_excel_with_check(here("data/GBD JME checked cleaned  final @CMC 161124.xlsx"))
  },
  error = function(e) {
    stop(paste("Error reading Excel file:", e$message))
  }
)

# Remove extraneous records
df <- df %>% filter(!is.na(`S. No`))

# Clean column names
names(df) <- tolower(gsub("[#@]", "", names(df)))
names(df) <- gsub(" ", "_", names(df))

# Load World Map Data (using rnaturalearth)
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")


# Spelling corrections
country_corrections <- c(
  "Kyrgyztan" = "Kyrgyzstan",
  "Venezuala" = "Venezuela",
  "Congo" = "Republic of the Congo",
  # DR Congo is already another record
  "Republic of Moldova" = "Moldova",
  "United Republic of Tanzania" = "Tanzania",
  "Syrian Arab Republic" = "Syria",
  "Türkiye" = "Turkey",
  "Micronesia" = "Federated States of Micronesia",
  "Lao People's Democratic Republic" = "Lao PDR",
  "Eswatini" = "Kingdom of eSwatini",
  "Sao Tome and Principe" = "São Tomé and Principe",
  "Gambia" = "The Gambia",
  "Cabo Verde" = "Republic of Cabo Verde"
)

df <- df %>%
  mutate(country...3 = recode(country...3, !!!country_corrections))

# Merge Data
merged_df <- left_join(world, df, by = c("name_long" = "country...3"))

# See what is not matched
unmatched <- anti_join(df, world, by = c("country...3" = "name_long"))

# Remove Antarctica
merged_df <- merged_df %>% filter(admin != "Antarctica")

africa <- merged_df %>% filter(continent == "Africa")
# Define bounding box for Africa (adjust as needed)
xmin <- -20 # West
xmax <- 60 # East
ymin <- -35 # South
ymax <- 40 # North

# Create bounding box as an sf object
africa_bbox <- st_bbox(c(
  xmin = xmin,
  xmax = xmax,
  ymin = ymin,
  ymax = ymax
), crs = st_crs(world)) %>% st_as_sfc()
africa <- st_crop(africa, africa_bbox)


get_top_n_label <- function(column_name, data = merged_df) {
  top_countries <- data %>%
    filter(!is.na(!!sym(column_name))) %>%
    arrange(desc(!!sym(column_name))) %>%
    slice(1:10) %>%
    mutate(label_with_rank = row_number()) %>%
    mutate(label_with_rank_name = paste(row_number(), ". ", name, sep = "")) %>%
    filter(continent == "Africa")

  return(
    geom_label_repel(
      data = top_countries,
      aes(label = label_with_rank, geometry = geometry),
      # Or name if you have cropped
      size = 4,
      stat = "sf_coordinates",
      color = "black",
      hjust = 0.5,
      force = 0.0005,
      # Increase force for further labels
      nudge_x = 0,
      # Adjust nudge for horizontal offset
      nudge_y = 0,
      # Adjust nudge for vertical offset
      segment.size = 0,
      min.segment.length = 0,
      segment.color = "darkgray",
      # Color of the arrow segment
      direction = "both",
      # Make the font bold
      fontface = "bold",
      # Add padding around the label
      label.padding = unit(0.25, "lines"),
      label.r = unit(0.5, "lines")
    )
  )
}


# Function to create maps
create_map <- function(column_name,
                       output_filename,
                       title,
                       extra = NULL,
                       width = 15,
                       data = merged_df) {
  # Convert column to numeric, coercing errors
  data[[column_name]] <- as.numeric(data[[column_name]])

  bbox <- st_bbox(data)

  # Base plot (grey for all countries)
  p <- ggplot(data = data) +
    geom_sf(
      fill = "white",
      color = "black",
      linewidth = 0.1
    ) +
    geom_rect(
      aes(
        xmin = bbox["xmin"] - 10,
        xmax = bbox["xmax"] + 10,
        ymin = bbox["ymin"] - 10,
        ymax = bbox["ymax"] + 10
      ),
      fill = NA,
      color = "black",
      linewidth = 0.5
    )

  # Plot data only where it's not NA
  if (any(!is.na(data[[column_name]]))) {
    p <- p + geom_sf(
      data = data %>% filter(!is.na(!!sym(column_name))),
      aes(fill = !!sym(column_name)),
      color = "black",
      linewidth = 0.1
    )
  }

  # Color scale
  p <- p + scale_fill_continuous(
    low = "lightblue",
    high = "darkblue",
    na.value = "white"
  )

  if (!(is.null(extra))) {
    p <- p + extra
  }
  p <- p + labs(
    x = NULL,
    y = NULL,
    title = title,
    fill = column_name
  ) +
    # theme_void() + # Remove background
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      rect = element_blank()
    )
  p <- p + theme(legend.position = "bottom")

  if (output_filename != "") {
    ggsave(
      output_filename,
      plot = p,
      bg = "white",
      width = width,
      height = 10,
      units = "in",
      dpi = 300
    )
  }
  return(p)
}

# variables
STUNTING <- "jme_stunting_2022"
WASTING <- "jmewasting_jme_2022"
SORGHUM <- "(17)sorghum_and_products"
MILLET <- "(18)millet_and_products"
CASSAVA <- "(19)cassava_and_products"
MAIZE <- "(16)maize_and_products"
RICE <- "(15)rice_and_products"
WHEAT <- "(14)wheat_and_products"


# Africa Maps
stunting_map_africa <- create_map(STUNTING,
  "africa_stunting.png",
  "Stunting",
  width = 10,
  data =
    africa
)
wasting_map_africa <- create_map(WASTING,
  "africa_wasting.png",
  "Wasting",
  width = 10,
  data =
    africa
)

ggsave(
  "africa_stunting_wasting_map.png",
  plot = wrap_plots(list(stunting_map_africa, wasting_map_africa), ncol = 2),
  width = 15,
  height = 10,
  units = "in",
  dpi = 300,
  bg = "white"
)


sorghum_map_africa <- create_map(
  SORGHUM,
  "africa_sorghum.png",
  "Sorghum",
  get_top_n_label(SORGHUM),
  width = 10,
  data = africa
)
millet_map_africa <- create_map(
  MILLET,
  "africa_millet.png",
  "Millet",
  get_top_n_label(MILLET),
  width = 10,
  data = africa
)

cassava_map_africa <- create_map(
  CASSAVA,
  "africa_cassava.png",
  "Cassava",
  get_top_n_label(CASSAVA),
  width = 10,
  data = africa
)


maize_map_africa <- create_map(
  MAIZE,
  "africa_maize.png",
  "Maize",
  get_top_n_label(MAIZE),
  width = 10,
  data =
    africa
)

rice_map_africa <- create_map(
  RICE,
  "africa_rice.png",
  "Rice",
  get_top_n_label((RICE)),
  width = 10,
  data =
    africa
)
wheat_map_africa <- create_map(
  WHEAT,
  "africa_wheat.png",
  "Wheat",
  extra = get_top_n_label(WHEAT),
  width = 10,
  data =
    africa
)

# World maps
sorghum_map_world <- create_map(SORGHUM, "sorghum.png", "Sorghum",
  data =
    merged_df
)
millet_map_world <- create_map(MILLET, "millet.png", "Millet",
  data =
    merged_df
)
cassava_map_world <- create_map(CASSAVA, "cassava.png", "Cassava",
  data =
    merged_df
)


maize_map_world <- create_map(MAIZE, "maize.png", "Maize",
  data =
    merged_df
)

rice_map_world <- create_map(RICE, "rice.png", "Rice",
  data =
    merged_df
)
wheat_map_world <- create_map(WHEAT, "wheat.png", "Wheat",
  data =
    merged_df
)


stunting_map_world <- create_map(STUNTING, "stunting.png", "Stunting",
  data =
    merged_df
)
wasting_map_world <- create_map(WASTING, "wasting.png", "Wasting",
  data =
    merged_df
)

ggsave(
  "stunting_wasting_map.png",
  plot = wrap_plots(list(stunting_map_world, wasting_map_world), ncol = 1),
  width = 15,
  height = 20,
  units = "in",
  dpi = 300,
  bg = "white"
)

print("Maps created successfully!")
