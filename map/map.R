library(sf)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(cowplot)
library(terra)
library(ggrepel)
library(here)
library(officer)
library(flextable)

here::i_am("map/map.R")
source(here("map/functions.R"))

tryCatch(
  {
    # First submission
    # df <- load_excel_with_check(here("data/GBD JME checked cleaned  final @CMC 161124.xlsx"))
    # Second submission
    df <- load_excel_with_check(here("data/GBD JME aggregate 05 Oct.xlsx"))
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
SDI <- "(4)sdi_2021"
ANIMAL <- "animal_protein_gm/cap/d"


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

# Now create report

create_top_10_table <- function(crop_column, crop_name, table_num, data = merged_df) {
  cols_to_select <- c(
    "name_long", crop_column, SDI, ANIMAL, STUNTING, WASTING
  )

  table_data <- data %>%
    as_tibble() %>%
    filter(!is.na(!!sym(crop_column))) %>%
    select(all_of(cols_to_select)) %>%
    arrange(desc(!!sym(crop_column))) %>%
    slice(1:10) %>%
    mutate(S.No = row_number()) %>%
    select(
      S.No,
      Country = name_long,
      Consumption = !!sym(crop_column),
      SDI = !!sym(SDI),
      Animal_Protein = !!sym(ANIMAL),
      Stunting = !!sym(STUNTING),
      Wasting = !!sym(WASTING)
    )

  col_names <- c(
    "S.No",
    "Country",
    paste0("Consumption ", crop_name, " (gm/capita/day)"),
    "SDI",
    "Animal Protein (gm/capita/day)",
    "Stunting Prevalence (%)",
    "Wasting Prevalence (%)"
  )

  names(table_data) <- col_names

  ft <- flextable(table_data) %>%
    theme_vanilla() %>%
    set_caption(
      caption = paste0("Table ", table_num, ": Top 10 LMIC Countries Consuming ", crop_name, " Protein")
    ) %>%
    autofit() %>%
    width(width = 4)

  return(ft)
}


create_crop_section <- function(doc, crop_name, crop_variable, writeup_para, merged_data, africa_map_file, world_map_file, table_num, fig_num) {
  doc <- doc %>%
    body_add_par(crop_name, style = "heading 3")


  ft <- create_top_10_table(crop_variable, crop_name, table_num, merged_data)
  doc <- doc %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal")

  doc <- doc %>%
    body_add_par(writeup_para, style = "Normal") %>%
    body_add_par("", style = "Normal")

  doc <- doc %>%
    body_add_img(src = africa_map_file, width = 6, height = 4) %>%
    body_add_caption(block_caption(
      label = paste("S Figure ", fig_num, ": Political map of Africa showing LMIC countries having high ", crop_name, " consumption"),
      style = "Normal"
    )) %>%
    body_add_par("", style = "Normal")


  doc <- doc %>%
    body_add_img(src = world_map_file, width = 6, height = 4) %>%
    body_add_caption(block_caption(
      label = paste("Figure ", fig_num + 1, ": World map showing LMIC countries having high ", crop_name, " consumption"),
      style = "Normal"
    )) %>%
    body_add_par("", style = "Normal")

  return(doc)
}

create_world_section <- function(doc, caption, world_map_file) {
  doc <- doc %>%
    body_add_img(src = world_map_file, width = 6, height = 4) %>%
    body_add_caption(block_caption(
      label = caption,
      style = "Normal"
    )) %>%
    body_add_par("", style = "Normal")

  return(doc)
}


create_word_report_manual <- function(data, output_file = "Report_Crop_Analysis_Manual.docx") {
  doc <- read_docx()

  doc <- doc %>%
    body_add_par("(J) Maps and tables of top staple consuming countries:", style = "heading 2") %>%
    body_add_par(
      "The tables below (S Table 39 to S Table 44) show the top 10 countries consuming the starchy staples discussed. Their SDI, animal protein consumption as well as stunting and wasting prevalence are shown along with for clarity. Any of the numbers of top 10 countries, if in Africa are shown along with in the map of Africa, indicating the geographic patterns of contiguity in crop cultivation. The thresholds low-to- medium-to- high for child stunting and wasting prevalence in top 10 countries are as per S Table 4 (from the Joint malnutrition estimates 2023). (11) Of note, also is the mirroring of distribution of high prevalence of wasting in Fig 1 in the manuscript and that of millet as well as sorghum consumption in Africa(S Fig 44 & S Fig 46) in the Sahel.",
      style = "Normal"
    ) %>%
    body_add_break()


  doc <- create_crop_section(
    doc = doc,
    crop_name = "Sorghum",
    crop_variable = SORGHUM,
    writeup_para = "The top 10 countries consuming Sorghum are all located in the arid Sahel region of Africa. The highest is Sudan in North Africa, but most others are from Western Sub-Saharan Africa with the exception of South Sudan and Ethiopia which are from Eastern Sub-Saharan Africa. They have among them highest wasting prevalence of 22.7 in South Sudan and the lowest 4.3 in Cameroon. Stunting ranges from a high of 47.4 for Niger to 21.8 for Burkina Faso, both falling above the very high and high thresholds as per JME estimates thresholds.",
    merged_data = data,
    africa_map_file = "africa_sorghum.png",
    world_map_file = "sorghum.png",
    table_num = 41,
    fig_num = 44
  )

  doc <- create_crop_section(
    doc = doc,
    crop_name = "Millet",
    crop_variable = MILLET,
    writeup_para = "Among the top 10 countries consuming millets, the highest was Mali with 13.64 gm per capita per day. The highest stunting prevalence was with 47.4 for Niger and the lowest was 13.6 for Gambia. The highest wasting prevalence was 16.3 for Sudan and lowest 5.1 for Gambia. Most countries are from Sahel region of Africa. The stunting and wasting prevalence ranges widely as per JME thresholds from medium to high and very high.",
    merged_data = data,
    africa_map_file = "africa_millet.png",
    world_map_file = "millet.png",
    table_num = 42,
    fig_num = 46
  )

  doc <- create_crop_section(
    doc = doc,
    crop_name = "Cassava",
    crop_variable = CASSAVA,
    writeup_para = "The location of top countries consuming cassava is South of the Sahel inAfrica. Four of top 10 Cassava consuming countries are from CentralSub-Saharan Africa. All other 6 among top 10 countries are from126126adjoining Eastern and Western Sub-Saharan Africa. The highestconsumption of cassava protein was 11.1 of DRC, which wasconsiderably lower than top consumers of other staples because of thevery low protein content of cassava. The highest stunting prevalencewas seen in Burundi with 56.5%(which was the highest among allcountries as well). Of note, is that among these countries the lowestanimal protein consumption was Burundi with 3.1 gm per capita per day.",
    merged_data = data,
    africa_map_file = "africa_cassava.png",
    world_map_file = "cassava.png",
    table_num = 43,
    fig_num = 48
  )

  doc <- create_crop_section(
    doc = doc,
    crop_name = "Maize",
    crop_variable = MAIZE,
    writeup_para = "Five of the top 10 maize consuming countries were located in Southern Sub-Saharan Africa while the remainder 3 were from Central Latin America and 1 from Tropical Latin America. The highest stunting prevalence among them was Guatemala with a 43.5% prevalence and lowest was Paraguay at 3.4%.Wasting was consistently low ranging from  129  129  Zambia with 4.2 which was the highest to Guatemala with 0.8, the lowest.",
    merged_data = data,
    africa_map_file = "africa_maize.png",
    world_map_file = "maize.png",
    table_num = 44,
    fig_num = 50
  )

  doc <- create_crop_section(
    doc = doc,
    crop_name = "Rice",
    crop_variable = RICE,
    writeup_para = "All the top 10 rice consuming countries were from South Asia and South East Asia, with the exception of Madagascar which was from Eastern Sub-Saharan Africa. Highest stunting prevalence among them was Madagascar with 38.6% and lowest was Thailand with 11.8. Wasting prevalence ranged from a high of 15.1 for Sri Lanka and a low of 4.7 for Vietnam.",
    merged_data = data,
    africa_map_file = "africa_rice.png",
    world_map_file = "rice.png",
    table_num = 45,
    fig_num = 52
  )

  doc <- create_crop_section(
    doc = doc,
    crop_name = "Wheat",
    crop_variable = WHEAT,
    writeup_para = "All the top wheat consuming countries were from North Africa and Middle East as well as Central Asia with the exception of Serbia which was from Central Europe. They all had low to very low wasting prevalence and low to medium stunting prevalence.",
    merged_data = data,
    africa_map_file = "africa_wheat.png",
    world_map_file = "wheat.png",
    table_num = 46,
    fig_num = 54
  )

  doc %>%
    body_add_par("World", style = "heading 3")

  doc <- create_world_section(
    doc = doc,
    caption = "S Fig 56: World map of stunting prevalence among LMICs",
    world_map_file = "stunting.png"
  )

  doc <- create_world_section(
    doc = doc,
    caption = "S Fig 57: World map of wasting prevalence among LMICs",
    world_map_file = "wasting.png"
  )


  print(paste("Saving Word document to:", here(output_file)))
  print(doc, target = here(output_file))
  print("Word document created successfully!")
}

create_word_report_manual(
  data = merged_df,
  output_file = "Section J - Supplementary.docx"
)
