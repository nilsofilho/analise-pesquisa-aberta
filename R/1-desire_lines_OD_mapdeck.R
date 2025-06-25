rm(list =ls()); gc()

source("fun/setup.R")




# OD analysis -------------------------------------------------------------

# Token mapdeck
mapdeck::set_token()

# Load google sheets data with google OAuth
dados <- read_sheet('1fAiHo6XCKSJF_gi79VyXvyE70Mmh-VYWAh-bYn__qo8') %>%
  setDT() %>%
  clean_names()

#phones with only numbers
dados[, (grep("x1", names(dados), value = TRUE)) := lapply(.SD, function(x) gsub("[^0-9]", "", x)), .SDcols = grep("x1", names(dados), value = TRUE)]

#centroids
bairros_centroids <- st_read('data/bairros/bairros_for.geojson') %>%
  mutate(key = stri_trans_general(tolower(Nome), "Latin-ASCII")) %>%
  st_centroid() %>%
  select(key, geometry)


#processing of data to OD

#referencing columns in dplyr by name pattern
orig_col <- sym("x4_qual_bairro_de_origem")
dest_col <- sym("x5_qual_bairro_de_destino")

od_data_processed <- dados %>%
  #correct names
  mutate(!!orig_col := case_when(!!orig_col == "JARDIM AMÁRICA" ~ "JARDIM AMÉRICA",
                                 !!orig_col == "SÃO JOÃO DO TAUAPE" ~ "TAUAPE",
                                 TRUE ~ !!orig_col)) %>%
  #OD pairs
  group_by(origem = !!orig_col, destino = !!dest_col) %>%
  summarise(volume = n(), .groups = 'drop') %>%
  filter(volume > 0) %>%
  #key for join with centroids
  mutate(
    key_orig = stri_trans_general(tolower(origem), "Latin-ASCII"),
    key_dest = stri_trans_general(tolower(destino), "Latin-ASCII")
  ) %>%
  #join
  left_join(bairros_centroids, by = c("key_orig" = "key")) %>%
  rename(orig_geom = geometry) %>%
  left_join(bairros_centroids, by = c("key_dest" = "key")) %>%
  rename(dest_geom = geometry)

#extracting coordinates for mapdeck arc
od_arc_data <- od_data_processed %>%
  filter(!st_is_empty(orig_geom) & !st_is_empty(dest_geom)) %>%
  mutate(
    orig_x = st_coordinates(orig_geom)[, 1],
    orig_y = st_coordinates(orig_geom)[, 2],
    dest_x = st_coordinates(dest_geom)[, 1],
    dest_y = st_coordinates(dest_geom)[, 2]
  ) %>%
  st_drop_geometry() %>%
  #re-scale arcs width based on volume
  mutate(
    stroke_width = rescale(volume, to = c(1, 10)),
    #labels
    tooltip_label = glue(
      "<b>Origem:</b> {origem}<br>
       <b>Destino:</b> {destino}<br>
       <b>Volume:</b> {volume}"
    )
  ) %>% arrange(volume)

#configuring color scale
# pal <- scales::col_numeric(palette = c("#deebf7", "#3182bd"), domain = c(1, 7))
pal <- scales::col_numeric(palette = c("#b6d3ed", "#1d4e72"), domain = c(min(od_arc_data$volume), max(od_arc_data$volume)))
od_arc_data$color_hex <- pal(od_arc_data$volume)


# Layers for mapdeck

#1. Bairros layer with trip generation
bairros_polygons_base <- st_read('data/bairros/bairros_for.geojson') %>%
  mutate(key = stri_trans_general(tolower(Nome), "Latin-ASCII"))

# trip production and attraction
prod <- od_data_processed %>% group_by(key_orig) %>% dplyr::summarise(prod = sum(volume))
atrat <- od_data_processed %>% group_by(key_dest) %>% dplyr::summarise(atrat = sum(volume))

# layer with bairros and trip generation
viagens <- bairros_polygons_base %>%
  left_join(prod, by = c("key" = "key_orig")) %>%
  left_join(atrat, by = c("key" = "key_dest")) %>%
  # using trip generation for color scale
  mutate(
    geracao = coalesce(prod, 0L) + coalesce(atrat, 0L),
    prod = coalesce(prod, 0L),
    atrat = coalesce(atrat, 0L)
  )

#contour and tootip
viagens <- viagens %>%
  mutate(
    stroke_colour_static = "#C3C3C3",  # Grey contour for all polygons
    tooltip_viagens = paste0("Bairro: ", Nome, "<br>Produção: ", prod, "<br>Atração: ", atrat, "<br>Geração Total: ", geracao)
  )

# fill colors
geracao_values_for_palette <- viagens$geracao[viagens$geracao > 0]
geracao_domain <- if (length(geracao_values_for_palette) > 0) range(geracao_values_for_palette) else c(0, 1)

heat_colors_vector <- RColorBrewer::brewer.pal(9, "Reds") # using reds palette

geracao_palette_func <- scales::col_numeric(
  palette = heat_colors_vector,
  domain = geracao_domain
)

# creating column with the dynamic fill color, if 0 then transparent
viagens <- viagens %>%
  mutate(
    fill_colour_dynamic = ifelse(geracao > 0, geracao_palette_func(geracao), "#FFFFFF00")
  )

# exporting prod and atrat table
viagens_tabela <- viagens %>% st_drop_geometry() %>%
  dplyr::select(id, Nome, prod, atrat, geracao) %>% arrange(-geracao)

write.xlsx(viagens_tabela, "data/tabelas/1-tabela_geracao_viagens.xlsx")

# Fortaleza's contour

# dissolve bairros
viagens_union <- st_union(viagens) %>% st_make_valid()

# polygon boundary to a linestring
viagens_contour_line <- st_cast(viagens_union, "MULTILINESTRING")

# dummy attribute
viagens_contour_sf <- st_as_sf(viagens_contour_line) %>%
  mutate(id = "outer_contour")

#Loading GTFS to add route layer

gtfs <- read_gtfs(path = 'data/gtfs/GTFS_start_20240517.zip')
linhas <- gtfstools::convert_shapes_to_sf(gtfs)
line_2d <- linhas %>% filter(shape_id %like% "0075") %>% st_zm()

# line block 3d
block_width_meters <- 50  
block_height_meters <- 75 

line_projected <- st_transform(line_2d, crs = 31984)

line_buffer_projected <- st_buffer(line_projected, dist = block_width_meters)

line_buffer_wgs84 <- st_transform(line_buffer_projected, crs = 4326)

# color and heigth for mapdeck layer
line_block <- line_buffer_wgs84 %>%
  mutate(
    color = "#FFD700",   # Gold
    height = block_height_meters
  )

#correct epsg for mapdeck
viagens <- st_transform(viagens, crs = 4326)

# rendering of the map
#important to specify layer_id, otherwise some layers will incorrectly interact

mapa_od <- mapdeck(
  style = mapdeck_style("light"),
  location = c(-38.52, -3.73),
  zoom = 11,
  pitch = 20
) %>%
  # Layer 1: contour
  add_polygon(
    layer_id = "contorno",
    data = viagens_contour_sf,
    stroke_colour = "#434343",
    stroke_width = 100,
    fill_colour = "#FFFFFF00",
    auto_highlight = FALSE,
    update_view = FALSE,
  ) %>%
  # Layer 2: 3D bus line
  add_polygon(
    layer_id = "Linha",
    data = line_block,
    fill_colour = "color",
    extruded = TRUE,
    update_view = TRUE,
    elevation = "height",
    tooltip = "id"
  ) %>%
  # Layer 3: trip generation by neighborhood
  add_polygon(
    layer_id = "Geração de viagens",
    data = viagens,
    stroke_colour = "stroke_colour_static",
    stroke_width = 50,
    fill_colour = "fill_colour_dynamic",
    fill_opacity = 0.8,
    tooltip = "tooltip_viagens",
    update_view = FALSE,
    legend = FALSE, # Disable the legend
    legend_options = list(
      title = "Geração de Viagens",
      fill_colour = "geracao" # Specify that the legend's fill scale is based on the 'geracao' column
    )
  ) %>%
  
  # Layer 4: OD arcs specifying origin and destination coordinates (not sf object)
  add_arc(
    data = od_arc_data,
    origin = c("orig_x", "orig_y"),
    destination = c("dest_x", "dest_y"),
    stroke_from = "color_hex",
    stroke_to = "color_hex",
    stroke_width = "stroke_width",
    tooltip = "tooltip_label",
    auto_highlight = TRUE,
    update_view = FALSE,
    highlight_colour = "#FFC300FF",
    height = 0.3
  )

#show map
mapa_od

#save for offline rendering
saveWidget(mapa_od, file = "data/mapa_od/mapa_od_interativo.html", selfcontained = TRUE)

