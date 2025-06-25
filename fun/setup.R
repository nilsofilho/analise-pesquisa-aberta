#setup pacotes e temas mapas


library(stringdist)
library(stringi)
library(readxl)
library(sf)
library(openxlsx)
library(parquetize)
library(arrow)
library(dplyr)
library(data.table)
library(gtfstools)
library(mapview)
library(tidyr)
library(zoo)
library(stringr)
library(purrr)
library(furrr)
library(future)
library(lubridate)
library(lwgeom)
library(ggplot2)
library(googlesheets4)
library(data.table)
library(janitor)
library(dplyr)
library(sf)
library(stringi)
library(mapdeck)
library(scales)
library(glue)
library(ggplot2)
library(ggtext)
library(showtext)
library(httr)
library(jsonlite)
library(xlsx)
library(htmlwidgets)
library(hms)
library(zoo)
#not in function
`%nin%` <- Negate(`%in%`)

#cores etufor

laranja <- "#e65d42ff"
amarelo <- "#fdbe71ff"
azul <- "#5c60abff"
azul_claro <- "#a1cbe2"

# Configurar fontes
showtext_auto()
options(scipen = 100000000)
font_add("encode_sans", 'data/fontes/EncodeSans-VariableFont_wdth,wght.ttf')
font_add("encode_sans_regular", 'data/fontes/EncodeSans-Regular.ttf')
font_add("encode_sans_bold", 'data/fontes/EncodeSans-Bold.ttf')
font_add("encode_sans_light", 'data/fontes/EncodeSans-Light.ttf')

tema_barras_svg <- theme(
  # title =  ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),
  title = element_blank(),
  legend.position = "right",            # Colocar a legenda no lado direito
  axis.text.x = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.5),
  axis.title.x = element_blank(),       # Remover o título do eixo x
  axis.text.y = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.5), # Ajustar o tamanho da fonte dos rótulos do eixo y
  axis.title.y = ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5), # Ajustar o tamanho da fonte do título do eixo y
  legend.title = ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
  legend.text = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.5),     # Tamanho da fonte dos textos da legenda
)

tema_barras_svg_h <- theme(
  # title =  ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),
  title = element_blank(),
  legend.position = "right",            # Colocar a legenda no lado direito
  axis.text.x = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.1),
  axis.title.x = ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),   # Remover o título do eixo x
  axis.text.y = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.1), # Ajustar o tamanho da fonte dos rótulos do eixo y
  axis.title.y = element_blank(), # Ajustar o tamanho da fonte do título do eixo y
  legend.title = ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
  legend.text = ggtext::element_markdown(size=14, family = "encode_sans_light", lineheight = 0.5),     # Tamanho da fonte dos textos da legenda
)

tema_barras <- theme(
  # title =  ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),
  title = element_blank(),
  legend.position = "right",            # Colocar a legenda no lado direito
  axis.text.x = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.5),
  axis.title.x = element_blank(),       # Remover o título do eixo x
  axis.text.y = ggtext::element_markdown(size=40, family = "encode_sans_light", lineheight = 0.5), # Ajustar o tamanho da fonte dos rótulos do eixo y
  axis.title.y = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5), # Ajustar o tamanho da fonte do título do eixo y
  legend.title = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
  legend.text = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.5),     # Tamanho da fonte dos textos da legenda
)

tema_barras_h <- theme(
  # title =  ggtext::element_markdown(size=20, family = "encode_sans_bold", lineheight = 0.5),
  title = element_blank(),
  legend.position = "right",            # Colocar a legenda no lado direito
  axis.text.x = ggtext::element_markdown(size=40, family = "encode_sans_light", lineheight = 0.1),
  axis.title.x = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5),    # Remover o título do eixo x
  axis.text.y = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.1), # Ajustar o tamanho da fonte dos rótulos do eixo y
  axis.title.y = element_blank(), # Ajustar o tamanho da fonte do título do eixo y
  legend.title = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
  legend.text = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.1),     # Tamanho da fonte dos textos da legenda
)
