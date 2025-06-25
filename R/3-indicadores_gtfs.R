#Análise dos dados de GPS (usando maio/2025)


rm(list =ls()); gc()
# --- 1. LOAD NECESSARY LIBRARIES ---
source("fun/setup.R")

# GTFS --------------------------------------------------------------------

gtfs <- read_gtfs(path = "data-raw/GTFS_start_202503.zip")

gtfs_75 <- gtfstools::filter_by_route_id(gtfs,"0075") %>% filter_by_service_id('U')


# Calcular headway programado -------------------------------------------

stop_times_75 <- gtfs_75$stop_times
trips_75 <- gtfs_75$trips

#corrigindo formato do departure_time e preenchendo vazios
partidas_programadas <- stop_times_75 %>%
  inner_join(trips_75, by = "trip_id") %>%
  
  select(trip_id, departure_time, stop_id, shape_id, stop_sequence) %>%
  
  mutate(
    sentido = stri_sub(shape_id, -1),
    
    departure_time = if_else(nchar(departure_time) < 6, NA_character_, departure_time),
    # converter departure time para hms
    departure_time = suppressWarnings(as_hms(departure_time))
  ) %>%
  
  # interpolando vazios
  group_by(trip_id) %>% 
  arrange(stop_sequence) %>% 
  
  mutate(departure_time = na.approx(departure_time, na.rm = FALSE)) %>%
  
  # preencher NAs que possam ter sobrado no início ou fim da viagem
  fill(departure_time, .direction = "down") %>%
  fill(departure_time, .direction = "up") %>%
  
  ungroup() %>% 
  
  filter(sentido %in% c("I", "V")) %>%
  select(trip_id, departure_time, stop_id, sentido)

#calculo dos headways por stop_is e sentido

headways_programados <- partidas_programadas %>%
  arrange(stop_id, sentido, departure_time) %>%
  group_by(stop_id, sentido) %>%
  
  mutate(
    headway_min = (lead(departure_time) - departure_time) / 60
  ) %>%
  
  filter(!is.na(headway_min)) %>%
  ungroup()


# determinar faixa de horário
headway_medio_programado <- headways_programados %>%
  mutate(
    hora_partida = departure_time / 3600,
    faixa_periodo = case_when(
      hora_partida >= 5 & hora_partida < 8 ~ "Pico Manhã (05h-08h)",
      hora_partida >= 8 & hora_partida < 11 ~ "Entre Picos Manhã (08h-11h)",
      hora_partida >= 11 & hora_partida < 14 ~ "Almoço (11h-14h)",
      hora_partida >= 14 & hora_partida < 17 ~ "Entre Picos Tarde (14h-17h)",
      hora_partida >= 17 & hora_partida < 19 ~ "Pico Noite (17h-19h)",
      hora_partida >= 19 & hora_partida < 22 ~ "Noite (19h-22h)"
    )
  ) %>%
  filter(!is.na(faixa_periodo)) %>%
  group_by(faixa_periodo, sentido) %>%
  summarise(
    headway_medio_programado_min = mean(headway_min, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    sentido = recode(sentido, "I" = "Ida", "V" = "Volta")
  ) %>%
  select(sentido, faixa_periodo, headway_medio_programado_min)


#headway medio por sentido, dias úteis
headways_programados_sentido <- headway_medio_programado %>%
  group_by(sentido) %>%
  summarise(head_mean = mean(headway_medio_programado_min))

#valor esperado do tempo de espera considerando chegada uniforme
headways_programados_sentido$head_mean/2




# Cálculo do tempo de viagem e velocidades programadas --------------------

#usando a função trip duration do gtfstools
trip_duration <- gtfstools::get_trip_duration(gtfs_75) %>%
  #join com a tabela trips para resgatar informaçaõ do sentido da viagem no stoptimes
  inner_join(trips_75, by = "trip_id") %>%
  select(trip_id, duration, shape_id) %>%
  mutate(
    sentido = stri_sub(shape_id, -1)) %>% group_by(sentido) %>%
  summarise(duration = mean(duration))

#média
mean(trip_duration$duration, na.rm = T)

#velocidades médias
vm_ida_prog <- (as.numeric(extensao_ida[1])/1000)/(trip_duration$duration[1]/60)
vm_volta_prog <- (as.numeric(extensao_ida[2])/1000)/(trip_duration$duration[2]/60)

#testes com outros tempos de viagem
(as.numeric(extensao_ida[1])/1000)/(96.1/60)

(as.numeric(extensao_ida[1])/1000)/(16.7/60)



