#Cálculo de indicadores operacional com gps de viagens da linha 075

rm(list= ls()) ;gc()

source("fun/setup.R")

linha <- 75

# Carregamento dos dados de GPS -------------------------------------------

gps <- setDT(read_parquet("data/viagens_marco/trips_075.parquet"))
#mostrar timestamp no tz de fortaleza
gps[, timestamp := lubridate::with_tz(timestamp, tzone = "America/Fortaleza")]

# Informações importantes do gtfs -----------------------------------------

gtfs <- read_gtfs("data/gtfs/GTFS_start_202503.zip")

#route gtfs
gtfs_linha <- gtfs %>% filter_by_route_id(str_pad(linha, width = 4, side = "left", pad = "0"))

rm(gtfs)

#route sf
stops_linha <- gtfstools::convert_stops_to_sf(gtfs_linha)
shapes_linha <- gtfstools::convert_shapes_to_sf(gtfs_linha)

ida <- gtfstools::convert_shapes_to_sf(gtfs_linha) %>%
  filter(shape_id == paste0("shape",str_pad(linha, width = 4, side = "left", pad = "0"),"-","I")) %>%
  st_as_sf()
volta <- gtfstools::convert_shapes_to_sf(gtfs_linha) %>%
  filter(shape_id == paste0("shape",str_pad(linha, width = 4, side = "left", pad = "0"),"-","V")) %>%
  st_as_sf()

trips_dt <- as.data.table(gtfs_linha$trips)
ida_trip_ids <- trips_dt[grepl("I", shape_id, ignore.case = TRUE), trip_id]
volta_trip_ids <- trips_dt[grepl("V", shape_id, ignore.case = TRUE), trip_id]


#sequências de paradas para cada sentido
stop_times_dt <- as.data.table(gtfs_linha$stop_times)

# Paradas e suas sequências para viagens de IDA
stops_ida_info <- stop_times_dt[trip_id %in% ida_trip_ids, .(stop_id, stop_sequence)] |> unique()

# Paradas e suas sequências para viagens de VOLTA
stops_volta_info <- stop_times_dt[trip_id %in% volta_trip_ids, .(stop_id, stop_sequence)] |> unique()

# Criar os objetos SF finais para cada sentido, juntando com a geometria
stops_ida_sf <- merge(stops_linha, stops_ida_info, by = "stop_id")
stops_volta_sf <- merge(stops_linha, stops_volta_info, by = "stop_id")

#visualizando as paradas por sentido e sequence
# mapview(stops_ida_sf, zcol = "stop_sequence")
# mapview(stops_volta_sf, zcol = "stop_sequence")

# Análises por GPS --------------------------------------------------------


# 1 - Tempo de viagem por dia da semana e sentido

try(Sys.setlocale("LC_TIME", "pt_BR.UTF-8"), silent = TRUE)
try(Sys.setlocale("LC_TIME", "Portuguese"), silent = TRUE)


#calculo do tempo de viagem e dia da semana

viagens_individuais <- gps %>%
  group_by(viagem_id_final) %>%
  summarise(
    horario_inicio = first(timestamp), 
    sentido = first(sentido),
    tempo_viagem_min = as.numeric(difftime(last(timestamp), first(timestamp), units = "mins")),
    .groups = 'drop'
  ) %>%
  mutate(dia_da_semana = wday(horario_inicio, label = TRUE, abbr = FALSE))

#médias por dia da semana
tempo_medio_dia_semana <- viagens_individuais %>%
  group_by(sentido, dia_da_semana) %>%
  summarise(
    tempo_medio_min = mean(tempo_viagem_min, na.rm = TRUE),
    .groups = 'drop'
  )

#media dias uteis
tempo_medio <- viagens_individuais %>%
  filter(dia_da_semana %nin% c("domingo","s<e1>bado")) %>%
  summarise(
    tempo_medio_min = mean(tempo_viagem_min, na.rm = TRUE),
    .groups = 'drop'
  )
#media dias uteis por sentido
tempo_medio_sentido <- viagens_individuais %>%
  group_by(sentido) %>%
  filter(dia_da_semana %nin% c("domingo","s<e1>bado")) %>%
  summarise(
    tempo_medio_min = mean(tempo_viagem_min, na.rm = TRUE),
    .groups = 'drop'
  )

#Cálculo da velocidade média operacional por viagem
extensao_ida <- shapes_linha %>% st_length()

vm_ida <- (as.numeric(extensao_ida[1])/1000)/(tempo_medio_sentido$tempo_medio_min[1]/60)
vm_volta <- (as.numeric(extensao_ida[2])/1000)/(tempo_medio_sentido$tempo_medio_min[2]/60)

#Tempo de viagem por faixa de horário

dados_grafico <- viagens_individuais %>%
  filter(!dia_da_semana %in% c("sábado", "domingo")) %>%
  mutate(
    hora_inicio = hour(horario_inicio), 
    faixa_periodo = case_when(
      hora_inicio >= 0 & hora_inicio < 5 ~ "Madrugada (00h-05h)",
      hora_inicio >= 5 & hora_inicio < 8 ~ "Pico Manhã (05h-08h)",
      hora_inicio >= 8 & hora_inicio < 11 ~ "Entre Picos Manhã (08h-11h)",
      hora_inicio >= 11 & hora_inicio < 14 ~ "Almoço (11h-14h)",
      hora_inicio >= 14 & hora_inicio < 17 ~ "Entre Picos Tarde (14h-17h)",
      hora_inicio >= 17 & hora_inicio < 19 ~ "Pico Noite (17h-19h)",
      hora_inicio >= 19 ~ "Noite (19h-00h)",
      TRUE ~ "Indefinido"
    )
  )


sumario_grafico <- dados_grafico %>%
  group_by(sentido, faixa_periodo) %>%
  summarise(tempo_medio_min = mean(tempo_viagem_min, na.rm = TRUE), .groups = 'drop') %>%
  filter(!is.na(faixa_periodo) & faixa_periodo != "Indefinido")

niveis_periodo <- c(
  "Madrugada (00h-05h)", "Pico Manhã (05h-08h)", "Entre Picos Manhã (08h-11h)",
  "Almoço (11h-14h)", "Entre Picos Tarde (14h-17h)", "Pico Noite (17h-19h)",
  "Noite (19h-00h)"
)
sumario_grafico$faixa_periodo <- factor(sumario_grafico$faixa_periodo, levels = niveis_periodo)

grafico_tempos_viagem <- ggplot(sumario_grafico, aes(x = faixa_periodo, y = tempo_medio_min, fill = sentido)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::number_format(accuracy = 0.1)(tempo_medio_min)),
            vjust = -0.5,
            size = 12,
            family = "encode_sans_light") + 
  scale_fill_manual(name = "Sentido",
                    breaks = c("I", "V"),
                    values = c("#00a394", "darkred"),
                    labels = c("I"="Ida (Unifor)",
                               "V"="Volta (Kennedy)")) +
  facet_wrap(~ sentido, scales = "free_y") +
  theme_minimal() +
  tema_barras +
  theme(
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5),
    axis.text.y = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.5), # Ajustar o tamanho da fonte dos rótulos do eixo y
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 36),
    legend.title = ggtext::element_markdown(size=44, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
    legend.text = ggtext::element_markdown(size=36, family = "encode_sans_light", lineheight = 0.5),
    legend.position = "right"
  ) +
  labs(
    y = "Tempo Médio de Viagem (Minutos)"
  )

grafico_tempos_viagem_svg <- ggplot(sumario_grafico, aes(x = faixa_periodo, y = tempo_medio_min, fill = sentido)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = scales::number_format(accuracy = 0.1)(tempo_medio_min)),
            vjust = -0.5,
            size = 5,
            family = "encode_sans_light") + 
  scale_fill_manual(name = "Sentido",
                    breaks = c("I", "V"),
                    values = c("#00a394", "darkred"),
                    labels = c("I"="Ida (Unifor)",
                               "V"="Volta (Kennedy)")) +
  facet_wrap(~ sentido, scales = "free_y") +
  theme_minimal() +
  tema_barras_svg +
  theme(
    strip.text = element_blank(),
    axis.title.y = ggtext::element_markdown(size=12, family = "encode_sans_bold", lineheight = 0.5),
    axis.text.y = ggtext::element_markdown(size=12, family = "encode_sans_light", lineheight = 0.5), # Ajustar o tamanho da fonte dos rótulos do eixo y
    axis.text.x = element_markdown(angle = 45, hjust = 1, size = 10),
    legend.title = ggtext::element_markdown(size=14, family = "encode_sans_bold", lineheight = 0.5),   # Tamanho da fonte do título da legenda
    legend.text = ggtext::element_markdown(size=12, family = "encode_sans_light", lineheight = 0.5),
    legend.position = "right"
  ) +
  labs(
    y = "Tempo Médio de Viagem (Minutos)"
  )

#Salvar o gráfico

ggsave(grafico_tempos_viagem, 
       file= 'data/graficos/6-tempo_viagem_gps.png', 
       dpi = 300, width = 22, height = 15, units = "cm")

ggsave(grafico_tempos_viagem_svg, 
       file= 'data/graficos/6-tempo_viagem_gps.svg',
       width = 22, height = 15, units = "cm")



# Headway executado por dia da semana e faixa de horário ------------------



# paradas inicial e final por sentido
limites_sequencia <- gps %>%
  filter(!is.na(stop_sequence)) %>%
  group_by(sentido) %>%
  summarise(
    min_seq = min(stop_sequence, na.rm = TRUE),
    max_seq = max(stop_sequence, na.rm = TRUE),
    .groups = 'drop'
  )

#segunda e penúltima paradas são mais consistentes
sequencias_alvo <- tibble(
  sentido = c(limites_sequencia$sentido, limites_sequencia$sentido),
  stop_sequence = c(limites_sequencia$min_seq + 1, limites_sequencia$max_seq - 1)
)


paradas_alvo <- gps %>%
  select(sentido, stop_id, stop_sequence) %>%
  inner_join(sequencias_alvo, by = c("sentido", "stop_sequence")) %>%
  distinct(stop_id) %>%
  pull(stop_id)

#calcular o headway das viagens por sentido nas paradas selecionadas
headways_df_paradas_selecionadas <- gps %>%
  filter(stop_id %in% paradas_alvo) %>%
  
  arrange(stop_id, sentido, timestamp) %>%
  group_by(stop_id, sentido) %>%
  mutate(
    headway_min = as.numeric(difftime(lead(timestamp), timestamp, units = "mins"))
  ) %>%
  filter(!is.na(headway_min)) %>%
  ungroup() %>%
  mutate(
    dia_da_semana = wday(timestamp, label = TRUE, abbr = FALSE),
    hora_do_dia = hour(timestamp)
  )

#Headway médio por dia da semana

headway_medio_dia_semana <- headways_df_paradas_selecionadas %>%
  group_by(sentido, dia_da_semana) %>%
  summarise(
    headway_medio_min = mean(headway_min, na.rm = TRUE),
    .groups = 'drop'
  )

#headway médio dias úteis
headways_realizados <- headway_medio_dia_semana %>%
  filter(dia_da_semana %nin% c("domingo", "s<e1>bado")) %>%
  group_by(sentido) %>%
  summarise(head_medio = mean(headway_medio_min))

#valor esperado do tempo de espera por sentido considerando chegada uniforme
headways_realizados$head_medio/2

#Gráfico de headways com ggplot2

# Identificar a primeira e última parada de cada sentido
# limites_paradas <- gps %>%
#   group_by(sentido) %>%
#   summarise(
#     primeira_parada = stop_id[which.min(stop_sequence)],
#     ultima_parada = stop_id[which.max(stop_sequence)]
#   )

# Vetor das paradas de interesse
paradas_selecionadas <- c("937", "4772", "7537", "3244", "5568", "2081")

# Nomes das paradas para exibição no gráfico
nomes_paradas_ordem_legenda  <- c(
  "RioMar Kennedy",
  "Benfica (Ida)",
  "Benfica (Volta)",
  "FAECE/Fafor"
)

paradas_ordem_barras <- c(
  "937",  # RioMar Kennedy (Ida)
  "4772", # RioMar Kennedy (Volta)
  "5568", # Benfica (Ida)
  "2081", # Benfica (Volta)
  "7537", # FAECE/Fafor (Ida)
  "3244"  # FAECE/Fafor (Volta)
)

sumario_grafico_headway <- headways_df %>%
  filter(
    hora_do_dia >= 5 & hora_do_dia < 22,
    !dia_da_semana %in% c("sábado", "domingo"),
    stop_id %in% paradas_selecionadas
  ) %>%
  mutate(
    sentido = recode(sentido, "I" = "Ida", "V" = "Volta"),
    nome_parada = case_when(
      stop_id %in% c("937", "4772") ~ "RioMar Kennedy",
      stop_id %in% c("7537", "3244") ~ "FAECE/Fafor",
      stop_id == "5568" ~ "Benfica (Ida)",
      stop_id == "2081" ~ "Benfica (Volta)"
    ),
    faixa_periodo = case_when(
      hora_do_dia >= 5 & hora_do_dia < 8 ~ "Pico Manhã (05h-08h)",
      hora_do_dia >= 8 & hora_do_dia < 11 ~ "Entre Picos Manhã (08h-11h)",
      hora_do_dia >= 11 & hora_do_dia < 14 ~ "Almoço (11h-14h)",
      hora_do_dia >= 14 & hora_do_dia < 17 ~ "Entre Picos Tarde (14h-17h)",
      hora_do_dia >= 17 & hora_do_dia < 19 ~ "Pico Noite (17h-19h)",
      hora_do_dia >= 19 & hora_do_dia < 22 ~ "Noite (19h-22h)"
    ),
    
    nome_parada = factor(nome_parada, levels = nomes_paradas_ordem_legenda),
    stop_id = factor(stop_id, levels = paradas_ordem_barras)
  ) %>%
  filter(!is.na(faixa_periodo)) %>%
  group_by(sentido, stop_id, nome_parada, faixa_periodo) %>%
  summarise(headway_medio_min = mean(headway_min, na.rm = TRUE), .groups = 'drop')

niveis_periodo_ajustado <- c("Pico Manhã (05h-08h)", "Entre Picos Manhã (08h-11h)", "Almoço (11h-14h)", "Entre Picos Tarde (14h-17h)", "Pico Noite (17h-19h)", "Noite (19h-22h)")
sumario_grafico_headway$faixa_periodo <- factor(sumario_grafico_headway$faixa_periodo, levels = niveis_periodo_ajustado)

grafico_headway <- ggplot(sumario_grafico_headway,
                          aes(x = faixa_periodo,
                              y = headway_medio_min,
                              fill = nome_parada,
                              group = stop_id)) + 
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  
  scale_fill_manual(
    name = "Ponto de Parada",
    breaks = nomes_paradas_ordem_legenda, 
    values = c(
      "RioMar Kennedy" = azul,
      "Benfica (Ida)" = laranja,
      "Benfica (Volta)" = amarelo,
      "FAECE/Fafor" = azul_claro
    )
  ) +
  
  facet_wrap(~ sentido) +
  
  geom_text(aes(label = scales::number_format(accuracy = 1)(headway_medio_min)),
            vjust = -0.5,
            size = 8,
            position = position_dodge(width = 0.9),
            stat = "identity",
            family = "encode_sans_light") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5), 
    axis.title.y = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5), 
    axis.text.y = element_text(size = 36, family = "encode_sans_light", lineheight = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1, size = 36), 
    legend.title = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5), 
    legend.text = element_text(size = 36, family = "encode_sans_light", lineheight = 0.5), 
    legend.position = "right"
  ) +
  labs(
    y = "Intervalo Médio (Minutos)",
    x = NULL
  )



#Salvar o gráfico png

ggsave(grafico_headway, 
       file= 'data/graficos/7-headway_gps.png', 
       dpi = 300, width = 22, height = 15, units = "cm")

#tempo de espera na parada - considerando chegada uniforme (h/2)

sumario_grafico_espera <- sumario_grafico_headway %>%
  mutate(tempo_espera_medio = headway_medio_min / 2)

grafico_tempo_espera <- ggplot(
  sumario_grafico_espera, 
  aes(x = faixa_periodo,
      y = tempo_espera_medio, 
      fill = nome_parada,
      group = stop_id)) +
  geom_bar(stat = "identity",
           position = position_dodge(width = 0.9)) +
  
  scale_fill_manual(
    name = "Ponto de Parada",
    breaks = nomes_paradas_ordem_legenda,
    values = c(
      "RioMar Kennedy" = azul,
      "Benfica (Ida)" = laranja,
      "Benfica (Volta)" = amarelo,
      "FAECE/Fafor" = azul_claro
    )
  ) +
  
  facet_wrap(~ sentido) +
  
  geom_text(aes(label = scales::number_format(accuracy = 1)(tempo_espera_medio)),
            vjust = -0.5,
            size = 8,
            position = position_dodge(width = 0.9),
            stat = "identity",
            family = "encode_sans_light") +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5),
    axis.title.y = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5),
    axis.text.y = element_text(size = 36, family = "encode_sans_light", lineheight = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 36),
    legend.title = element_text(size = 44, family = "encode_sans_bold", lineheight = 0.5),
    legend.text = element_text(size = 36, family = "encode_sans_light", lineheight = 0.5),
    legend.position = "right"
  ) +
  labs(
    y = "Tempo de Espera Médio (min)",
    x = NULL
  )

ggsave(grafico_tempo_espera, 
       file= 'data/graficos/8-tempo_espera_gps.png', 
       dpi = 300, width = 22, height = 15, units = "cm")


#Histograma do headway por faixa de horário

headways_para_histograma <- headways_df %>%
  filter(
    !stop_id %in% c("6404", "6303"),
    hora_do_dia >= 5 & hora_do_dia < 22,
    !dia_da_semana %in% c("sábado", "domingo")
  ) %>%
  mutate(
    sentido = recode(sentido, "I" = "Ida", "V" = "Volta"),
    faixa_periodo = case_when(
      hora_do_dia >= 5 & hora_do_dia < 8 ~ "Pico Manhã (05h-08h)",
      hora_do_dia >= 8 & hora_do_dia < 11 ~ "Entre Picos Manhã (08h-11h)",
      hora_do_dia >= 11 & hora_do_dia < 14 ~ "Almoço (11h-14h)",
      hora_do_dia >= 14 & hora_do_dia < 17 ~ "Entre Picos Tarde (14h-17h)",
      hora_do_dia >= 17 & hora_do_dia < 19 ~ "Pico Noite (17h-19h)",
      hora_do_dia >= 19 & hora_do_dia < 22 ~ "Noite (19h-22h)"
    )
  ) %>%
  filter(!is.na(faixa_periodo), headway_min < 60)

medias_por_periodo <- headways_para_histograma %>%
  group_by(sentido, faixa_periodo) %>% 
  summarise(headway_medio = mean(headway_min, na.rm = TRUE), .groups = 'drop')


# ordem dos painéis
niveis_periodo_ajustado <- c("Pico Manhã (05h-08h)", "Entre Picos Manhã (08h-11h)", "Almoço (11h-14h)", "Entre Picos Tarde (14h-17h)", "Pico Noite (17h-19h)", "Noite (19h-22h)")
headways_para_histograma$faixa_periodo <- factor(headways_para_histograma$faixa_periodo, levels = niveis_periodo_ajustado)
medias_por_periodo$faixa_periodo <- factor(medias_por_periodo$faixa_periodo, levels = niveis_periodo_ajustado)


histograma_distribuicao_sentido <- ggplot(
  headways_para_histograma, 
  aes(x = headway_min)
) +
  geom_histogram(binwidth = 5, fill = "#00a394", color = "white", alpha = 0.9) +
  
  geom_vline(
    data = medias_por_periodo,
    aes(xintercept = headway_medio),
    color = "darkred",
    linetype = "dashed",
    linewidth = 1
  ) +
  
  geom_text(
    data = medias_por_periodo,
    aes(x = headway_medio + 5, y = Inf, label = paste("Média:", round(headway_medio, 1))),
    color = "darkred",
    vjust = 2,
    hjust = 0,
    size = 5 
  ) +
  
  facet_grid(sentido ~ faixa_periodo, scales = "free_y") +
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 34, family = "encode_sans_bold"),
    axis.title.y = element_text(size = 34, family = "encode_sans_bold"),
    axis.text.y = element_text(size = 30, family = "encode_sans_bold"),
    axis.text.x = element_text(angle = 0, hjust = 1, size = 30, family = "encode_sans_bold"),
    axis.title.x = element_text(angle = 0, size = 34, family = "encode_sans_bold"),
  ) +
  labs(
    x = "Headway (Minutos)",
    y = "Frequência"
  )


ggsave(histograma_distribuicao_sentido, 
       file= 'data/graficos/9-histograma_headway_gps.png', 
       dpi = 300, width = 22, height = 15, units = "cm")


#histograma separado por sentido

dados_ida <- headways_para_histograma %>% filter(sentido == "Ida")
medias_ida <- medias_por_periodo %>% filter(sentido == "Ida")

histograma_ida <- ggplot(dados_ida, aes(x = headway_min)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))), # Calcula a proporção
    binwidth = 2, fill = "#00a394", color = "white", alpha = 0.9
  ) +
  geom_vline(
    data = medias_ida, aes(xintercept = headway_medio),
    color = "darkred", linetype = "dashed", linewidth = 1
  ) +
  facet_wrap(~ faixa_periodo, scales = "free_y") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22, family = "encode_sans_bold"),
    axis.title.y = element_text(size = 24, family = "encode_sans_bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16)
  ) +
  labs(
    title = "Distribuição do Headway por Período - Sentido IDA",
    x = "Headway (Minutos)",
    y = "Frequência (%)"
  )

dados_volta <- headways_para_histograma %>% filter(sentido == "Volta")
medias_volta <- medias_por_periodo %>% filter(sentido == "Volta")

histograma_volta <- ggplot(dados_volta, aes(x = headway_min)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 2, fill = "#00a394", color = "white", alpha = 0.9
  ) +
  geom_vline(
    data = medias_volta, aes(xintercept = headway_medio),
    color = "darkred", linetype = "dashed", linewidth = 1
  ) +
  facet_wrap(~ faixa_periodo, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    strip.text = element_text(size = 22, family = "encode_sans_bold"),
    axis.title.y = element_text(size = 24, family = "encode_sans_bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16)
  ) +
  labs(
    title = "Distribuição do Headway por Período - Sentido VOLTA",
    x = "Headway (Minutos)",
    y = "Frequência (%)"
  )

#histograma considerando apenas picos e almoco
headways_detalhe <- headways_para_histograma %>%
  filter(faixa_periodo %in% c(
    "Pico Manhã (05h-08h)",
    "Almoço (11h-14h)",
    "Pico Noite (17h-19h)"
  ))

medias_detalhe <- headways_detalhe %>%
  group_by(sentido, faixa_periodo) %>%
  summarise(headway_medio = mean(headway_min, na.rm = TRUE), .groups = 'drop')

histograma_detalhe <- ggplot(headways_detalhe, aes(x = headway_min)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count))),
    binwidth = 5, fill = "#00a394", color = "white", alpha = 0.9
  ) +
  geom_vline(
    data = medias_detalhe, aes(xintercept = headway_medio),
    color = "darkred", linetype = "dashed", linewidth = 1
  ) +
  geom_text(
    aes(
      x = headway_medio, 
      y = Inf, 
      label = round(headway_medio, 1) 
    ),
    color = "darkred",
    vjust = 2,    
    hjust = -0.1, 
    size = 8,     
    family = "encode_sans_bold"
  ) +
  
  facet_grid(sentido ~ faixa_periodo, scales = "free_y") +
  
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  
  theme_minimal() +
  theme(
    strip.text = element_text(size = 34, family = "encode_sans_bold"),
    axis.title.y = element_text(size = 34, family = "encode_sans_bold"),
    axis.text.y = element_text(size = 30, family = "encode_sans_bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, size = 30, family = "encode_sans_bold"),
    axis.title.x = element_text(angle = 0, size = 34, family = "encode_sans_bold")
  ) +
  labs(
    x = "Headway (Minutos)",
    y = "Frequência (%)"
  )

ggsave(histograma_detalhe, 
       file= 'data/graficos/10-histograma_headway_detalhe.png', 
       dpi = 300, width = 22, height = 15, units = "cm")







