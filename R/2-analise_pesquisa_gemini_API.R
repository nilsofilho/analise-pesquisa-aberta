rm(list =ls()); gc()

source("fun/setup.R")
source("fun/gemini.R")




# Survey analysis -------------------------------------------------------------

# Load google sheets data with google OAuth
dados <- read_sheet('1fAiHo6XCKSJF_gi79VyXvyE70Mmh-VYWAh-bYn__qo8') %>%
  setDT() %>%
  clean_names()

#phones with only numbers
dados[, (grep("x1", names(dados), value = TRUE)) := lapply(.SD, function(x) gsub("[^0-9]", "", x)), .SDcols = grep("x1", names(dados), value = TRUE)]


#ANÁLISE DAS PERGUNTAS


# 1 - Frequência de uso, pergunta x2 --------------------------------------

col_freq <- grep("x2", names(dados), value = TRUE)

freq_semanal <- dados[, .(freq = .N), by = col_freq]
freq_semanal[, user_percent := freq/sum(freq)]

names(freq_semanal) <- c("classe", "freq", "user_percent")

#adding <br> for better visualization and render with ggtext::element_markdown
#better control of lineheigth and font rendering with showtext
freq_semanal[, classe := case_when(classe == "5 ou mais dias" ~ "5 ou mais dias<br>por semana",
                                     classe == "3 ou 4 dias por semana" ~ "3 ou 4 dias<br>por semana",
                                     classe == "1 ou 2 dias por semana" ~ "1 ou 2 dias<br>por semana",
                                     classe == "Raramente/menos de uma vez por semana" ~ "Raramente/menos de 1<br>vez por semana")]

freq_semanal$classe <- factor(freq_semanal$classe,
                                levels = c("Raramente/menos de 1<br>vez por semana",
                                           "1 ou 2 dias<br>por semana",
                                           "3 ou 4 dias<br>por semana",
                                           "5 ou mais dias<br>por semana"))

#gráfico de frequência usando ggplot2 em svg e png

grafico_frequencia_svg <- ggplot(freq_semanal, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100)(user_percent)),
            vjust = -0.5,
            size = 5,
            family = "encode_sans_light") +  
  labs(title = "Frequência de uso semanal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  
  theme_minimal() +
  tema_barras_svg

grafico_frequencia <- ggplot(freq_semanal, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100)(user_percent)),
            vjust = -0.5,
            size = 15,
            family = "encode_sans_light") +  # Adiciona porcentagem nas barras
  labs(title = "Frequência de uso semanal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  # Formatar eixo y em porcentagem
  theme_minimal() +
  tema_barras

#Salvar o gráfico

ggsave(grafico_frequencia, 
       file= 'data/graficos/1-frequencia_uso_semanal.png', 
       dpi = 300, width = 22, height = 15, units = "cm")
  
ggsave(grafico_frequencia_svg, 
       file= 'data/graficos/1-frequencia_uso_semanal.svg',
       width = 22, height = 15, units = "cm")


#principais horários de uso

# 2 - horário de uso, pergunta x3 -----------------------------------------


dados_horario <- dados

#remover respostas duplicadas
col_id_nome <- grep("x1", names(dados_horario), value = TRUE)

dados_deduplicados <- unique(dados, by = col_id_nome)
message(paste("Número de linhas original em 'dados':", nrow(dados)))
message(paste("Número de linhas após deduplicação por telefone ('dados_deduplicados'):", nrow(dados_deduplicados)))

#column name
col_horario_nome <- grep("x3", names(dados_deduplicados), value = TRUE)

#desaninhar respostas
colunas_para_agrupar_e_manter <- names(dados_deduplicados)[names(dados_deduplicados) != col_horario_nome]

dados_expandidos <- dados_deduplicados[, .(
  horario_individual = trimws(unlist(strsplit(as.character(get(col_horario_nome)), ",")))
),
by = colunas_para_agrupar_e_manter # Agrupa por todas as outras colunas
]

dados_expandidos <- dados_expandidos[!is.na(horario_individual) & horario_individual != ""]

#frequencia por faixa de horário
horario <- dados_expandidos[, .(freq = .N), by = horario_individual]
horario[, user_percent := freq/sum(freq)]

names(horario) <- c("classe", "freq", "user_percent")

horario[, classe := case_when(classe == "Madrugada (00h às 05h)" ~ "Madrugada<br>(00h às 05h)",
                                   classe == "Pico da manhã (05h às 08h)" ~ "Pico da manhã<br>(05h às 08h)",
                                   classe == "Entre picos manhã (08h às 11h)" ~ "Entre picos manhã<br>(08h às 11h)",
                                   classe == "Almoço (11h às 14h)" ~ "Almoço<br>(11h às 14h)",
                              classe == "Entre picos tarde (14h às 17h)" ~ "Entre picos tarde<br>(14h às 17h)",
                              classe == "Pico noite (17h às 19h)" ~ "Pico noite<br>(17h às 19h)",
                              classe == "Noite (19h às 00h)" ~ "Noite<br>(19h às 00h)")]

horario$classe <- factor(horario$classe,
                              levels = c("Madrugada<br>(00h às 05h)",
                                         "Pico da manhã<br>(05h às 08h)",
                                         "Entre picos manhã<br>(08h às 11h)",
                                         "Almoço<br>(11h às 14h)" ,
                                         "Entre picos tarde<br>(14h às 17h)",
                                         "Pico noite<br>(17h às 19h)",
                                         "Noite<br>(19h às 00h)"))

#gráfico de horário usando ggplot2 em png e svg

grafico_horario_svg <- ggplot(horario, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100)(user_percent)),
            vjust = -0.5,
            size = 5,
            family = "encode_sans_light") +
  labs(title = "Horário de uso", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  theme_minimal() +
  tema_barras_svg + 
  theme(axis.text.x = ggtext::element_markdown(size=14,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 45,
                                               vjust = 0.5))



grafico_horario <- ggplot(horario, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100)(user_percent)),
            vjust = -0.5,
            size = 15,
            family = "encode_sans_light") + 
  labs(title = "Horário de uso", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +  
  theme_minimal() +
  tema_barras + 
  theme(axis.text.x = ggtext::element_markdown(size=30,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 45,
                                               vjust = 0.5))


#Salvar o gráfico

ggsave(grafico_horario, 
       file= 'data/graficos/2-horario_uso.png', 
       dpi = 300, width = 15, height = 15, units = "cm")

ggsave(grafico_horario_svg, 
       file= 'data/graficos/2-horario_uso.svg',
       width = 22, height = 15, units = "cm")



#Principal problema:

# 3 - Principal problema, pergunta x6 semiaberta --------------------------


dados_prob_principal <- dados

col_id_nome <- grep("x1", names(dados_horario), value = TRUE)

#remover respostas duplicadas
dados_deduplicados_prob_prin <- unique(dados, by = col_id_nome)
message(paste("Número de linhas original em 'dados':", nrow(dados)))
message(paste("Número de linhas após deduplicação por telefone ('dados_deduplicados'):", nrow(dados_deduplicados)))

#col name
col_probprin_nome <- grep("x6", names(dados_deduplicados_prob_prin), value = TRUE)


#filtrar problemas que foram inseridos na modalidade outros

problemas_padrao <- c("Lotação",
                      "Tempo dentro do veículo",
                      "Tempo de espera na parada",
                      "Tempo de espera no terminal",
                      "Tempo de caminhada até a parada",
                      "Tempo de caminhada na integração",
                      "Previsibilidade (ônibus chega no horário previsto)",
                      "Conforto no veículo",
                      "Custo da passagem",
                      "Segurança (risco de assalto)")

problemas_custom <- dados_deduplicados_prob_prin %>% filter(!!sym(col_probprin_nome) %nin% problemas_padrao)

#criação do prompt do gemini para cada resposta customizada

problemas_custom <- problemas_custom %>%
mutate(prompt = paste("Por favor, classifique o seguinte problema reportado por um usuário de transporte público:",
                      !!sym(col_probprin_nome),      
"As categorias predefinidas são:
- Lotação
- Tempo dentro do veículo
- Tempo de espera na parada
- Tempo de espera no terminal
- Tempo de caminhada até a parada
- Tempo de caminhada na integração
- Previsibilidade (ônibus chega no horário previsto)
- Conforto no veículo (inclui ar-condicionado, limpeza)
- Custo da passagem
- Segurança (risco de assalto)

Se a resposta se encaixar em uma dessas categorias, retorne somente o nome da categoria.
Se a resposta não se encaixar bem mas for um problema válido de transporte público, sugira uma nova categoria concisa ou retorne 'NOVA CATEGORIA: [sugestão]'.
Se a resposta for uma brincadeira, sem sentido, ou claramente não relacionada a um problema de transporte, retorne 'DESCONSIDERAR'.
Se a resposta indicar múltiplos problemas, retorne a categoria do problema principal ou as categorias separadas por ';'."))

#teste com uma respsota
teste_gemini <- gemini(prompt = problemas_custom$prompt[1])

#for debug
i = 2

#formata o resultado do prompt do gemini
aplica_gemini <- function(i){
  
  j <- problemas_custom$prompt[i]
  Sys.sleep(3)
  #função de consulta ao gemini, necessario informar api key no primeiro uso
  #possivel ajustar o modelo usado pelo parâmetro "model"
  result <- gemini(j)
  result2 <- trimws(as.character(result))
  return(result2)
}

#respostas categorizadas pelo gemini
vector_results <- lapply(X = 1:length(problemas_custom$prompt), aplica_gemini)

problemas_custom2 <- problemas_custom %>% mutate(problema_gemini = vector_results) %>% dplyr::select(-prompt)

#Expandindo respostas que retornaram mais de uma categoria
colunas_para_agrupar_e_manter <- names(problemas_custom2)[names(problemas_custom2) != "problema_gemini"]

problemas_custom2_expandidos <- problemas_custom2[, .(
  problema_principal_individual = trimws(unlist(strsplit(as.character(get("problema_gemini")), ";")))
),
by = colunas_para_agrupar_e_manter
]

problemas_custom2_expandidos <- problemas_custom2_expandidos[!is.na(problema_principal_individual) & problema_principal_individual != ""]

problemas_custom2_expandidos <- problemas_custom2_expandidos %>% filter(problema_principal_individual != "DESCONSIDERAR")

#juntando com os problemas no padrão correto
#nota: usar !!sym("colname_string") no dplyr r
principal_problema_final <- rbind(dados_deduplicados_prob_prin %>%
                                    filter(!!sym(col_probprin_nome) %in% problemas_padrao),
                                  problemas_custom2_expandidos %>%
                                    mutate(!!sym(col_probprin_nome) := problema_principal_individual) %>%
                                    dplyr::select(names(dados_deduplicados_prob_prin))) %>%
  mutate(!!sym(col_probprin_nome) := case_when(!!sym(col_probprin_nome)=="CONFORTO NO VEÍCULO"~"Conforto no veículo",
                                               TRUE~!!sym(col_probprin_nome)))

#verificar nome dos modelos gemini
# available_gemini_models <- list_gemini_models()

#cálculo da frequência
problema_principal <- principal_problema_final[, .(freq = .N), by = col_probprin_nome]

problema_principal[, user_percent := freq/sum(freq)]

names(problema_principal) <- c("classe", "freq", "user_percent")

#remover o texto nova categoria
problema_principal <- problema_principal %>% mutate(classe = sub("NOVA CATEGORIA: ", "", classe, fixed = TRUE))

#as.factor decrescente por frequencia
problema_principal[, classe := factor(classe, levels = problema_principal[order(-freq), classe])]

#gráfico de problema principal usando ggplot2 em svg e png

grafico_problema_principal <- ggplot(problema_principal, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 15,
            family = "encode_sans_light") +  
  labs(title = "Problema principal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(problema_principal$user_percent)+0.1)) +  
  theme_minimal() +
  coord_flip() +
  tema_barras_h + 
  theme(axis.text.x = ggtext::element_markdown(size=30,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 0,
                                               vjust = 0.5))
 
grafico_problema_principal_svg <- ggplot(problema_principal, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 5,
            family = "encode_sans_light") +  
  labs(title = "Problema principal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(problema_principal$user_percent)+0.1)) +  
  theme_minimal() +
  coord_flip() +
  tema_barras_svg_h + 
  theme(axis.text.x = ggtext::element_markdown(size=14,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 45,
                                               vjust = 0.5))

#Salvar o gráfico

ggsave(grafico_problema_principal, 
       file= 'data/graficos/3-principal_problema.png', 
       dpi = 300, width = 22, height = 15, units = "cm")

ggsave(grafico_problema_principal_svg, 
       file= 'data/graficos/3-principal_problema.svg',
       width = 22, height = 15, units = "cm")



#4. Análise dos principais problemas (Campo semiaberto x7)


dados_problemas <- dados

col_id_nome <- grep("x1", names(dados_horario), value = TRUE)

dados_deduplicados_problemas <- unique(dados, by = col_id_nome)
message(paste("Número de linhas original em 'dados':", nrow(dados)))
message(paste("Número de linhas após deduplicação por telefone ('dados_deduplicados'):", nrow(dados_deduplicados)))


col_problemas <- grep("x7", names(dados_deduplicados_prob_prin), value = TRUE)


#Ate 3 problemas por respostas, colocar cada problema em uma linha dos dados

colunas_para_agrupar_e_manter <- names(dados_deduplicados_problemas)[names(dados_deduplicados_problemas) != col_problemas]

problemas_expandidos <- dados_deduplicados_problemas[, .(
  problema_individual = trimws(unlist(strsplit(as.character(get(col_problemas)), ",")))
),
by = colunas_para_agrupar_e_manter 
]

problemas_expandidos <- problemas_expandidos[!is.na(problema_individual) & problema_individual != ""]

#filtrar problemas que foram inseridos na modalidade outros

problemas_padrao <- c("Lotação",
                      "Tempo dentro do veículo",
                      "Tempo de espera na parada",
                      "Tempo de espera no terminal",
                      "Tempo de caminhada até a parada",
                      "Tempo de caminhada na integração",
                      "Previsibilidade (ônibus chega no horário previsto)",
                      "Conforto no veículo",
                      "Custo da passagem",
                      "Segurança (risco de assalto)")

problemas_custom_varios <- problemas_expandidos %>% filter(problema_individual %nin% problemas_padrao)

#determinar se o gemini é necessário e aplicar conforme pergunta anterior

if (nrow(problemas_custom_varios) > 0){

problemas_custom_varios <- problemas_custom_varios %>%
  mutate(prompt = paste("Por favor, classifique o seguinte problema reportado por um usuário de transporte público:",
                        problema_individual,      
                        "As categorias predefinidas são:
- Lotação
- Tempo dentro do veículo
- Tempo de espera na parada
- Tempo de espera no terminal
- Tempo de caminhada até a parada
- Tempo de caminhada na integração
- Previsibilidade (ônibus chega no horário previsto)
- Conforto no veículo (inclui ar-condicionado, limpeza)
- Custo da passagem
- Segurança (risco de assalto)

Se a resposta se encaixar em uma dessas categorias, retorne somente o nome da categoria.
Se a resposta não se encaixar bem mas for um problema válido de transporte público, sugira uma nova categoria concisa ou retorne 'NOVA CATEGORIA: [sugestão]'.
Se a resposta for uma brincadeira, sem sentido, ou claramente não relacionada a um problema de transporte, retorne 'DESCONSIDERAR'.
Se a resposta indicar múltiplos problemas, retorne a categoria do problema principal ou as categorias separadas por ';'."))

aplica_gemini <- function(i){
  
  j <- problemas_custom$prompt[i]
  Sys.sleep(3)
  result <- gemini(j)
  result2 <- trimws(as.character(result))
  return(result2)
}

vector_results_varios <- lapply(X = 1:length(problemas_custom_varios$prompt), aplica_gemini)

problemas_custom_varios <- problemas_custom_varios %>% mutate(problema_gemini = vector_results_varios) %>% dplyr::select(-prompt)


problemas_custom_varios_expandidos <- problemas_custom_varios %>% filter(problema_gemini != "DESCONSIDERAR")

problemas_expandidos_final <- rbind(problemas_expandidos %>%
                                    filter(problema_individual %in% problemas_padrao),
                                  problemas_custom_varios_expandidos %>%
                                    mutate(problema_individual := problema_gemini) %>%
                                    dplyr::select(names(problemas_expandidos)))
} else {
  
  problemas_expandidos_final <- problemas_expandidos %>%
    filter(problema_individual %in% problemas_padrao)
  
}
#calcular frequencias
problemas_principais <- problemas_expandidos_final[, .(freq = .N), by = problema_individual]

problemas_principais[, user_percent := freq/nrow(dados_deduplicados_problemas)]

names(problemas_principais) <- c("classe", "freq", "user_percent")

#se o gemini tiver adicionado nova categoria
problemas_principais <- problemas_principais %>% mutate(classe = sub("NOVA CATEGORIA: ", "", classe, fixed = TRUE))

problemas_principais[, classe := factor(classe, levels = problemas_principais[order(-freq), classe])]

#gráfico dos principais problemas usando ggplot2

grafico_problemas <- ggplot(problemas_principais, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 15,
            family = "encode_sans_light") +  
  labs(title = "Problemas principais", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(problemas_principais$user_percent)+0.1)) +  
  theme_minimal() +
  coord_flip() +
  tema_barras_h + 
  theme(axis.text.x = ggtext::element_markdown(size=30,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 0,
                                               vjust = 0.5))

grafico_problemas_svg <- ggplot(problemas_principais, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 5,
            family = "encode_sans_light") +  
  labs(title = "Problema principal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(problemas_principais$user_percent)+0.1)) +  
  theme_minimal() +
  coord_flip() +
  tema_barras_svg_h + 
  theme(axis.text.x = ggtext::element_markdown(size=14,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 45,
                                               vjust = 0.5))

#Salvar o gráfico

ggsave(grafico_problemas, 
       file= 'data/graficos/4-3_problemas_principais.png', 
       dpi = 300, width = 22, height = 15, units = "cm")

ggsave(grafico_problemas_svg, 
       file= 'data/graficos/4-3_problemas_principais.svg',
       width = 22, height = 15, units = "cm")




#5. Análise dos comentários e sugestões x8----------------------------------


dados_comentarios <- dados

col_id_nome <- grep("x1", names(dados_horario), value = TRUE)


dados_deduplicados_comentarios <- unique(dados, by = col_id_nome)
message(paste("Número de linhas original em 'dados':", nrow(dados)))
message(paste("Número de linhas após deduplicação por telefone ('dados_deduplicados'):", nrow(dados_deduplicados_comentarios)))


col_comentarios <- grep("x8", names(dados_deduplicados_comentarios), value = TRUE)

#sugestões padrão de ponto de partida para o gemini
sugestoes_padrao <- c("Aumentar a frota",
                      "Melhorar o ar-condicionado",
                      "Melhorar a limpeza",
                      "Diminuir o tempo de viagem",
                      "Melhorar o conforto",
                      "Diminuir o tempo de espera",
                      "Melhorar a segurança",
                      "Mudar o itinerário",
                      "Adicionar mais linhas",
                      "Treinar melhor os motoristas",
                      "Melhorar o wi-fi")

#problemas padrão de ponto de partida para o gemini
problemas_padrao_comentarios <- c("Lotação",
                                  "Tempo dentro do veículo",
                                  "Tempo de espera na parada",
                                  "Tempo de espera no terminal",
                                  "Tempo de caminhada até a parada",
                                  "Tempo de caminhada na integração",
                                  "Previsibilidade (ônibus chega no horário previsto)",
                                  "Conforto no veículo",
                                  "Custo da passagem",
                                  "Segurança (risco de assalto)",
                                  "Comportamento do motorista",
                                  "Comentário sobre outra linha")

#remover respostas vazias
sugestoes <- dados_deduplicados_comentarios[!is.na(get(col_comentarios)) & get(col_comentarios) != ""]


#determinar se o gemini é necessário e aplicar o prompt

if (nrow(sugestoes) > 0){
  
  sugestoes <- sugestoes %>%
    mutate(prompt = paste("
    Por favor, analise o seguinte comentário de um usuário de transporte público.
O comentário pode ser uma sugestão, uma crítica, um problema, ou uma combinação destes.
Sua tarefa é:
1.  Identificar os pontos individuais (problemas, sugestões, elogios) no comentário.
2.  Para cada ponto identificado, classifique-o da seguinte forma:

    a.  **PROBLEMAS/CRÍTICAS:**
        * Se corresponder a uma das 'Categorias de PROBLEMAS/CRÍTICAS PREDEFINIDAS' abaixo, retorne no formato: 'PROBLEMA: [Nome da Categoria Predefinida]'
        * Se for um problema/crítica válido não listado, retorne: 'NOVA CATEGORIA DE PROBLEMA: [Descrição concisa do novo problema]'

    b.  **SUGESTÕES:**
        * Se corresponder a uma das 'Categorias de SUGESTÕES PREDEFINIDAS' abaixo, retorne no formato: 'SUGESTÃO: [Nome da Categoria Predefinida]'
        * Se for uma sugestão válida não listada, retorne: 'NOVA CATEGORIA DE SUGESTÃO: [Descrição concisa da nova sugestão]'

    c.  **ELOGIOS:**
        * Como não há categorias predefinidas para elogios, se identificar um, retorne: 'ELOGIO: [Descrição concisa do elogio]' (Você deverá criar a descrição do elogio de forma concisa, por exemplo, 'ELOGIO: Motoristas atenciosos').

    d.  **IRRELEVANTE/OUTRA LINHA:**
        * Se o comentário for uma brincadeira, sem sentido, ou não relacionado a transporte, retorne: 'DESCONSIDERAR'
        * Se o comentário for exclusivamente sobre outra linha de transporte, retorne: 'COMENTÁRIO SOBRE OUTRA LINHA: [Resumo conciso do comentário]'

3.  Se o comentário contiver múltiplos pontos, separe cada classificação com um ponto e vírgula (';').

Segue um exemplo de como analisar um comentário complexo:
COMENTÁRIO EXEMPLO: `Minha namorada é PCD, Cadeirante mais especificamente e sempre os ônibus demoram muito para vir e acabamos não conseguindo almoçar algumas vezes por causa do atraso de alguns ônibus nos horários de almoço, fora já ter que perder algumas aulas por conta da lotação no horário de pico matinal e ter que vir mais tarde. Além de tudo isso alguns ônibus tem o elevador da porta do meio do ônibus quebrado, o que torna mais demorado ainda, nesse caso teriamos que esperar outro ônibus. Na hora da volta é outra dificuldade, dessa vez no sentido volta, Benfica-Riomar Kennedy, lotação extrema até muito tarde, como vimos em alguns dias, como exemplo um ônibus que passou no IFCE 19:30 e estava lotado, o que acaba faz com que ela pegue o ônibus de mais tarde para fugir de lotação, o que acaba sendo muito perigoso para qualquer pessoa, mais ainda para um PCD. Apesar disso comparado as outras linhas, a linha 75 cumpre um ótimo itinerário, considerando a distância do itinerário o tempo entre os ônibus não é tão longo, a maioria em que vim sempre teve ar condicionado, motoristas gentis e pacientes com a situação. Mesmo com tudo isso ainda é necessário mais frota de ônibus para diminuir a lotação.`
RESPOSTA ESPERADA PARA O EXEMPLO: `PROBLEMA: Tempo de espera na parada; PROBLEMA: Imprevisibilidade (ônibus não chega no horário previsto); PROBLEMA: Lotação; NOVA CATEGORIA DE PROBLEMA: Equipamento de acessibilidade quebrado; PROBLEMA: Segurança (risco de assalto); ELOGIO: Bom itinerário; ELOGIO: Ar condicionado funcional; ELOGIO: Motoristas gentis; SUGESTÃO: Aumentar a frota`

Categorias de PROBLEMAS/CRÍTICAS PREDEFINIDAS:
- Lotação
- Tempo dentro do veículo
- Tempo de espera na parada
- Tempo de espera no terminal
- Tempo de caminhada até a parada
- Tempo de caminhada na integração
- Imprevisibilidade (ônibus não chega no horário previsto)
- Conforto no veículo (inclui ar-condicionado, limpeza)
- Custo da passagem
- Segurança (risco de assalto)

Categorias de SUGESTÕES PREDEFINIDAS:
- Aumentar a frota
- Melhorar o ar-condicionado
- Melhorar a limpeza
- Diminuir o tempo de viagem
- Melhorar o conforto
- Diminuir o tempo de espera
- Melhorar a segurança
- Mudar o itinerário
- Adicionar mais linhas
- Treinar melhor os motoristas
- Melhorar o wi-fi

---
COMENTÁRIO PARA ANÁLISE:",!!sym(col_comentarios) ,"
---"))
  
  sugestoes <- sugestoes %>%
    mutate(prompt = trimws(gsub("\\s+", " ", gsub(" ", " ", prompt, fixed = TRUE))))
  
  #aplicar o gemini com temperature menor (menor variabilidade)
  aplica_gemini_comments <- function(i){
    
    j <- sugestoes$prompt[i]
    Sys.sleep(3)
    result <- gemini(j,
                     temperature = 0.3)
    result2 <- trimws(as.character(result))
    return(result2)
  }
  
  vector_results_varios <- lapply(X = 1:length(sugestoes$prompt), aplica_gemini_comments)
  # vector_results_varios <- lapply(X = 10:10, aplica_gemini_comments)
  
  sugestoes <- sugestoes %>% mutate(gemini = vector_results_varios) %>% dplyr::select(-prompt)
  
  sugestoes2 <- sugestoes %>% filter(gemini != "DESCONSIDERAR")
  
  
} else {
  
  sugestoes2 <- sugestoes
  
}

#Colocar cada problema em uma linha dos dados

colunas_para_agrupar_e_manter <- names(sugestoes2)[names(sugestoes2) != "gemini"]

sugestoes_expandidos <- sugestoes2[, .(
  sugestao_individual = trimws(unlist(strsplit(as.character(gemini), ";")))
),
by = colunas_para_agrupar_e_manter
]


sugestoes_expandidos <- sugestoes_expandidos[!is.na(sugestao_individual) & sugestao_individual != ""]
sugestoes_expandidos[, tipo := tstrsplit(sugestao_individual, ":", fixed = TRUE, keep = 1L)]
sugestoes_expandidos[, texto := sub("^[^:]+:\\s*", "", sugestao_individual)]
sugestoes_expandidos <- sugestoes_expandidos[sugestao_individual != "DESCONSIDERAR"]

unique(sugestoes_expandidos$tipo)

elogios <- sugestoes_expandidos[tipo == "ELOGIO"]

sugestoes <- sugestoes_expandidos[tipo == "SUGESTÃO" | tipo == "NOVA CATEGORIA DE SUGESTÃO"]

problemas_comentarios <- sugestoes_expandidos[tipo == "PROBLEMA" | tipo == "NOVA CATEGORIA DE PROBLEMA"]

outra_linha <-  sugestoes_expandidos[tipo == "COMENTÁRIO SOBRE OUTRA LINHA"]


#contabilização dos problemas


problemas_resumo <- problemas_comentarios[, .(freq = .N), by = texto]

problemas_resumo[, user_percent := freq/sum(freq)]

names(problemas_resumo) <- c("classe", "freq", "user_percent")

problemas_resumo[, classe := factor(classe, levels = problemas_resumo[order(-freq), classe])]


#contabilização dos elogios
elogios_resumo <- elogios[, .(freq = .N), by = texto]

elogios_resumo[, user_percent := freq/sum(freq)]

names(elogios_resumo) <- c("classe", "freq", "user_percent")

elogios_resumo[, classe := factor(classe, levels = elogios_resumo[order(-freq), classe])]


#contabilização das sugestões

#padronizar algumas sugestões e <br> para plotar no gráfico:

sugestoes[, texto2 := case_when(texto == "Adicionar mais linhas" ~ "Implantar mais linhas<br>para as universidades",
                               texto == "Adicionar mais linhas que percorram os trajetos que se destinam a universidades." ~ "Implantar mais linhas<br>para as universidades",
                               texto == "Aumentar a frota" ~ "Aumentar a frota",
                               texto == "Aumentar a frota e melhorar a distribuição dos ônibus" ~ "Aumentar a frota",
                               texto == "Aumentar a frota nesse horário" ~ "Aumentar a frota",
                               texto == "Aumentar a velocidade dos ônibus" ~ "Aumentar a velocidade<br>dos ônibus",
                               texto == "Diminuir o tempo de espera" ~ "Diminuir o tempo de espera",
                              texto == "Diminuir o tempo de viagem" ~ "Diminuir o tempo de viagem",
                              texto == "Distribuir melhor a frota para otimizar os horários" ~ "Aumentar a frota",
                              texto == "Expandir a linha para atender mais bairros" ~ "Mudar o itinerário<br>ou ponto de parada<br>(Campus do Pici)",
                              texto == "Implementar faixa exclusiva para ônibus." ~ "Implementar faixa exclusiva",
                              texto == "Melhorar as condições do trajeto" ~ "Mudar o itinerário<br>ou ponto de parada<br>(Campus do Pici)",
                              
                              texto == "Melhorar o ar-condicionado" ~ "Melhorar o ar-condicionado",
                              texto == "Melhorar o conforto" ~ "Melhorar o ar-condicionado",
                              texto == "Mudar o itinerário" ~ "Mudar o itinerário<br>ou ponto de parada<br>(Campus do Pici)",
                              
                              texto == "Padronizar os ônibus."  ~ "Melhorar o ar-condicionado",
                              texto == "Permitir acesso a aplicativos de banco."  ~ "Melhorar o Wi-Fi",
                              texto == "Prolongar o trajeto"   ~ "Mudar o itinerário<br>ou ponto de parada<br>(Campus do Pici)",
                              TRUE ~ "Outros")]


sugestoes_resumo <- sugestoes[, .(freq = .N), by = texto2]

sugestoes_resumo[, user_percent := freq/sum(freq)]

names(sugestoes_resumo) <- c("classe", "freq", "user_percent")

sugestoes_resumo[, classe := factor(classe, levels = sugestoes_resumo[order(-freq), classe])]

#gráfico de SUGESTÕES usando ggplot2

grafico_sugestoes <- ggplot(sugestoes_resumo, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 15,
            family = "encode_sans_light") +  
  labs(title = "Problemas principais", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(sugestoes_resumo$user_percent)+0.1)) + 
  theme_minimal() +
  coord_flip() +
  tema_barras_h + 
  theme(axis.text.x = ggtext::element_markdown(size=30,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 0,
                                               vjust = 0.5))

grafico_sugestoes_svg <- ggplot(sugestoes_resumo, aes(x = classe, y = user_percent)) +
  geom_bar(stat = "identity",
           fill = "#00a394") +
  geom_text(aes(label = scales::percent_format(scale = 100, accuracy = 0.1)(user_percent)),
            vjust = 0.5,
            hjust = -0.1,
            size = 5,
            family = "encode_sans_light") +  
  labs(title = "Problema principal", x = col_freq, y = "Porcentagem dos respondentes") +
  scale_y_continuous(labels = scales::percent_format(scale = 100),
                     limits = c(0, max(sugestoes_resumo$user_percent)+0.1)) +  
  theme_minimal() +
  coord_flip() +
  tema_barras_svg_h + 
  theme(axis.text.x = ggtext::element_markdown(size=14,
                                               family = "encode_sans_light",
                                               lineheight = 0.5,
                                               angle = 45,
                                               vjust = 0.5))

#Salvar o gráfico

ggsave(grafico_sugestoes, 
       file= 'data/graficos/5-sugestoes.png', 
       dpi = 300, width = 25, height = 15, units = "cm")

ggsave(grafico_sugestoes_svg, 
       file= 'data/graficos/5-sugestoes.svg',
       width = 25, height = 15, units = "cm")



