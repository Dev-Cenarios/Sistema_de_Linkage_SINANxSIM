############################## SISTEMA DE LINKAGE SINAN + SIM ##############################

### PACOTES

pacman::p_load(tidyverse, lubridate, readr, readxl,
               stringr, tidyr, digest, stringi, base64enc, glue, DBI, duckdb, DT)

library(shinythemes)
library(shinyjs)
library(DT)

### CONFIGURAÇÕES
# Limite de upload: 500 MB
options(shiny.maxRequestSize = 500 * 1024^2)

# Aumenta o timeout de upload para arquivos grandes (em ms)
options(shiny.http.response.size = 512 * 1024^2)

### FUNÇÕES

limpa_str <- function(x) {
  
  # Converter para UTF-8 e remover acentos
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT//IGNORE")
  # Converter para caixa alta (maiúsculas)
  x <- toupper(x)
  # Remover acentos
  x <- stringi::stri_trans_general(x, "Latin-ASCII")
  # Substituir "DESCONHECIDO" por um registro em branco
  x <- gsub("DESCONHECIDO", "", x, ignore.case = TRUE)
  x <- gsub("NA NA", "", x, ignore.case = TRUE)
  # Remover caracteres não alfanuméricos
  x <- gsub("[^[:alnum:]]", " ", x)
  # Remover preposições
  x <- gsub("\\bE\\b|\\bDO\\b|\\bDA\\b|\\bDOS\\b|\\bDAS\\b|\\bDE\\b", "", x, ignore.case = TRUE)
  # Remover espaços extras
  x <- gsub("\\s+", " ", x)
  # Remover "GUIA" seguido de informação
  x <- gsub("\\<GUIA\\> \\d+.*", "", x)
  # Remover traços no final do nome
  x <- gsub(" - $", "", x)
  # Remover espaços no início e no fim da string
  x <- trimws(x)
  
  return(x)
  
}

converter_data <- function(x, origem = c("sim", "sinan")) {
  
  origem <- match.arg(origem)
  
  # Já é Date
  if (inherits(x, "Date")) return(x)
  
  # POSIX
  if (inherits(x, c("POSIXct", "POSIXlt"))) return(as.Date(x))
  
  # ----------------------------------------------------------------
  # NUMÉRICO
  # ----------------------------------------------------------------
  if (is.numeric(x)) {
    
    x_char <- sprintf("%08.0f", x)
    
    if (origem == "sim") {
      # SIM numérico: formato ddmmyyyy (ex: 01022024)
      datas <- suppressWarnings(as.Date(x_char, format = "%d%m%Y"))
      
      idx_na <- is.na(datas)
      if (any(idx_na)) {
        datas[idx_na] <- as.Date(x[idx_na], origin = "1899-12-30")
      }
      return(datas)
    }
    
    # SINAN numérico: pode ser yyyymmdd (ex: 20210408) ou serial Excel
    # Tenta yyyymmdd primeiro
    datas <- suppressWarnings(as.Date(x_char, format = "%Y%m%d"))
    
    # Fallback: ddmmyyyy
    idx_na <- is.na(datas)
    if (any(idx_na)) {
      datas[idx_na] <- suppressWarnings(
        as.Date(x_char[idx_na], format = "%d%m%Y")
      )
    }
    
    # Último fallback: serial Excel
    idx_na <- is.na(datas)
    if (any(idx_na)) {
      datas[idx_na] <- as.Date(x[idx_na], origin = "1899-12-30")
    }
    
    return(datas)
  }
  
  # ----------------------------------------------------------------
  # CHARACTER
  # ----------------------------------------------------------------
  x <- trimws(as.character(x))
  
  # Substitui valores inválidos por NA antes de tentar parsear
  invalidos <- c("", "00000000", "0000-00-00", "00/00/0000",
                 "NA", "N/A", "NULL", "IGNORADO", "NAO INFORMADO")
  x[x %in% invalidos] <- NA_character_
  
  if (origem == "sim") {
    # SIM character: espera ddmmyyyy (ex: "01022024")
    datas <- suppressWarnings(as.Date(x, format = "%d%m%Y"))
    
    idx_na <- is.na(datas)
    if (any(idx_na)) {
      datas[idx_na] <- suppressWarnings(
        lubridate::parse_date_time(
          x[idx_na],
          orders = c("dmy", "ymd", "d/m/Y", "Y-m-d", "Ymd")
        ) |> as.Date()
      )
    }
    return(datas)
  }
  
  # SINAN character: espera dd/mm/yyyy (ex: "08/04/2021")
  # mas Manaus pode vir como "yyyymmdd" string também
  datas <- suppressWarnings(as.Date(x, format = "%d/%m/%Y"))
  
  idx_na <- is.na(datas)
  if (any(idx_na)) {
    datas[idx_na] <- suppressWarnings(
      as.Date(x[idx_na], format = "%Y%m%d")
    )
  }
  
  idx_na <- is.na(datas)
  if (any(idx_na)) {
    datas[idx_na] <- suppressWarnings(
      lubridate::parse_date_time(
        x[idx_na],
        orders = c("dmy", "ymd", "Y-m-d", "Ymd")
      ) |> as.Date()
    )
  }
  
  return(datas)
}

# Função auxiliar para diagnóstico de formato de data
detectar_formato_data <- function(x, campo, base) {
  tipo <- class(x)[1]
  n_na <- sum(is.na(x))
  amostra <- head(x[!is.na(x)], 3)
  message(sprintf("[%s] %s | tipo: %s | NAs: %d | amostra: %s",
                  base, campo, tipo, n_na, paste(amostra, collapse = ", ")))
}

preprocessar_dados <- function(sim.orig, sinan.orig) {
  
  ################################
  #### LIMPEZA SIM
  ################################
  
  detectar_formato_data(sim.orig$DTOBITO, "DTOBITO", "SIM")
  detectar_formato_data(sim.orig$DTNASC,  "DTNASC",  "SIM")
  
  sim.orig$DTOBITO <- converter_data(sim.orig$DTOBITO, "sim")
  sim.orig$DTNASC  <- converter_data(sim.orig$DTNASC,  "sim")
  
  # Valida resultado — alerta se muitos NAs após conversão
  pct_na_dtnasc_sim <- mean(is.na(sim.orig$DTNASC))
  if (pct_na_dtnasc_sim > 0.05)
    warning(sprintf("SIM: %.1f%% de DTNASC virou NA após converter_data — verifique o formato de entrada.", 
                    pct_na_dtnasc_sim * 100))
  
  sim.orig$DIA     <- lubridate::day(sim.orig$DTNASC)
  sim.orig$MES     <- lubridate::month(sim.orig$DTNASC)
  sim.orig$ANO     <- lubridate::year(sim.orig$DTNASC)
  sim.orig$anoobito <- lubridate::year(sim.orig$DTOBITO)
  
  sim.orig <- sim.orig |>
    mutate(
      idade2 = anoobito - ANO,
      idade_SIM = ifelse(
        is.na(idade2), 0,
        ifelse(idade2 < 1,   300 + round(idade2 * 12),
               ifelse(idade2 < 100, 400 + idade2, 499))
      )
    )
  
  sim <- sim.orig |>
    mutate(idade2 = as.numeric(idade2)) |>
    select(
      NUMERODO, NUMERODV, NUMERODN, DTOBITO, anoobito, SEXO, NOME,
      DTNASC, DIA, MES, ANO, idade2, NOMEMAE, NOMEPAI, BAIRES,
      ENDRES, NUMRES, COMPLRES, CAUSABAS, CODMUNOCOR, CODMUNRES
    ) |>
    rename(idade = idade2) |>
    rename_with(tolower) |>
    mutate(endereco = paste(endres, numres)) |>
    select(-endres, -numres) |>
    select(
      numerodo, numerodn, sexo, nome, dtnasc, dia, mes, ano, idade,
      nomemae, baires, endereco, codmunocor, codmunres,
      dtobito, anoobito, causabas
    ) |>
    distinct(numerodo, .keep_all = TRUE) |>
    rename(bairro = baires)
  
  sim$nome     <- limpa_str(sim$nome)
  sim$nomemae  <- limpa_str(sim$nomemae)
  sim$bairro   <- limpa_str(sim$bairro)
  sim$endereco <- limpa_str(sim$endereco)
  
  # Padroniza sexo para "M"/"F" (SINAN usa M/F, garante consistência)
  sim$sexo <- dplyr::case_when(
    sim$sexo %in% c("1", "M", "m") ~ "M",
    sim$sexo %in% c("2", "F", "f") ~ "F",
    TRUE ~ NA_character_
  )
  
  ################################
  #### LIMPEZA SINAN
  ################################
  
  detectar_formato_data(sinan.orig$DT_NOTIFIC, "DT_NOTIFIC", "SINAN")
  detectar_formato_data(sinan.orig$DT_NASC,    "DT_NASC",    "SINAN")
  
  sinan.orig$DT_NOTIFIC <- converter_data(sinan.orig$DT_NOTIFIC, "sinan")
  sinan.orig$DT_NASC    <- converter_data(sinan.orig$DT_NASC,    "sinan")
  
  pct_na_dtnasc_sinan <- mean(is.na(sinan.orig$DT_NASC))
  if (pct_na_dtnasc_sinan > 0.05)
    warning(sprintf("SINAN: %.1f%% de DT_NASC virou NA após converter_data — verifique o formato de entrada.",
                    pct_na_dtnasc_sinan * 100))
  
  sinan.orig$DIA <- lubridate::day(sinan.orig$DT_NASC)
  sinan.orig$MES <- lubridate::month(sinan.orig$DT_NASC)
  sinan.orig$ANO <- lubridate::year(sinan.orig$DT_NASC)
  
  sinan <- sinan.orig |>
    select(
      NU_NOTIFIC, DT_NOTIFIC, NU_ANO, SG_UF_NOT, ID_MUNICIP,
      NM_PACIENT, DT_NASC, NU_IDADE_N, CS_SEXO, NM_MAE_PAC,
      SG_UF, ID_MN_RESI, NM_BAIRRO, NM_LOGRADO, NU_NUMERO,
      NM_COMPLEM, DIA, MES, ANO
    ) |>
    rename_with(tolower) |>
    mutate(
      nu_idade_n    = as.character(nu_idade_n),
      unidade_idade = substr(nu_idade_n, 1, 1),
      valor_idade   = as.numeric(substr(nu_idade_n, 2, nchar(nu_idade_n))),
      idade = floor(case_when(
        unidade_idade == "4" ~ valor_idade,
        unidade_idade == "3" ~ valor_idade / 12,
        unidade_idade == "2" ~ valor_idade / 365,
        unidade_idade == "1" ~ valor_idade / (365 * 24),
        TRUE ~ NA_real_
      ))
    ) |>
    distinct(nu_notific, .keep_all = TRUE) |>
    rename(
      nunotific  = nu_notific,
      dtnotific  = dt_notific,
      nome       = nm_pacient,
      nomemae    = nm_mae_pac,
      sexo       = cs_sexo,
      dtnasc     = dt_nasc,
      bairro     = nm_bairro,
      codmunocor = id_municip,
      codmunres  = id_mn_resi
    ) |>
    select(-valor_idade, -unidade_idade) |>
    mutate(endereco = paste(nm_logrado, nu_numero)) |>
    select(-nm_logrado, -nu_numero) |>
    select(
      nunotific, dtnotific, sexo, nome, dtnasc,
      dia, mes, ano, idade, nomemae,
      bairro, endereco, codmunocor, codmunres
    ) |>
    distinct(nunotific, .keep_all = TRUE)
  
  sinan$nome     <- limpa_str(sinan$nome)
  sinan$nomemae  <- limpa_str(sinan$nomemae)
  sinan$bairro   <- limpa_str(sinan$bairro)
  sinan$endereco <- limpa_str(sinan$endereco)
  
  # Padroniza sexo para "M"/"F" (alinha com SIM)
  sinan$sexo <- dplyr::case_when(
    sinan$sexo %in% c("1", "M", "m") ~ "M",
    sinan$sexo %in% c("2", "F", "f") ~ "F",
    TRUE ~ NA_character_
  )
  
  ################################
  
  return(list(
    sim    = sim,
    sinan  = sinan,
    n_sim  = nrow(sim),
    n_sinan = nrow(sinan)
  ))
}

ler_arquivo_grande_shiny <- function(caminho, origem = c("sim", "sinan")) {
  origem <- match.arg(origem)
  ext <- tolower(tools::file_ext(caminho))

  # Lê todas as colunas do layout DATASUS (não só o subconjunto usado em
  # preprocessar_dados()): a etapa de consolidação SINAN+SIM precisa
  # recompor as colunas originais do SINAN e as colunas originais do SIM
  # não usadas no linkage (ex: CAUSABAS para o filtro de óbitos por TB).
  if (ext %in% c("xls", "xlsx")) {
    df <- readxl::read_excel(caminho)
  } else if (ext == "csv") {
    # data.table::fread é mais robusto para uploads e encoding
    df <- data.table::fread(caminho, encoding = "unknown", showProgress = FALSE)
  } else {
    stop("Formato de arquivo não suportado")
  }

  return(as.data.frame(df))
}

# Consolida o SINAN deduplicado com os óbitos do SIM (equivalente à etapa
# 03_link_sinan_sim.R da DAG). Extraída em função porque precisa rodar duas
# vezes: uma vez logo após o linkage, e de novo ao final da revisão manual
# (aba "Revisão manual"), já que remover pares da fase 15 muda tb_linkage e
# tb_sinan_dedup e por tanto deveria mudar o resultado desta consolidação.
montar_sinan_sim_linkado <- function(tb_linkage, tb_sinan_dedup, sinan_dup_raw, sim_raw) {

  # NU_NOTIFIC pode ser lido como integer (fread/read_excel detectam o tipo
  # pelo conteúdo); força character aqui para casar com a chave derivada de
  # tb_sinan_dedup abaixo — join de tipos diferentes falha no dplyr
  sinan_dup_raw <- sinan_dup_raw |>
    mutate(NU_NOTIFIC = as.character(NU_NOTIFIC))

  sinan_dedup_upper <- tb_sinan_dedup |>
    rename_with(toupper) |>
    mutate(NU_NOTIFIC = as.character(NUNOTIFIC))

  tb_linkage_pares <- tb_linkage |>
    mutate(
      NU_NOTIFIC = as.character(nunotific),
      NUMERODO   = as.character(numerodo)
    ) |>
    select(NU_NOTIFIC, NUMERODO) |>
    distinct(NU_NOTIFIC, .keep_all = TRUE)

  # Recupera as colunas originais do SINAN (as que não sobreviveram à
  # seleção/limpeza feita para o linkage), casando pela notificação
  chave <- "NU_NOTIFIC"
  colunas_faltantes <- setdiff(names(sinan_dup_raw), names(sinan_dedup_upper))

  sinan_sim_linkado <- sinan_dedup_upper |>
    left_join(
      sinan_dup_raw |>
        select(all_of(c(chave, colunas_faltantes))) |>
        distinct(across(all_of(chave)), .keep_all = TRUE),
      by = chave
    )

  # Descarta as colunas de trabalho (limpas/derivadas para o linkage) — o
  # que resta são as colunas originais do SINAN, recompostas acima
  colunas_trabalho <- c(
    "DTNOTIFIC", "SEXO", "NOME", "DTNASC", "DIA", "MES", "ANO", "IDADE",
    "NOMEMAE", "BAIRRO", "ENDERECO", "CODMUNOCOR", "CODMUNRES",
    "PRIMEIRONOME", "SEGUNDONOME", "TERCEIRONOME", "ULTIMONOME", "SEMULTIMONOME",
    "PRIMEIRONOMEMAE", "SEGUNDONOMEMAE", "TERCEIRONOMEMAE",
    "ULTIMONOMEMAE", "SEMULTIMONOMEMAE"
  )
  sinan_sim_linkado <- sinan_sim_linkado |>
    select(-any_of(colunas_trabalho))

  # O SIM entra em dois grupos, tratados de forma diferente:
  #  - PAREADO (tem NUMERODO em tb_linkage): entra com QUALQUER causa de
  #    óbito, não só TB — é o que permite detectar quando o SINAN registrou
  #    um desfecho e o SIM mostra outro (inclusive óbito por causa diferente
  #    de TB).
  #  - SEM PAR (não aparece em tb_linkage): só entra se a causa for TB (CID
  #    A15-A19). Óbito de outra causa sem notificação de TB associada não é
  #    subnotificação — não deve inflar a base com mortes sem relação com o
  #    programa.
  numerodos_pareados <- tb_linkage_pares$NUMERODO

  sim_pareado <- sim_raw |>
    filter(NUMERODO %in% numerodos_pareados)

  sim_sem_par_tb <- sim_raw |>
    filter(!NUMERODO %in% numerodos_pareados) |>
    mutate(cid_3 = substr(as.character(CAUSABAS), 1, 3)) |>
    filter(cid_3 %in% c("A15", "A16", "A17", "A18", "A19")) |>
    select(-cid_3)

  sim_para_join <- bind_rows(sim_pareado, sim_sem_par_tb)

  sinan_sim_linkado <- sinan_sim_linkado |>
    mutate(NU_NOTIFIC = as.character(NU_NOTIFIC)) |>
    left_join(
      tb_linkage_pares |>
        mutate(
          NU_NOTIFIC = as.character(NU_NOTIFIC),
          NUMERODO   = as.character(NUMERODO)
        ),
      by = "NU_NOTIFIC"
    ) |>
    full_join(
      sim_para_join |> mutate(NUMERODO = as.character(NUMERODO)),
      by = "NUMERODO"
    )

  # Óbitos por TB que ficaram "só no SIM" (sem par no SINAN) — continua
  # só-TB por construção, já que sim_sem_par_tb filtrou antes do join
  apenas_sim <- sinan_sim_linkado |>
    filter(is.na(NU_NOTIFIC))

  # Registros vindos só do SIM não têm NU_NOTIFIC; cria identificador
  # sintético ("SIM_" + NUMERODO) porque chave nula quebraria filtros e
  # comparações posteriores (NULL nunca casa em IN/JOIN)
  sinan_sim_linkado <- sinan_sim_linkado |>
    mutate(
      NU_NOTIFIC = ifelse(is.na(NU_NOTIFIC), paste0("SIM_", NUMERODO), NU_NOTIFIC)
    ) |>
    select(-ends_with(".y")) |>
    rename_with(~ gsub("\\.x$", "", .x)) |>
    rename(NUMERODO_SIM = NUMERODO)

  list(sinan_sim_linkado = sinan_sim_linkado, apenas_sim = apenas_sim)
}

log_msg <- function(msg){
  
  linha <- paste0(
    "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
    msg
  )
  
  log_execucao(
    paste(log_execucao(), linha, sep = "\n")
  )
  
}

### UI

ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  useShinyjs(),
  
  div(
    class = "header-container",
    style = "display:flex; align-items:center; justify-content:space-between;",
    
    div(
      class = "title-container",
      h2("Sistema de Linkage SINAN + SIM")
    ),
    
    div(
      class = "logo-container",
      tags$img(src = "cenarios_logo_full.jpeg", 
               style = "height:100px; width:auto;",
               onerror = "this.onerror=null; this.src=''; this.alt='Logo não carregado';")
    )
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      
      div(class = "method-title", "Linkage SINAN-SIM"),
      
      fileInput(
        "arquivo_sinan",
        "Carregar arquivo SINAN",
        accept = c(".csv",".txt",".xlsx")
      ),
      
      fileInput(
        "arquivo_sim",
        "Carregar arquivo SIM",
        accept = c(".csv",".txt",".xlsx")
      ),
      
      br(),
      
      actionButton(
        "preparar",
        "Preparar Dados",
        icon = icon("play"),
        class = "btn-primary"
      )
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Resumo",
          
          verbatimTextOutput("resumo"),
          
          br(),
          
          hr(),
          
          h4("Log de execução"),
          
          div(
            style = "
              background-color:#f8f9fa;
              border:1px solid #dee2e6;
              border-radius:6px;
              padding:15px;
              max-height:250px;
              overflow-y:auto;
            ",
            verbatimTextOutput("log_execucao")
          ),
          
          br(),
          hr(),
          
          h4("Download das bases processadas"),
          
          downloadButton("download_all", "Baixar todos os arquivos (ZIP)")
          
        ),
        
        tabPanel(
          "Revisão manual",
          
          br(),
          
          h4("Casos da fase cinza (fase = 15)"),
          
          p("Indique se cada par é MATCH ou NÃO MATCH."),
          
          DT::dataTableOutput("tabela_revisao"),
          
          br(),
          
          fluidRow(
            column(4, actionButton("revisao_anterior", "← Anterior", width = "100%")),
            column(4, align = "center", textOutput("pagina_info")),
            column(4, actionButton("revisao_proxima", "Próxima →", width = "100%"))
          ),
          
          br(),
          
          actionButton(
            "finalizar_revisao",
            "Finalizar revisão",
            class = "btn-success"
          )
        )
        
      )
      
    )
    
  )
)

### SERVER

server <- function(input, output, session){
  
  output$logo_image_base64 <- renderUI({
    
    # Certifique-se de que o arquivo existe no caminho especificado
    caminho_img <- "www/cenarios_logo_full.jpeg"
    
    if (!file.exists(caminho_img)) {
      caminho_img <- "cenarios_logo_full.jpeg"
    }
    
    if (file.exists(caminho_img)) {
      # Codifica em Base64
      img_base64 <- base64enc::dataURI(file = caminho_img, mime = "image/jpeg")
      
      # Renderiza no UI
      tags$img(src = img_base64, style = "height:60px; width:auto;")
    } else {
      # Mensagem de erro se a imagem não for encontrada
      div("Imagem não encontrada", style = "color: red;")
    }
  })
  
  ################################
  # LOG REATIVO
  ################################
  
  log_execucao <- reactiveVal("")
  
  log_msg <- function(msg){
    
    linha <- paste0(
      "[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ",
      msg
    )
    
    log_execucao(
      paste(log_execucao(), linha, sep = "\n")
    )
    
  }
  
  output$log_execucao <- renderText({
    log_execucao()
  })
  
  ################################
  # LOGO
  ################################
  
  output$logo_image_base64 <- renderUI({
    
    caminho_img <- "www/cenarios_logo_full.jpeg"
    
    if (!file.exists(caminho_img)) {
      caminho_img <- "cenarios_logo_full.jpeg"
    }
    
    if (file.exists(caminho_img)) {
      
      img_base64 <- base64enc::dataURI(file = caminho_img, mime = "image/jpeg")
      
      tags$img(src = img_base64, style = "height:60px; width:auto;")
      
    } else {
      
      div("Imagem não encontrada", style = "color: red;")
      
    }
  })

  ################################
  # BOTÃO PRINCIPAL
  ################################
  
  observeEvent(input$preparar, {

    req(input$arquivo_sim)
    req(input$arquivo_sinan)

    log_execucao("")

    log_msg("🚀 Iniciando processamento")

    tempo_total_inicio <- Sys.time()

    con <- NULL

    resultado_processamento <- tryCatch({

      withProgress(message = "Processando linkage...", value = 0, {

        ################################
        # ETAPA 1 — LEITURA
        ################################

        log_msg("📥 Lendo arquivos enviados pelo usuário")

        incProgress(0.1, detail = "Lendo arquivos...")

        sim_orig   <- ler_arquivo_grande_shiny(input$arquivo_sim$datapath, "sim")
        sinan_orig <- ler_arquivo_grande_shiny(input$arquivo_sinan$datapath, "sinan")

        log_msg("✅ Arquivos carregados")

        ################################
        # ETAPA 2 — PREPROCESSAMENTO
        ################################

        log_msg("🧹 Pré-processando dados")

        tempo_inicio <- Sys.time()

        dados <- preprocessar_dados(sim_orig, sinan_orig)

        sim_df   <- dados$sim
        sinan_df <- dados$sinan

        tempo_fim <- Sys.time()

        log_msg(paste0(
          "⏱️ Tempo preprocessamento: ",
          round(difftime(tempo_fim, tempo_inicio, units="secs"),2),
          " segundos"
        ))

        incProgress(0.2, detail = "Pré-processamento concluído")

        ################################
        # ETAPA 3 — DUCKDB
        ################################

        log_msg("🦆 Conectando ao DuckDB")

        tempo_inicio <- Sys.time()

        con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

        incProgress(0.1, detail = "Criando tabelas")

        DBI::dbWriteTable(con, "tb_sim", sim_df, overwrite = TRUE)
        DBI::dbWriteTable(con, "tb_sinan", sinan_df, overwrite = TRUE)

        log_msg("📦 Tabelas carregadas no DuckDB")

        ################################
        # ETAPA 4 — SQL
        ################################

        log_msg("⚙️ Executando script SQL de linkage")

        incProgress(0.4, detail = "Executando SQL...")

        sql_script <- paste(
          readLines("script_linkage_duckdb.txt", encoding = "UTF-8"),
          collapse = "\n"
        )

        # Executa um statement por vez: mandar o script inteiro (centenas de
        # statements) numa única chamada de dbExecute() pode travar/derrubar
        # o processo R nesta versão do driver duckdb, em vez de dar um erro
        # capturável. Statements separados por ";" no fim da linha (com ou
        # sem comentário "--" depois do ";").
        statements <- strsplit(sql_script, ";[ \t]*(--[^\n]*)?\n")[[1]]
        statements <- trimws(statements)

        # Descarta blocos que, sem os comentários, não têm SQL de verdade
        # (ex: um bloco "/* ... */" de cabeçalho que sobrou isolado entre
        # dois statements reais depois do split)
        so_sql <- gsub("(?s)/\\*.*?\\*/", "", statements, perl = TRUE)
        so_sql <- gsub("--[^\n]*", "", so_sql)
        statements <- statements[nchar(trimws(so_sql)) > 0]

        for (stmt in statements) {
          DBI::dbExecute(con, stmt)
        }

        tempo_fim <- Sys.time()

        log_msg("✅ Script SQL executado")

        log_msg(paste0(
          "⏱️ Tempo SQL: ",
          round(difftime(tempo_fim, tempo_inicio, units="secs"),2),
          " segundos"
        ))

        ################################
        # ETAPA 5 — LEITURA RESULTADOS
        ################################

        log_msg("📊 Lendo tabelas resultantes")

        incProgress(0.1, detail = "Carregando resultados...")

        tb_sinan_id <- DBI::dbReadTable(con, "tb_sinan")
        tb_sinan_dedup <- DBI::dbReadTable(con, "tb_sinan_dedup")
        tb_sinan_duplicados <- DBI::dbReadTable(con, "tb_sinan_duplicados")
        tb_sinan_revisao_manual <- DBI::dbReadTable(con, "tb_linkage_dedup")
        tb_linkage   <- DBI::dbReadTable(con, "tb_linkage_final")

        log_msg("📌 Total de pares linkage:")
        log_msg(paste0("   ", nrow(tb_linkage)))

        ################################
        # ETAPA 6 — CONSOLIDAÇÃO SINAN + SIM
        ################################

        log_msg("🔗 Consolidando SINAN deduplicado com os óbitos do SIM")

        incProgress(0.05, detail = "Consolidando SINAN + SIM...")

        # Bases originais (todas as colunas, como enviadas pelo usuário) —
        # sim_orig/sinan_orig não são alteradas por preprocessar_dados()
        # (que opera sobre cópias locais), então continuam intactas aqui.
        # Ficam guardadas no resultado para permitir recalcular esta
        # consolidação após a revisão manual (aba "Revisão manual").
        sinan_dup_raw <- sinan_orig |> rename_with(toupper)
        sim_raw       <- sim_orig   |> rename_with(toupper)

        consolidado <- montar_sinan_sim_linkado(tb_linkage, tb_sinan_dedup, sinan_dup_raw, sim_raw)
        sinan_sim_linkado <- consolidado$sinan_sim_linkado
        apenas_sim <- consolidado$apenas_sim

        log_msg(paste0("💀 Óbitos por TB apenas no SIM (sem par no SINAN): ", nrow(apenas_sim)))
        log_msg(paste0("✅ sinan_sim_linkado: ", nrow(sinan_sim_linkado), " linhas"))

        ################################
        # FINALIZAÇÃO
        ################################

        incProgress(0.1, detail = "Finalizando...")

        tempo_total_fim <- Sys.time()

        log_msg("--------------------------------------------")
        log_msg("🎯 Processamento finalizado")

        log_msg(paste0(
          "⏳ Tempo total: ",
          round(difftime(tempo_total_fim, tempo_total_inicio, units="secs"),2),
          " segundos"
        ))

        list(
          sim = sim_df,
          sinan = sinan_df,
          tb_sinan_id = tb_sinan_id,
          tb_sinan_dedup = tb_sinan_dedup,
          tb_sinan_duplicados = tb_sinan_duplicados,
          tb_sinan_revisao_manual = tb_sinan_revisao_manual,
          tb_linkage = tb_linkage,
          sinan_sim_linkado = sinan_sim_linkado,
          apenas_sim = apenas_sim,
          sinan_dup_raw = sinan_dup_raw,
          sim_raw = sim_raw,
          n_sim = nrow(sim_df),
          n_sinan = nrow(sinan_df)
        )

      })

    }, error = function(e) {

      log_msg(paste0("❌ Erro no processamento: ", conditionMessage(e)))

      showNotification(
        paste("Erro ao processar os dados:", conditionMessage(e)),
        type = "error",
        duration = NULL
      )

      NULL

    }, finally = {

      if (!is.null(con)) {
        tryCatch(DBI::dbDisconnect(con, shutdown = TRUE), error = function(e) NULL)
      }

    })

    req(resultado_processamento)

    ################################
    # RESULTADO
    ################################

    resultado(resultado_processamento)

  })
  
  #############################################################
  # ABA DE REVISÃO MANUAL DO LINKAGE (CORRIGIDA - id_sinan)
  #############################################################
  
  resultado <- reactiveVal(NULL)
  revisao_manual <- reactiveVal(NULL)
  
  # Seleções de match do usuário (agora usando id_sinan)
  selecoes_revisao <- reactiveVal(data.frame(
    id_sinan = integer(),
    stringsAsFactors = FALSE
  ))
  
  # Controle de paginação
  idx_revisao_atual <- reactiveVal(1)
  linhas_por_pagina <- 10
  
  # Carrega os dados da fase 15 quando resultado() é atualizado
  observeEvent(resultado(), {
    req(resultado())
    
    dados_rev <- resultado()$tb_sinan_revisao_manual
    dados_rev <- dados_rev[dados_rev$fase == 15, ]
    
    revisao_manual(dados_rev)
    
    # Print para depuração (opcional)
    #print(head(dados_rev))
    
    # Inicializa seleções com nenhuma linha selecionada
    selecoes_revisao(data.frame(id_sinan = integer()))
    
    # Página inicial
    idx_revisao_atual(1)
  })
  
  # Navegação: próxima página
  observeEvent(input$revisao_proxima, {
    req(revisao_manual())
    total_paginas <- ceiling(nrow(revisao_manual()) / linhas_por_pagina)
    if (idx_revisao_atual() < total_paginas) {
      idx_revisao_atual(idx_revisao_atual() + 1)
    }
  })
  
  # Navegação: página anterior
  observeEvent(input$revisao_anterior, {
    if (idx_revisao_atual() > 1) {
      idx_revisao_atual(idx_revisao_atual() - 1)
    }
  })
  
  # Renderização da tabela com checkboxes
  output$tabela_revisao <- renderDT({
    req(revisao_manual())
    
    dados <- revisao_manual()
    
    # Verifica se a coluna 'id_sinan' existe nos dados
    if (!"id_sinan" %in% names(dados)) {
      showNotification(
        "Erro: a coluna 'id_sinan' não foi encontrada nos dados de revisão.",
        type = "error",
        duration = NULL
      )
      return(NULL)
    }
    
    # Paginação manual com tratamento para zero linhas
    if (nrow(dados) == 0) {
      dados_exibir <- dados[0, , drop = FALSE]
    } else {
      idx <- idx_revisao_atual()
      start <- (idx - 1) * linhas_por_pagina + 1
      end <- min(idx * linhas_por_pagina, nrow(dados))
      dados_exibir <- dados[start:end, , drop = FALSE]
    }
    
    # Adiciona coluna de checkboxes (HTML) usando id_sinan
    dados_exibir$match <- sapply(dados_exibir$id_sinan, function(id) {
      checked <- if (id %in% selecoes_revisao()$id_sinan) "checked" else ""
      sprintf('<input type="checkbox" class="match_box" data-id="%s" %s/>', id, checked)
    })
    
    # Seleciona colunas para exibir (usando os nomes reais fornecidos)
    colunas_exibir <- intersect(
      c("nome_tb1", "nome_tb2", "nomemae_tb1", "nomemae_tb2",
        "dtnasc_tb1", "dtnasc_tb2", "match"),
      names(dados_exibir)
    )
    
    DT::datatable(
      dados_exibir[, colunas_exibir, drop = FALSE],
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      options = list(
        pageLength = linhas_por_pagina,
        ordering = FALSE,
        dom = 't' # desativa paginação do DT (usamos a nossa)
      ),
      callback = JS("
      table.on('change', '.match_box', function(){
        var id = $(this).data('id');
        var checked = $(this).prop('checked');
        Shiny.setInputValue('match_click', {id: id, value: checked, nonce: Math.random()}, {priority: 'event'});
      });
    ")
    )
  })
  
  # Atualiza seleções quando um checkbox é marcado/desmarcado
  observeEvent(input$match_click, {
    req(input$match_click$id)
    id <- input$match_click$id
    
    selecoes <- selecoes_revisao()
    
    if (input$match_click$value) {
      # Adiciona
      selecoes <- unique(rbind(selecoes, data.frame(id_sinan = id, stringsAsFactors = FALSE)))
    } else {
      # Remove
      selecoes <- selecoes[selecoes$id_sinan != id, , drop = FALSE]
    }
    
    selecoes_revisao(selecoes)
  })
  
  # Finaliza a revisão: remove registros não selecionados
  observeEvent(input$finalizar_revisao, {
    req(resultado())
    req(revisao_manual())
    
    dados_match <- selecoes_revisao()$id_sinan
    
    novo_resultado <- resultado()
    
    # IDs da fase 15 (usando id_sinan)
    ids_fase15 <- resultado()$tb_sinan_revisao_manual$id_sinan[
      resultado()$tb_sinan_revisao_manual$fase == 15
    ]
    
    # IDs a remover: fase 15 não marcados
    ids_remover <- setdiff(ids_fase15, dados_match)
    
    # Filtra todas as tabelas relevantes (assumindo que usam id_sinan)
    if (nrow(novo_resultado$tb_linkage) > 0 && "id_sinan" %in% names(novo_resultado$tb_linkage)) {
      novo_resultado$tb_linkage <- novo_resultado$tb_linkage[
        !(novo_resultado$tb_linkage$id_sinan %in% ids_remover), 
      ]
    }
    
    if (nrow(novo_resultado$tb_sinan_dedup) > 0 && "id_sinan" %in% names(novo_resultado$tb_sinan_dedup)) {
      novo_resultado$tb_sinan_dedup <- novo_resultado$tb_sinan_dedup[
        !(novo_resultado$tb_sinan_dedup$id_sinan %in% ids_remover), 
      ]
    }
    
    if (nrow(novo_resultado$tb_sinan_duplicados) > 0 && "id_sinan" %in% names(novo_resultado$tb_sinan_duplicados)) {
      novo_resultado$tb_sinan_duplicados <- novo_resultado$tb_sinan_duplicados[
        !(novo_resultado$tb_sinan_duplicados$id_sinan %in% ids_remover), 
      ]
    }
    
    # Também filtra a própria tabela de revisão manual
    if (nrow(novo_resultado$tb_sinan_revisao_manual) > 0 && "id_sinan" %in% names(novo_resultado$tb_sinan_revisao_manual)) {
      novo_resultado$tb_sinan_revisao_manual <- novo_resultado$tb_sinan_revisao_manual[
        !(novo_resultado$tb_sinan_revisao_manual$id_sinan %in% ids_remover),
      ]
    }

    # Recalcula a consolidação SINAN+SIM com o tb_linkage/tb_sinan_dedup já
    # filtrados pela revisão manual, para o ZIP não sair com sinan_sim_linkado
    # desatualizado em relação aos pares que o usuário rejeitou
    if (!is.null(novo_resultado$sinan_dup_raw) && !is.null(novo_resultado$sim_raw)) {
      consolidado <- montar_sinan_sim_linkado(
        novo_resultado$tb_linkage,
        novo_resultado$tb_sinan_dedup,
        novo_resultado$sinan_dup_raw,
        novo_resultado$sim_raw
      )
      novo_resultado$sinan_sim_linkado <- consolidado$sinan_sim_linkado
      novo_resultado$apenas_sim <- consolidado$apenas_sim
    }

    resultado(novo_resultado)
    
    showNotification("Revisão manual finalizada", type = "message")
  })
  
  #############################################################
  # FIM DA ABA DE REVISÃO MANUAL DO LINKAGE
  #############################################################
  
  ################################
  #### RESUMO
  ################################
  output$resumo <- renderPrint({
    req(resultado())
    
    cat("Preprocessamento concluído\n\n")
    cat("Registros SIM:", resultado()$n_sim, "\n")
    cat("Registros SINAN:", resultado()$n_sinan, "\n\n")
    
    cat("Tabelas resultantes:\n")
    cat("tb_sinan_id:", nrow(resultado()$tb_sinan_id), "linhas\n")
    cat("tb_sinan_dedup:", nrow(resultado()$tb_sinan_dedup), "linhas\n")
    cat("tb_sinan_duplicados:", nrow(resultado()$tb_sinan_duplicados), "linhas\n")
    cat("tb_sinan_revisao_manual:", nrow(resultado()$tb_sinan_revisao_manual), "linhas\n")
    cat("tb_linkage:", nrow(resultado()$tb_linkage), "linhas\n")
    cat("sinan_sim_linkado:", nrow(resultado()$sinan_sim_linkado), "linhas\n")
    cat("apenas_sim:", nrow(resultado()$apenas_sim), "linhas\n")
  })
  
  ################################
  #### DOWNLOAD ZIP COM TODAS AS TABELAS
  ################################
  output$download_all <- downloadHandler(
    filename = function() {
      paste0("linkage_resultados_", Sys.Date(), ".zip")
    },
    
    content = function(file) {
      # Criar arquivos CSV temporários
      tmp <- tempdir()
      
      arquivos <- list(
        sim = resultado()$sim,
        sinan = resultado()$sinan,
        tb_sinan_id = resultado()$tb_sinan_id,
        tb_sinan_dedup = resultado()$tb_sinan_dedup,
        tb_sinan_duplicados = resultado()$tb_sinan_duplicados,
        tb_sinan_revisao_manual = resultado()$tb_sinan_revisao_manual,
        tb_linkage = resultado()$tb_linkage,
        sinan_sim_linkado = resultado()$sinan_sim_linkado,
        apenas_sim = resultado()$apenas_sim
      )
      
      paths <- sapply(names(arquivos), function(nm) {
        path <- file.path(tmp, paste0(nm, ".csv"))
        readr::write_csv(arquivos[[nm]], path)
        path
      })
      
      # Criar ZIP com todos os CSVs
      zip::zipr(zipfile = file, files = paths, recurse = FALSE)
    }
  )
  
}

shinyApp(ui, server)