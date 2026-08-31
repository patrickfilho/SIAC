# ============================================================
# Extração BR-DWGD e ANA  -  Taquari / RS
# Script independente. Não junta com CHIRPS/NASA POWER ainda.
# Grava em <SIAC>/diario/ no schema padrão: data | prec
# ============================================================
#
# Correções incorporadas:
#   - brclimr: a função é fetch_data(), não zonal_data()
#   - hydrobr: não está no CRAN; a função é inventory()
#   - CRS: inv vem em EPSG:4326 e centro em EPSG:4674 -> haversine
#   - inventário do hydrobr não traz período de operação:
#     é preciso baixar vários postos e medir a cobertura
#   - série da ANA precisa de grade completa de dias para que
#     os buracos apareçam como NA
#
# Instale uma vez, depois comente estas linhas:
# install.packages(c("brclimr", "remotes", "sf", "dplyr", "lubridate", "geobr"))
# remotes::install_github("hydroversebr/hydrobr")
# ============================================================

library(sf)
library(dplyr)
library(lubridate)

INICIO   <- as.Date("1981-01-01")
FIM      <- as.Date("2025-12-31")
COD_IBGE <- 4321303          # Taquari/RS  (Taquara é 4321204, não confundir)

DESTINO <- "C:/Users/Patrick Filho/OneDrive/Área de Trabalho/LabMA/SIAC"

dir.create(file.path(DESTINO, "diario"), showWarnings = FALSE, recursive = TRUE)
if (!dir.exists(DESTINO))
  stop("Caminho nao encontrado. Verifique o acento em 'Area de Trabalho'.")


# ------------------------------------------------------------
# 0. Centroide de Taquari
# ------------------------------------------------------------

library(geobr)

mun_rs  <- read_municipality(code_muni = "RS", year = 2020)
taquari <- mun_rs %>% filter(code_muni == COD_IBGE)
stopifnot(nrow(taquari) == 1)

centro <- suppressWarnings(st_centroid(taquari))
xy     <- st_coordinates(centro)
LON    <- xy[1, "X"]
LAT    <- xy[1, "Y"]

cat(sprintf("Taquari/RS | lon %.5f | lat %.5f\n\n", LON, LAT))


# ------------------------------------------------------------
# Função de gravação no schema padrão
# ------------------------------------------------------------

gravar_diario <- function(df, fonte) {
  out <- df %>%
    filter(data >= INICIO, data <= FIM) %>%
    transmute(data = as.Date(data), prec = as.numeric(prec)) %>%
    mutate(prec = ifelse(prec < 0, NA_real_, prec)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  write.csv(out, file.path(DESTINO, "diario", paste0(fonte, ".csv")),
            row.names = FALSE)
  
  cat(sprintf("\n%-10s %5d dias | %s a %s | %d faltantes\n",
              fonte, nrow(out), min(out$data), max(out$data),
              sum(is.na(out$prec))))
  invisible(out)
}


# ############################################################
# PARTE A - BR-DWGD via brclimr
# ############################################################

library(brclimr)

# indicadores e estatísticas disponíveis
product_info("brdwgd")

# "pr" = precipitação
# "mean" = média das células que intersectam o município.
#   É o mais próximo do CHIRPS, que é célula única no centroide.
brdwgd_dia <- fetch_data(
  code_muni  = COD_IBGE,
  product    = "brdwgd",
  indicator  = "pr",
  statistics = "mean",
  date_start = INICIO,
  date_end   = FIM
) %>%
  transmute(data = as.Date(date), prec = value)

cat("\nBR-DWGD bruto:", nrow(brdwgd_dia), "linhas |",
    as.character(min(brdwgd_dia$data)), "a",
    as.character(max(brdwgd_dia$data)), "\n")

gravar_diario(brdwgd_dia, "BR_DWGD")

# >>> OLHE A DATA FINAL ACIMA <<<
# A metodologia do brclimr descreve a grade indo até 2020-07-31.
# Se for esse o caso, a série é mais curta que as outras fontes.
# Isso é aceitável: 1981-2020 dá ~474 blocos mensais.
# A alternativa (NetCDF do Xavier) exige ~6 GB de RAM e ambiente
# Python com miniconda, o que não compensa para um município.
# Fonte: https://github.com/AlexandreCandidoXavier/BR-DWGD


# ############################################################
# PARTE B - ANA / HidroWeb via hydrobr
# ############################################################

library(hydrobr)


# ---- B1. Inventário de postos pluviométricos no RS ----

inv <- inventory(
  states      = "RIO GRANDE DO SUL",
  stationType = "plu",
  as_sf       = TRUE,
  aoi         = NULL
)

cat("\nPostos pluviometricos no RS:", nrow(inv), "\n")
cat("Colunas:", paste(names(inv), collapse = ", "), "\n")


# ---- B2. Postos próximos (haversine, evita conflito de CRS) ----

haversine <- function(lat1, lon1, lat2, lon2) {
  R <- 6371
  dlat <- (lat2 - lat1) * pi/180
  dlon <- (lon2 - lon1) * pi/180
  a <- sin(dlat/2)^2 + cos(lat1*pi/180) * cos(lat2*pi/180) * sin(dlon/2)^2
  2 * R * asin(sqrt(a))
}

inv_perto <- as.data.frame(inv) %>%
  mutate(dist_km = round(haversine(lat, long, LAT, LON), 1)) %>%
  filter(dist_km <= 50) %>%
  arrange(dist_km) %>%
  select(station_code, name, lat, long, dist_km)

cat("\n--- Postos ate 50 km do centroide ---\n")
print(head(inv_perto, 25))
cat("Total:", nrow(inv_perto), "postos\n")

write.csv(inv_perto, file.path(DESTINO, "ana_postos_proximos.csv"),
          row.names = FALSE)

# ============================================================
# B3 + B4  -  Diagnóstico dos postos da ANA próximos de Taquari
# Substitui integralmente os blocos B3 e B4 anteriores.
# ============================================================
#
# Por que não usamos organize():
#   o organize() do hydrobr rejeitou 5 dos 6 postos por critério
#   interno, e o único aprovado não tem nenhum registro dentro de
#   1981-2025. Pivotamos os dados brutos diretamente.
#
# Formato bruto da ANA: uma linha por MÊS, com colunas chuva01..chuva31.
# Precisamos de uma linha por DIA para bater com as outras fontes.
# ============================================================

library(tidyr)

N_CANDIDATOS <- 71
candidatos   <- head(inv_perto$station_code, N_CANDIDATOS)


# ------------------------------------------------------------
# Pivot: matriz mensal -> série diária
# ------------------------------------------------------------

pivotar_ana <- function(bruto_df) {
  df <- as.data.frame(bruto_df)
  names(df) <- sub("^X[0-9]+\\.", "", names(df))   # remove prefixo do código
  
  cols_chuva <- grep("^chuva[0-9]{2}$", names(df), value = TRUE)
  if (length(cols_chuva) == 0 || !"data" %in% names(df)) return(NULL)
  
  tem_nivel <- "nivelconsistencia" %in% names(df)
  if (!tem_nivel) df$nivelconsistencia <- 1
  
  df %>%
    select(nivelconsistencia, data, all_of(cols_chuva)) %>%
    pivot_longer(all_of(cols_chuva), names_to = "dia", values_to = "prec") %>%
    mutate(
      dia      = as.integer(sub("chuva", "", dia)),
      mes_ini  = as.Date(data),
      data_dia = mes_ini + (dia - 1)
    ) %>%
    filter(!is.na(data_dia), month(data_dia) == month(mes_ini)) %>%
    # nivel 2 = consistido; preferir sobre o bruto (1) quando houver os dois
    arrange(data_dia, desc(nivelconsistencia)) %>%
    distinct(data_dia, .keep_all = TRUE) %>%
    transmute(data = data_dia, prec = as.numeric(prec)) %>%
    arrange(data)
}


# ------------------------------------------------------------
# Baixar e pivotar os candidatos
# ------------------------------------------------------------

series <- list()

for (cod in candidatos) {
  message("--- baixando ", cod)
  b <- try(stationsData(inventoryResult = inv %>% filter(station_code == cod),
                        deleteNAstations = FALSE), silent = TRUE)
  if (inherits(b, "try-error")) { message("  falha no download"); next }
  
  p <- try(pivotar_ana(b[[1]]), silent = TRUE)
  if (inherits(p, "try-error") || is.null(p) || nrow(p) == 0) {
    message("  falha no pivot"); next
  }
  
  series[[as.character(cod)]] <- p
  message(sprintf("  %d dias | %s a %s", nrow(p),
                  min(p$data), max(p$data)))
}

cat("\nSeries obtidas:", length(series), "de", length(candidatos), "\n")


# ------------------------------------------------------------
# Avaliar cobertura DENTRO de 1981-2025
# ------------------------------------------------------------

dias_periodo <- as.numeric(FIM - INICIO) + 1
anos_alvo    <- year(INICIO):year(FIM)

avaliar <- function(s, codigo) {
  no_periodo <- s %>% filter(data >= INICIO, data <= FIM, !is.na(prec))
  
  if (nrow(no_periodo) == 0) {
    return(data.frame(
      station_code = as.character(codigo),
      serie_inicio = as.character(min(s$data)),
      serie_fim    = as.character(max(s$data)),
      inicio_periodo = NA_character_, fim_periodo = NA_character_,
      dias_com_dado = 0L, pct_cobertura = 0,
      anos_cobertos = 0L, anos_completos = 0L,
      max_prec = NA_real_
    ))
  }
  
  por_ano <- no_periodo %>%
    mutate(ano = year(data)) %>%
    count(ano, name = "dias")
  
  data.frame(
    station_code   = as.character(codigo),
    serie_inicio   = as.character(min(s$data)),
    serie_fim      = as.character(max(s$data)),
    inicio_periodo = as.character(min(no_periodo$data)),
    fim_periodo    = as.character(max(no_periodo$data)),
    dias_com_dado  = nrow(no_periodo),
    pct_cobertura  = round(100 * nrow(no_periodo) / dias_periodo, 1),
    anos_cobertos  = nrow(por_ano),                  # anos com algum dado
    anos_completos = sum(por_ano$dias >= 350),       # anos praticamente cheios
    max_prec       = round(max(no_periodo$prec), 1)
  )
}

resumo <- bind_rows(
  lapply(seq_along(series), function(i)
    avaliar(series[[i]], names(series)[i]))
) %>%
  left_join(inv_perto %>% mutate(station_code = as.character(station_code)),
            by = "station_code") %>%
  arrange(desc(anos_completos), desc(pct_cobertura))

cat("\n========== COBERTURA EM 1981-2025 ==========\n")
print(resumo %>% select(station_code, name, dist_km, serie_inicio, serie_fim,
                        inicio_periodo, fim_periodo, dias_com_dado,
                        pct_cobertura, anos_cobertos, anos_completos, max_prec))

write.csv(resumo, file.path(DESTINO, "ana_candidatos.csv"), row.names = FALSE)


# ------------------------------------------------------------
# Recomendação automática
# ------------------------------------------------------------

viaveis <- resumo %>% filter(anos_completos >= 20)

cat("\n================= RECOMENDACAO =================\n")

if (nrow(viaveis) > 0) {
  m <- viaveis[1, ]
  cat(sprintf(
    "Melhor posto: %s - %s
  distancia ao centroide : %.1f km
  serie completa do posto: %s a %s
  dentro de 1981-2025    : %s a %s
  dias com dado          : %d de %d (%.1f%%)
  anos com algum dado    : %d de %d
  anos praticamente cheios (>=350 dias): %d
  maior chuva diaria     : %.1f mm

Preencha:  CODIGO_ANA <- %s\n",
    m$station_code, m$name, m$dist_km,
    m$serie_inicio, m$serie_fim,
    m$inicio_periodo, m$fim_periodo,
    m$dias_com_dado, dias_periodo, m$pct_cobertura,
    m$anos_cobertos, length(anos_alvo), m$anos_completos,
    m$max_prec, m$station_code))
} else {
  cat("Nenhum dos", N_CANDIDATOS, "postos tem 20+ anos completos no periodo.\n")
  cat("Opcoes: aumentar N_CANDIDATOS (ha", nrow(inv_perto),
      "postos em 50 km) ou seguir sem a ANA.\n")
}


# ------------------------------------------------------------
# Cobertura ano a ano do melhor posto: onde estao os buracos
# ------------------------------------------------------------

if (nrow(viaveis) > 0) {
  melhor <- viaveis$station_code[1]
  
  cat("\n--- Dias com dado por ano | posto", melhor, "---\n")
  
  series[[melhor]] %>%
    filter(data >= INICIO, data <= FIM) %>%
    mutate(ano = year(data)) %>%
    group_by(ano) %>%
    summarise(dias_com_dado = sum(!is.na(prec)), .groups = "drop") %>%
    right_join(data.frame(ano = anos_alvo), by = "ano") %>%
    mutate(dias_com_dado = ifelse(is.na(dias_com_dado), 0L, dias_com_dado),
           situacao = case_when(
             dias_com_dado >= 350 ~ "completo",
             dias_com_dado >= 180 ~ "parcial",
             dias_com_dado >    0 ~ "ruim",
             TRUE                 ~ "sem dado")) %>%
    as.data.frame() %>% print()
}

fonte_lista <- series   # o B5 usa isto

# ---- B5. Escolher o posto e gravar ----
# Olhe a recomendação impressa acima. Critério, nesta ordem:
# anos_completos, pct_cobertura, distância só como desempate.

CODIGO_ANA <- 2951024    # <-- preencha com o código do posto escolhido

if (!is.na(CODIGO_ANA)) {
  
  ana_dia <- fonte_lista[[as.character(CODIGO_ANA)]] %>% arrange(data)
  
  # grade completa de dias: sem isso o buraco não vira NA e a
  # regra de falhas do bloco mensal não enxerga a ausência
  grade   <- data.frame(data = seq(INICIO, FIM, by = "day"))
  ana_dia <- left_join(grade, ana_dia, by = "data")
  
  gravar_diario(ana_dia, "ANA")
  
  cat("\n--- Falhas por ano ---\n")
  falhas_ano <- ana_dia %>%
    mutate(ano = year(data)) %>%
    group_by(ano) %>%
    summarise(dias = n(), faltantes = sum(is.na(prec)),
              pct_falha = round(100 * faltantes / dias, 1),
              .groups = "drop") %>%
    as.data.frame()
  
  print(falhas_ano)
  write.csv(falhas_ano, file.path(DESTINO, "ana_falhas_por_ano.csv"),
            row.names = FALSE)
  
  # quantos blocos mensais sobrevivem à regra de MAX_FALHAS
  MAX_FALHAS <- 3
  blocos <- ana_dia %>%
    mutate(ano = year(data), mes = month(data)) %>%
    group_by(ano, mes) %>%
    summarise(faltantes = sum(is.na(prec)), .groups = "drop") %>%
    summarise(total = n(), validos = sum(faltantes <= MAX_FALHAS))
  
  cat(sprintf("\nBlocos mensais utilizaveis: %d de %d (%.1f%%)\n",
              blocos$validos, blocos$total,
              100 * blocos$validos / blocos$total))
  
} else {
  message("\n>> Preencha CODIGO_ANA e rode o B5.")
}


# ------------------------------------------------------------
# Confirmação
# ------------------------------------------------------------

cat("\n=== Arquivos em", DESTINO, "===\n")
print(list.files(DESTINO, recursive = TRUE))

