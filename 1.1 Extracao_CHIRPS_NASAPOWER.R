# ============================================================
# Extração de precipitação diária para Taquari / RS
# Quatro fontes: CHIRPS, BR-DWGD, NASA POWER, ANA
# Saída: séries diárias + máximos mensais + relatório de cobertura
# ============================================================
#
# ESTRUTURA
#   Bloco 0 - centroide de Taquari
#   Bloco 1 - CHIRPS        (automático)
#   Bloco 2 - NASA POWER    (automático)
#   Bloco 3 - BR-DWGD       (automático OU csv manual)
#   Bloco 4 - ANA/HidroWeb  (automático OU csv manual)
#   Bloco 5 - máximos mensais com regra de falhas
#   Bloco 6 - relatório de cobertura (início/fim de cada fonte)
#
# Rode bloco a bloco. Se um falhar, os outros seguem.
# ============================================================

install.packages(c("geobr","sf","dplyr","tidyr","lubridate","chirps","nasapower"))

library(sf)
library(dplyr)
library(tidyr)
library(lubridate)

INICIO <- as.Date("1981-01-01")
FIM    <- as.Date("2025-12-31")

# máximo de dias faltantes tolerados num bloco mensal
MAX_FALHAS <- 3

dir.create("saidas", showWarnings = FALSE)


# ============================================================
# BLOCO 0 - Centroide de Taquari
# ============================================================

library(geobr)

mun_rs   <- read_municipality(code_muni = "RS", year = 2020)
taquari  <- mun_rs %>% filter(name_muni == "Taquari")

centro <- suppressWarnings(st_centroid(taquari))
xy  <- st_coordinates(centro)
LON <- xy[1, "X"]
LAT <- xy[1, "Y"]
COD_IBGE <- taquari$code_muni[1]

cat(sprintf("Taquari/RS | lon %.5f | lat %.5f | IBGE %s\n", LON, LAT, COD_IBGE))


# ============================================================
# BLOCO 1 - CHIRPS  (0.05 graus ~ 5,3 km | 1981-presente)
# ============================================================

chirps_dia <- NULL

try({
  library(chirps)
  
  ponto <- data.frame(lon = LON, lat = LAT)
  
  # a API recusa intervalos longos; quebramos ano a ano
  anos <- year(INICIO):year(FIM)
  
  lista <- lapply(anos, function(a) {
    ini <- max(as.Date(sprintf("%d-01-01", a)), INICIO)
    fim <- min(as.Date(sprintf("%d-12-31", a)), FIM)
    message("CHIRPS ", a)
    r <- try(get_chirps(ponto, dates = c(as.character(ini), as.character(fim)),
                        server = "CHC", as.matrix = FALSE), silent = TRUE)
    if (inherits(r, "try-error")) { warning("falhou em ", a); return(NULL) }
    r
  })
  
  chirps_dia <- bind_rows(lista[!sapply(lista, is.null)]) %>%
    transmute(data = as.Date(date), prec = chirps) %>%
    mutate(prec = ifelse(prec < 0, NA_real_, prec)) %>%   # -9999 = faltante
    arrange(data) %>%
    distinct(data, .keep_all = TRUE)
  
  cat("CHIRPS:", nrow(chirps_dia), "dias\n")
}, silent = FALSE)


# ============================================================
# BLOCO 2 - NASA POWER  (MERRA-2 | 1981-presente)
# Reanálise: precipitação simulada, não medida
# ============================================================

power_dia <- NULL

try({
  library(nasapower)
  
  power_dia <- get_power(
    community   = "ag",
    lonlat      = c(LON, LAT),
    pars        = "PRECTOTCORR",
    dates       = c(as.character(INICIO), as.character(FIM)),
    temporal_api = "daily"
  ) %>%
    as.data.frame() %>%
    transmute(data = as.Date(YYYYMMDD), prec = PRECTOTCORR) %>%
    mutate(prec = ifelse(prec < 0, NA_real_, prec)) %>%     # -999 = faltante
    arrange(data)
  
  cat("NASA POWER:", nrow(power_dia), "dias\n")
}, silent = FALSE)

# ============================================================
# COLE ISTO DEPOIS DO BLOCO 2
# Usa chirps_dia e power_dia que já estão na memória.
# BR-DWGD e ANA entram depois: basta gravar os CSVs em
# saidas/diario/ com colunas data,prec e rodar o BLOCO 5 em diante.
# ============================================================


# ============================================================
# BLOCO 2.5 - Gravar as séries diárias no schema padrão
# ============================================================

dir.create("saidas/diario", showWarnings = FALSE, recursive = TRUE)

gravar_diario <- function(df, fonte) {
  if (is.null(df) || nrow(df) == 0) {
    message(fonte, ": vazio, nada gravado"); return(invisible(NULL))
  }
  out <- df %>%
    filter(data >= INICIO, data <= FIM) %>%
    transmute(data = as.Date(data), prec = as.numeric(prec)) %>%
    mutate(prec = ifelse(prec < 0, NA_real_, prec)) %>%
    distinct(data, .keep_all = TRUE) %>%
    arrange(data)
  
  write.csv(out, file.path("saidas/diario", paste0(fonte, ".csv")),
            row.names = FALSE)
  cat(sprintf("%-12s %5d dias | %s a %s\n",
              fonte, nrow(out), min(out$data), max(out$data)))
  invisible(out)
}

gravar_diario(chirps_dia, "CHIRPS")
gravar_diario(power_dia,  "NASA_POWER")


# ============================================================
# BLOCO 5 - Máximos mensais
# Lê tudo que estiver em saidas/diario/, então quando você
# adicionar BR-DWGD e ANA é só rerodar daqui para baixo.
# ============================================================

arquivos <- list.files("saidas/diario", pattern = "\\.csv$", full.names = TRUE)
fontes   <- tools::file_path_sans_ext(basename(arquivos))
cat("\nFontes encontradas:", paste(fontes, collapse = ", "), "\n\n")

diarios <- setNames(
  lapply(arquivos, function(f)
    read.csv(f) %>% mutate(data = as.Date(data), prec = as.numeric(prec))),
  fontes
)

# Regra de falhas: bloco com mais de MAX_FALHAS dias ausentes é
# descartado. Mês incompleto entra com máximo baixo e achata a
# cauda da GEV, criando viés que parece diferença entre fontes.
maximos_mensais <- function(df, fonte) {
  df %>%
    filter(data >= INICIO, data <= FIM) %>%
    mutate(ano = year(data), mes = month(data)) %>%
    group_by(ano, mes) %>%
    summarise(
      dias_esperados = as.integer(days_in_month(
        as.Date(sprintf("%d-%02d-01", first(ano), first(mes))))),
      dias_com_dado  = sum(!is.na(prec)),
      falhas         = dias_esperados - sum(!is.na(prec)),
      max_prec       = if (any(!is.na(prec))) max(prec, na.rm = TRUE) else NA_real_,
      data_do_max    = if (any(!is.na(prec)))
        data[which.max(replace(prec, is.na(prec), -Inf))]
      else as.Date(NA),
      .groups = "drop"
    ) %>%
    mutate(
      fonte   = fonte,
      ano_mes = sprintf("%d-%02d", ano, mes),
      valido  = falhas <= MAX_FALHAS & !is.na(max_prec)
    )
}

max_all <- bind_rows(lapply(names(diarios),
                            function(f) maximos_mensais(diarios[[f]], f)))


# ============================================================
# BLOCO 6 - Cobertura: onde cada fonte começa e termina
# ============================================================

n_meses <- length(seq(INICIO, FIM, by = "month"))   # 540 no periodo atual

cobertura <- max_all %>%
  group_by(fonte) %>%
  summarise(
    inicio          = min(ano_mes[valido]),
    fim             = max(ano_mes[valido]),
    blocos_validos  = sum(valido),
    blocos_totais   = n(),
    blocos_perdidos = sum(!valido),
    .groups = "drop"
  ) %>%
  mutate(meses_no_intervalo = n_meses,
         cobertura_pct      = round(100 * blocos_validos / n_meses, 1))

cat("=============== COBERTURA POR FONTE ===============\n")
print(as.data.frame(cobertura))

descartados <- max_all %>%
  filter(!valido) %>%
  select(fonte, ano_mes, dias_com_dado, falhas) %>%
  arrange(fonte, ano_mes)

cat("\nBlocos descartados:", nrow(descartados), "\n")
if (nrow(descartados) > 0) print(head(as.data.frame(descartados), 30))

comuns <- max_all %>% filter(valido) %>% count(ano_mes) %>%
  filter(n == length(fontes))
if (nrow(comuns) > 0)
  cat(sprintf("\nPeriodo comum as %d fontes: %s a %s (%d blocos)\n",
              length(fontes), min(comuns$ano_mes), max(comuns$ano_mes),
              nrow(comuns)))


# ============================================================
# BLOCO 7 - VALIDACAO CONTRA O TCC DA GLENDA
# Recorte 1981-01-01 a 2025-02-28, apenas CHIRPS.
# Se isto bater, a extracao esta correta.
# ============================================================

if ("CHIRPS" %in% fontes) {
  
  ref <- max_all %>%
    filter(fonte == "CHIRPS", valido, ano_mes <= "2025-02") %>%
    pull(max_prec)
  
  assimetria <- function(x) mean((x - mean(x))^3) / sd(x)^3
  curtose_f  <- function(x) mean((x - mean(x))^4) / sd(x)^4 - 3
  
  comp <- data.frame(
    metrica = c("N","Media","Mediana","DesvioPadrao",
                "Minimo","Maximo","Assimetria","Curtose"),
    glenda  = c(530, 37.78, 35.05, 15.93, 6.50, 119.23, 1.26, 2.81),
    obtido  = round(c(length(ref), mean(ref), median(ref), sd(ref),
                      min(ref), max(ref), assimetria(ref), curtose_f(ref)), 2)
  ) %>% mutate(diferenca = round(obtido - glenda, 2))
  
  cat("\n===== VALIDACAO CONTRA O TCC (ate fev/2025) =====\n")
  print(comp, row.names = FALSE)
  
  cat("\nPercentil 99%:", round(quantile(ref, 0.99), 2),
      " (o TCC adotou limiar de 80 mm, situado acima do p99)\n")
  
  lb <- Box.test(ref, lag = 12, type = "Ljung-Box")
  cat(sprintf("Ljung-Box(12): Q = %.2f | p = %.2f   (TCC: 13,36 / 0,34)\n",
              lb$statistic, lb$p.value))
  
  if (requireNamespace("Kendall", quietly = TRUE)) {
    mk <- Kendall::MannKendall(ref)
    cat(sprintf("Mann-Kendall : tau = %.3f | p = %.2f  (TCC: 0,03 / 0,24)\n",
                mk$tau, mk$sl))
  } else {
    cat("install.packages('Kendall') para o teste de Mann-Kendall\n")
  }
  
  # Ajuste GEV - alvos do TCC: alpha 30,62 | beta 12,18 | xi 0,01
  if (requireNamespace("extRemes", quietly = TRUE)) {
    fit <- extRemes::fevd(ref, type = "GEV", method = "MLE")
    p   <- fit$results$par
    cat(sprintf("\nGEV MLE: loc = %.2f | scale = %.2f | shape = %.3f\n",
                p["location"], p["scale"], p["shape"]))
    cat("TCC    : loc = 30.62 | scale = 12.18 | shape = 0.010\n")
  } else {
    cat("install.packages('extRemes') para o ajuste GEV\n")
  }
  
  write.csv(comp, "saidas/validacao_tcc.csv", row.names = FALSE)
}


# ============================================================
# BLOCO 8 - Saidas
# ============================================================

max_wide <- max_all %>%
  filter(valido) %>%
  select(ano_mes, ano, mes, fonte, max_prec) %>%
  pivot_wider(names_from = fonte, values_from = max_prec) %>%
  arrange(ano_mes)

# ============================================================
# BLOCO 8 - Gravacao de todas as saidas
# Cole logo depois de   max_wide <- ...
# ============================================================

DESTINO <- "C:/Users/Patrick Filho/OneDrive/Área de Trabalho/LabMA/SIAC"

dir.create(DESTINO,                      showWarnings = FALSE, recursive = TRUE)
dir.create(file.path(DESTINO, "diario"), showWarnings = FALSE, recursive = TRUE)

if (!dir.exists(DESTINO))
  stop("Caminho nao encontrado. Verifique o acento em 'Area de Trabalho'.")


# ---- séries diárias (matéria-prima, insubstituível) ----

diarios_orig <- list.files("saidas/diario", pattern = "\\.csv$",
                           full.names = TRUE)

if (length(diarios_orig) > 0) {
  file.copy(diarios_orig, file.path(DESTINO, "diario"), overwrite = TRUE)
} else {
  # se o Bloco 2.5 nao rodou, grava direto da memoria
  if (exists("chirps_dia") && !is.null(chirps_dia))
    write.csv(chirps_dia, file.path(DESTINO, "diario", "CHIRPS.csv"),
              row.names = FALSE)
  if (exists("power_dia") && !is.null(power_dia))
    write.csv(power_dia, file.path(DESTINO, "diario", "NASA_POWER.csv"),
              row.names = FALSE)
}


# ---- tabelas processadas ----

write.csv(max_wide,    file.path(DESTINO, "maximos_mensais_wide.csv"), row.names = FALSE)
write.csv(max_all,     file.path(DESTINO, "maximos_mensais_long.csv"), row.names = FALSE)
write.csv(cobertura,   file.path(DESTINO, "cobertura.csv"),            row.names = FALSE)
write.csv(descartados, file.path(DESTINO, "blocos_descartados.csv"),   row.names = FALSE)

if (exists("comp"))
  write.csv(comp, file.path(DESTINO, "validacao_tcc.csv"), row.names = FALSE)


# ---- coordenadas usadas, para reprodutibilidade ----

write.csv(
  data.frame(municipio = "Taquari/RS", cod_ibge = COD_IBGE,
             lon = LON, lat = LAT,
             inicio = as.character(INICIO), fim = as.character(FIM),
             max_falhas = MAX_FALHAS),
  file.path(DESTINO, "parametros_extracao.csv"), row.names = FALSE
)


# ---- confirmacao ----

cat("\nGravado em:", DESTINO, "\n\n")
print(list.files(DESTINO, recursive = TRUE))

cat("\nResumo dos maximos mensais:\n")
print(summary(max_wide[, !names(max_wide) %in% c("ano_mes", "ano", "mes")]))
