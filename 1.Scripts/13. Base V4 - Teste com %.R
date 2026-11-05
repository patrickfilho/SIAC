# ============================================================
# BASE TRATADA V4
#
# Unidade:
# GRID x MES
#
# Objetivo:
#
# Reduzir a base de risk-group x grid x mes para uma linha
# por grid-mes, mantendo a composicao da carteira por meio
# de percentuais.
#
#
# Y_{g,t} ~ NB(lambda_{g,t}, phi)
#
# lambda_{g,t}
# =
# E_g *
# [
#   exp(beta'X_{g,t})
#   +
#   psi_g * (A_queen * Ybar_{t-1})_g
# ]
#
#
# IMPORTANTE:
#
# - Exposicao = numero de domicilios segurados no grid
# - Total mensal de exposicao = 15.000
# - Processo gerador verdadeiro = QUEEN
# - PHI global
# - PSI especifico por grid
# - Uma linha por grid-mes
#
# ============================================================


# ------------------------------------------------------------
# 0. PACOTES
# ------------------------------------------------------------

library(dplyr)
library(tidyr)
library(readr)
library(tibble)


# ------------------------------------------------------------
# 1. CONFIGURACOES
# ------------------------------------------------------------

set.seed(54321)

ARQUIVO_BASE <-
  "base_tratada_v3.csv"

ARQUIVO_SAIDA <-
  "base_tratada_v4.csv"


N_DOMICILIOS <- 15000


# ------------------------------------------------------------
# Parametros gerais da simulacao
# ------------------------------------------------------------

# Dispersao global da Negative Binomial
#
# Maior phi = menor overdispersion.
#
# Estamos usando overdispersion moderada para que exista
# ruido, mas o sinal estrutural ainda seja identificavel.

PHI_TRUE <- 8


# Quantil utilizado para definir um shock

QUANTIL_SHOCK <- 0.80


# ------------------------------------------------------------
# Intensidade desejada do componente espacial
# ------------------------------------------------------------

# Queremos que, quando o efeito espacial estiver ativo,
# ele seja material.
#
# TARGET_SPATIAL_SHARE = 0.20 significa que,
# para um nivel tipico de atividade espacial,
# aproximadamente 20% da taxa pode vir do componente espacial.

TARGET_SPATIAL_SHARE <- 0.20


# Numero de vizinhos em shock usado como referencia
# para calibrar psi.

S_REF <- 2


# ============================================================
# 2. LER BASE V3
# ============================================================

base_v3 <- readr::read_csv(
  ARQUIVO_BASE,
  show_col_types = FALSE
)


# ------------------------------------------------------------
# Validar colunas
# ------------------------------------------------------------

colunas_necessarias <- c(
  
  "risk_group_id",
  
  "id_celula",
  "linha",
  "coluna",
  
  "ano",
  "mes",
  "data_mes",
  
  "pisos",
  "qualidade",
  "idade_faixa",
  
  "n_domicilios",
  
  "prec_mensal_soma",
  "prec_mensal_max",
  "dias_acima_threshold",
  "prec_mensal_soma_lag1"
  
)


faltantes <- setdiff(
  colunas_necessarias,
  names(base_v3)
)


if (length(faltantes) > 0) {
  
  stop(
    
    paste(
      "Colunas faltantes:",
      paste(
        faltantes,
        collapse = ", "
      )
    )
    
  )
  
}


base_v3 <- base_v3 %>%
  
  dplyr::mutate(
    
    id_celula =
      as.character(id_celula),
    
    ano =
      as.integer(ano),
    
    mes =
      as.integer(mes),
    
    data_mes =
      as.Date(data_mes)
    
  )


# ============================================================
# 3. EXTRAIR A CARTEIRA UNICA
# ============================================================

# Na V3 o mesmo risk group aparece repetido em todos os meses.
#
# Para construir os percentuais do grid, precisamos de apenas
# uma linha por:
#
# grid x risk group


carteira <- base_v3 %>%
  
  dplyr::distinct(
    
    id_celula,
    
    linha,
    coluna,
    
    risk_group_id,
    
    pisos,
    qualidade,
    idade_faixa,
    
    n_domicilios
    
  )


cat(
  "\nNumero de registros unicos da carteira:",
  nrow(carteira),
  "\n"
)


cat(
  "Total de domicilios:",
  sum(carteira$n_domicilios),
  "\n"
)


stopifnot(
  
  sum(
    carteira$n_domicilios
  ) ==
    N_DOMICILIOS
  
)


# ============================================================
# 4. EXPOSICAO POR GRID
# ============================================================

exposicao_grid <- carteira %>%
  
  dplyr::group_by(
    
    id_celula,
    linha,
    coluna
    
  ) %>%
  
  dplyr::summarise(
    
    exposicao =
      sum(
        n_domicilios
      ),
    
    .groups =
      "drop"
    
  )


cat(
  "\nExposicao por grid:\n"
)


print(
  exposicao_grid,
  n = Inf
)


cat(
  "\nTotal:",
  sum(exposicao_grid$exposicao),
  "\n"
)


# ============================================================
# 5. COMPOSICAO DA CARTEIRA: PISOS
# ============================================================

composicao_pisos <- carteira %>%
  
  dplyr::group_by(
    id_celula
  ) %>%
  
  dplyr::summarise(
    
    pct_pisos_1 =
      sum(
        n_domicilios[
          pisos == "1"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_pisos_2 =
      sum(
        n_domicilios[
          pisos == "2"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_pisos_3mais =
      sum(
        n_domicilios[
          pisos == "3+"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    .groups =
      "drop"
    
  )


# ============================================================
# 6. COMPOSICAO DA CARTEIRA: QUALIDADE
# ============================================================

composicao_qualidade <- carteira %>%
  
  dplyr::group_by(
    id_celula
  ) %>%
  
  dplyr::summarise(
    
    pct_qualidade_boa =
      sum(
        n_domicilios[
          qualidade == "Boa"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_qualidade_media =
      sum(
        n_domicilios[
          qualidade == "Media"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_qualidade_baixa =
      sum(
        n_domicilios[
          qualidade == "Baixa"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    .groups =
      "drop"
    
  )


# ============================================================
# 7. COMPOSICAO DA CARTEIRA: IDADE
# ============================================================

composicao_idade <- carteira %>%
  
  dplyr::group_by(
    id_celula
  ) %>%
  
  dplyr::summarise(
    
    pct_idade_0_10 =
      sum(
        n_domicilios[
          idade_faixa == "0-10"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_idade_11_20 =
      sum(
        n_domicilios[
          idade_faixa == "11-20"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_idade_21_30 =
      sum(
        n_domicilios[
          idade_faixa == "21-30"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    pct_idade_30mais =
      sum(
        n_domicilios[
          idade_faixa == "30+"
        ]
      ) /
      sum(
        n_domicilios
      ),
    
    .groups =
      "drop"
    
  )


# ============================================================
# 8. CRIAR TABELA DA CARTEIRA POR GRID
# ============================================================

carteira_grid <- exposicao_grid %>%
  
  dplyr::left_join(
    
    composicao_pisos,
    
    by =
      "id_celula"
    
  ) %>%
  
  dplyr::left_join(
    
    composicao_qualidade,
    
    by =
      "id_celula"
    
  ) %>%
  
  dplyr::left_join(
    
    composicao_idade,
    
    by =
      "id_celula"
    
  )


# ============================================================
# 9. VALIDAR PERCENTUAIS
# ============================================================

carteira_grid <- carteira_grid %>%
  
  dplyr::mutate(
    
    soma_pisos =
      
      pct_pisos_1 +
      pct_pisos_2 +
      pct_pisos_3mais,
    
    
    soma_qualidade =
      
      pct_qualidade_boa +
      pct_qualidade_media +
      pct_qualidade_baixa,
    
    
    soma_idade =
      
      pct_idade_0_10 +
      pct_idade_11_20 +
      pct_idade_21_30 +
      pct_idade_30mais
    
  )


cat(
  "\nChecagem dos percentuais:\n"
)


print(
  
  summary(
    
    carteira_grid %>%
      
      dplyr::select(
        
        soma_pisos,
        soma_qualidade,
        soma_idade
        
      )
    
  )
  
)


# ============================================================
# 10. EXTRAIR BASE CLIMATICA GRID-MES
# ============================================================

# Como na V3 o clima se repete para todos os risk groups,
# usamos distinct() para obter uma observacao por grid-mes.


clima <- base_v3 %>%
  
  dplyr::distinct(
    
    id_celula,
    linha,
    coluna,
    
    ano,
    mes,
    data_mes,
    
    prec_mensal_soma,
    prec_mensal_max,
    dias_acima_threshold,
    prec_mensal_soma_lag1
    
  ) %>%
  
  dplyr::arrange(
    
    data_mes,
    linha,
    coluna
    
  )


cat(
  "\nLinhas da base climatica:",
  nrow(clima),
  "\n"
)


stopifnot(
  
  nrow(clima) ==
    7500
  
)


# ============================================================
# 11. CRIAR BASE GRID-MES
# ============================================================

base_v4 <- clima %>%
  
  dplyr::left_join(
    
    carteira_grid %>%
      
      dplyr::select(
        -soma_pisos,
        -soma_qualidade,
        -soma_idade
      ),
    
    by =
      c(
        "id_celula",
        "linha",
        "coluna"
      )
    
  )


# ============================================================
# 12. CHECAR EXPOSICAO MENSAL
# ============================================================

checagem_exposicao <- base_v4 %>%
  
  dplyr::group_by(
    ano,
    mes
  ) %>%
  
  dplyr::summarise(
    
    exposicao_total =
      sum(
        exposicao
      ),
    
    .groups =
      "drop"
    
  )


if (
  
  all(
    checagem_exposicao$exposicao_total ==
    N_DOMICILIOS
  )
  
) {
  
  cat(
    "\nOK: todos os meses possuem exposicao total =",
    N_DOMICILIOS,
    "\n"
  )
  
} else {
  
  stop(
    "A exposicao mensal nao soma 15.000."
  )
  
}


# ============================================================
# 13. PADRONIZAR VARIAVEIS CLIMATICAS
# ============================================================

padronizar <- function(x) {
  
  (
    x -
      mean(
        x,
        na.rm = TRUE
      )
  ) /
    
    sd(
      x,
      na.rm = TRUE
    )
  
}


base_v4 <- base_v4 %>%
  
  dplyr::mutate(
    
    z_prec_mensal =
      padronizar(
        prec_mensal_soma
      ),
    
    z_prec_max =
      padronizar(
        prec_mensal_max
      ),
    
    z_dias_extremos =
      padronizar(
        dias_acima_threshold
      ),
    
    z_prec_lag1 =
      padronizar(
        prec_mensal_soma_lag1
      )
    
  )


# ============================================================
# 14. PADRONIZAR COMPOSICAO DA CARTEIRA
# ============================================================

# IMPORTANTE:
#
# No modelo vamos utilizar somente as categorias que nao sao
# referencia.
#
# Referencias:
#
# pisos       -> 1 piso
# qualidade   -> Boa
# idade       -> 0-10 anos
#
#
# Mantemos os percentuais das referencias no CSV para
# interpretacao, mas nao precisam entrar na regressao.


# ------------------------------------------------------------
# Criar tabela unica de 25 grids para padronizar percentuais
# ------------------------------------------------------------

carteira_z <- base_v4 %>%
  
  dplyr::distinct(
    
    id_celula,
    
    pct_pisos_2,
    pct_pisos_3mais,
    
    pct_qualidade_media,
    pct_qualidade_baixa,
    
    pct_idade_11_20,
    pct_idade_21_30,
    pct_idade_30mais
    
  )


# ------------------------------------------------------------
# Funcao segura de z-score
# ------------------------------------------------------------

z_seguro <- function(x) {
  
  sx <-
    sd(
      x,
      na.rm = TRUE
    )
  
  if (
    is.na(sx) ||
    sx == 0
  ) {
    
    return(
      rep(
        0,
        length(x)
      )
    )
    
  }
  
  
  (
    x -
      mean(
        x,
        na.rm = TRUE
      )
  ) /
    sx
  
}


carteira_z <- carteira_z %>%
  
  dplyr::mutate(
    
    z_pct_pisos_2 =
      z_seguro(
        pct_pisos_2
      ),
    
    z_pct_pisos_3mais =
      z_seguro(
        pct_pisos_3mais
      ),
    
    z_pct_qualidade_media =
      z_seguro(
        pct_qualidade_media
      ),
    
    z_pct_qualidade_baixa =
      z_seguro(
        pct_qualidade_baixa
      ),
    
    z_pct_idade_11_20 =
      z_seguro(
        pct_idade_11_20
      ),
    
    z_pct_idade_21_30 =
      z_seguro(
        pct_idade_21_30
      ),
    
    z_pct_idade_30mais =
      z_seguro(
        pct_idade_30mais
      )
    
  )


base_v4 <- base_v4 %>%
  
  dplyr::left_join(
    
    carteira_z %>%
      
      dplyr::select(
        
        id_celula,
        
        starts_with(
          "z_pct"
        )
        
      ),
    
    by =
      "id_celula"
    
  )


# ============================================================
# 15. IDENTIFICAR GRIDS
# ============================================================

grids <- base_v4 %>%
  
  dplyr::distinct(
    
    id_celula,
    linha,
    coluna
    
  ) %>%
  
  dplyr::arrange(
    
    linha,
    coluna
    
  )


G <-
  nrow(grids)


stopifnot(
  G == 25
)


grid_order <-
  grids$id_celula


# ============================================================
# 16. MATRIZ QUEEN
# ============================================================

A_queen <- matrix(
  
  0,
  
  nrow =
    G,
  
  ncol =
    G
  
)


for (
  i in seq_len(G)
) {
  
  for (
    j in seq_len(G)
  ) {
    
    
    if (
      i != j
    ) {
      
      
      dl <-
        abs(
          grids$linha[i] -
            grids$linha[j]
        )
      
      
      dc <-
        abs(
          grids$coluna[i] -
            grids$coluna[j]
        )
      
      
      if (
        
        dl <= 1 &&
        dc <= 1
        
      ) {
        
        A_queen[i, j] <-
          1
        
      }
      
    }
    
  }
  
}


rownames(
  A_queen
) <-
  grid_order


colnames(
  A_queen
) <-
  grid_order


# ============================================================
# 17. MATRIZ ROOK
# ============================================================

A_rook <- matrix(
  
  0,
  
  nrow =
    G,
  
  ncol =
    G
  
)


for (
  i in seq_len(G)
) {
  
  for (
    j in seq_len(G)
  ) {
    
    
    if (
      i != j
    ) {
      
      
      dl <-
        abs(
          grids$linha[i] -
            grids$linha[j]
        )
      
      
      dc <-
        abs(
          grids$coluna[i] -
            grids$coluna[j]
        )
      
      
      if (
        dl + dc ==
        1
      ) {
        
        A_rook[i, j] <-
          1
        
      }
      
    }
    
  }
  
}


rownames(
  A_rook
) <-
  grid_order


colnames(
  A_rook
) <-
  grid_order


# ------------------------------------------------------------
# Validacoes
# ------------------------------------------------------------

stopifnot(
  
  all(
    diag(
      A_queen
    ) == 0
  ),
  
  all(
    diag(
      A_rook
    ) == 0
  ),
  
  isTRUE(
    all.equal(
      A_queen,
      t(
        A_queen
      )
    )
  ),
  
  isTRUE(
    all.equal(
      A_rook,
      t(
        A_rook
      )
    )
  )
  
)


cat(
  "\nPares Rook:",
  sum(A_rook) / 2,
  "\n"
)


cat(
  "Pares Queen:",
  sum(A_queen) / 2,
  "\n"
)


# ============================================================
# 18. DEFINIR BETAS VERDADEIROS
# ============================================================

# Agora os coeficientes da carteira atuam sobre as
# PROPORCOES PADRONIZADAS de cada caracteristica.
#
# Isso permite que as diferencas de composicao dos grids
# tenham efeito identificavel mesmo que os percentuais brutos
# nao variem enormemente.


BETA_TRUE <- c(
  
  # ----------------------------------------------------------
  # Baseline
  # ----------------------------------------------------------
  
  intercepto =
    log(0.002),
  
  
  # ----------------------------------------------------------
  # Clima
  # ----------------------------------------------------------
  
  z_prec_mensal =
    0.30,
  
  z_prec_max =
    0.22,
  
  z_dias_extremos =
    0.18,
  
  z_prec_lag1 =
    0.12,
  
  
  # ----------------------------------------------------------
  # Composicao da carteira
  # ----------------------------------------------------------
  
  z_pct_pisos_2 =
    0.08,
  
  z_pct_pisos_3mais =
    0.14,
  
  z_pct_qualidade_media =
    0.08,
  
  z_pct_qualidade_baixa =
    0.16,
  
  z_pct_idade_11_20 =
    0.05,
  
  z_pct_idade_21_30 =
    0.10,
  
  z_pct_idade_30mais =
    0.16
  
)


# ============================================================
# 19. CALCULAR TAXA BASE VERDADEIRA
# ============================================================

eta_base <-
  
  BETA_TRUE[
    "intercepto"
  ] +
  
  BETA_TRUE[
    "z_prec_mensal"
  ] *
  base_v4$z_prec_mensal +
  
  BETA_TRUE[
    "z_prec_max"
  ] *
  base_v4$z_prec_max +
  
  BETA_TRUE[
    "z_dias_extremos"
  ] *
  base_v4$z_dias_extremos +
  
  BETA_TRUE[
    "z_prec_lag1"
  ] *
  base_v4$z_prec_lag1 +
  
  BETA_TRUE[
    "z_pct_pisos_2"
  ] *
  base_v4$z_pct_pisos_2 +
  
  BETA_TRUE[
    "z_pct_pisos_3mais"
  ] *
  base_v4$z_pct_pisos_3mais +
  
  BETA_TRUE[
    "z_pct_qualidade_media"
  ] *
  base_v4$z_pct_qualidade_media +
  
  BETA_TRUE[
    "z_pct_qualidade_baixa"
  ] *
  base_v4$z_pct_qualidade_baixa +
  
  BETA_TRUE[
    "z_pct_idade_11_20"
  ] *
  base_v4$z_pct_idade_11_20 +
  
  BETA_TRUE[
    "z_pct_idade_21_30"
  ] *
  base_v4$z_pct_idade_21_30 +
  
  BETA_TRUE[
    "z_pct_idade_30mais"
  ] *
  base_v4$z_pct_idade_30mais


base_v4$taxa_base_true <-
  exp(
    eta_base
  )


# ============================================================
# 20. CALIBRAR PSI
# ============================================================

# Queremos aproximadamente:
#
# spatial /
# (baseline + spatial)
#
# = TARGET_SPATIAL_SHARE
#
#
# Para um caso de referencia com:
#
# spatial_lag = S_REF
#
#
# Logo:
#
# psi =
#
# [share / (1-share)]
# *
# baseline
# /
# S_REF


taxa_base_mediana <-
  median(
    base_v4$taxa_base_true,
    na.rm = TRUE
  )


PSI_BASE <-
  
  (
    TARGET_SPATIAL_SHARE /
      (
        1 -
          TARGET_SPATIAL_SHARE
      )
  ) *
  
  taxa_base_mediana /
  
  S_REF


cat(
  "\nTaxa baseline mediana:",
  taxa_base_mediana,
  "\n"
)


cat(
  "Psi base calibrado:",
  PSI_BASE,
  "\n"
)


# ------------------------------------------------------------
# Pequena heterogeneidade entre grids
# ------------------------------------------------------------

psi_true <- PSI_BASE *
  
  runif(
    
    G,
    
    min =
      0.75,
    
    max =
      1.25
    
  )


names(
  psi_true
) <-
  grid_order


# ============================================================
# 21. FREQUENCIA DE REFERENCIA DO SHOCK
# ============================================================

# Utilizamos uma media de referencia da taxa baseline,
# ponderada pela exposicao.
#
# Ela nao usa Y observado futuro.

LAMBDA_SHOCK_REF <-
  
  weighted.mean(
    
    base_v4$taxa_base_true,
    
    w =
      base_v4$exposicao
    
  )


cat(
  "\nFrequencia mensal de referencia para shock:",
  LAMBDA_SHOCK_REF,
  "\n"
)


# ============================================================
# 22. PREPARAR COLUNAS DA SIMULACAO
# ============================================================

base_v4 <- base_v4 %>%
  
  dplyr::mutate(
    
    spatial_lag_queen_true =
      0,
    
    taxa_spatial_true =
      0,
    
    lambda_true =
      NA_real_,
    
    sinistros =
      NA_integer_,
    
    lambda_ref_shock =
      NA_real_,
    
    threshold_ybar =
      NA_integer_,
    
    y_bar =
      0L
    
  )


# ============================================================
# 23. SIMULAR SEQUENCIALMENTE
# ============================================================

meses <-
  sort(
    unique(
      base_v4$data_mes
    )
  )


# Vetor do Y_bar anterior

ybar_anterior <- rep(
  0,
  G
)

names(
  ybar_anterior
) <-
  grid_order


for (
  tt in seq_along(
    meses
  )
) {
  
  
  mes_atual <-
    meses[tt]
  
  
  idx <-
    which(
      base_v4$data_mes ==
        mes_atual
    )
  
  
  dados_mes <-
    base_v4[
      idx,
    ]
  
  
  # ----------------------------------------------------------
  # 23.1 Calcular A_queen %*% Ybar_{t-1}
  # ----------------------------------------------------------
  
  if (
    tt == 1
  ) {
    
    S <- rep(
      0,
      G
    )
    
  } else {
    
    S <-
      as.numeric(
        
        A_queen %*%
          ybar_anterior
        
      )
    
  }
  
  
  names(S) <-
    grid_order
  
  
  spatial_lag <-
    
    S[
      dados_mes$id_celula
    ]
  
  
  # ----------------------------------------------------------
  # 23.2 Componente espacial
  # ----------------------------------------------------------
  
  psi_linha <-
    
    psi_true[
      dados_mes$id_celula
    ]
  
  
  taxa_spatial <-
    
    psi_linha *
    spatial_lag
  
  
  # ----------------------------------------------------------
  # 23.3 Media total
  # ----------------------------------------------------------
  
  lambda <-
    
    dados_mes$exposicao *
    
    (
      dados_mes$taxa_base_true +
        taxa_spatial
    )
  
  
  lambda <-
    pmax(
      lambda,
      1e-12
    )
  
  
  # ----------------------------------------------------------
  # 23.4 Simular sinistros
  # ----------------------------------------------------------
  
  Y <-
    
    rnbinom(
      
      n =
        length(lambda),
      
      mu =
        lambda,
      
      size =
        PHI_TRUE
      
    )
  
  
  # ----------------------------------------------------------
  # 23.5 Construir threshold para shock
  # ----------------------------------------------------------
  
  lambda_ref_shock <-
    
    LAMBDA_SHOCK_REF *
    dados_mes$exposicao
  
  
  threshold_ybar <-
    
    qpois(
      
      QUANTIL_SHOCK,
      
      lambda =
        lambda_ref_shock
      
    )
  
  
  y_bar <-
    
    as.integer(
      
      Y >
        threshold_ybar
      
    )
  
  
  # ----------------------------------------------------------
  # 23.6 Gravar
  # ----------------------------------------------------------
  
  base_v4$spatial_lag_queen_true[
    idx
  ] <-
    spatial_lag
  
  
  base_v4$taxa_spatial_true[
    idx
  ] <-
    taxa_spatial
  
  
  base_v4$lambda_true[
    idx
  ] <-
    lambda
  
  
  base_v4$sinistros[
    idx
  ] <-
    Y
  
  
  base_v4$lambda_ref_shock[
    idx
  ] <-
    lambda_ref_shock
  
  
  base_v4$threshold_ybar[
    idx
  ] <-
    threshold_ybar
  
  
  base_v4$y_bar[
    idx
  ] <-
    y_bar
  
  
  # ----------------------------------------------------------
  # Atualizar para o proximo mes
  # ----------------------------------------------------------
  
  ybar_anterior <-
    y_bar
  
  
  names(
    ybar_anterior
  ) <-
    dados_mes$id_celula
  
  
  # Garantir ordem da matriz A
  
  ybar_anterior <-
    
    ybar_anterior[
      grid_order
    ]
  
}


# ============================================================
# 24. CALCULAR SPATIAL LAG ROOK PARA DIAGNOSTICO
# ============================================================

base_v4$spatial_lag_rook <-
  0


for (
  tt in 2:length(meses)
) {
  
  
  mes_anterior <-
    meses[
      tt - 1
    ]
  
  
  mes_atual <-
    meses[
      tt
    ]
  
  
  anterior <-
    base_v4 %>%
    
    dplyr::filter(
      data_mes ==
        mes_anterior
    )
  
  
  vetor_ybar <- rep(
    0,
    G
  )
  
  
  names(
    vetor_ybar
  ) <-
    grid_order
  
  
  vetor_ybar[
    anterior$id_celula
  ] <-
    anterior$y_bar
  
  
  S_rook <-
    
    as.numeric(
      
      A_rook %*%
        vetor_ybar
      
    )
  
  
  names(
    S_rook
  ) <-
    grid_order
  
  
  idx <-
    which(
      base_v4$data_mes ==
        mes_atual
    )
  
  
  base_v4$spatial_lag_rook[
    idx
  ] <-
    
    S_rook[
      base_v4$id_celula[
        idx
      ]
    ]
  
}


# ============================================================
# 25. SHARE DO COMPONENTE ESPACIAL
# ============================================================

base_v4 <- base_v4 %>%
  
  dplyr::mutate(
    
    share_spatial_true =
      
      if_else(
        
        taxa_base_true +
          taxa_spatial_true >
          0,
        
        taxa_spatial_true /
          (
            taxa_base_true +
              taxa_spatial_true
          ),
        
        0
        
      )
    
  )


# ============================================================
# 26. DIAGNOSTICOS DA SIMULACAO
# ============================================================

cat(
  "\n=============================================\n"
)

cat(
  "BASE V4 - RESUMO\n"
)

cat(
  "=============================================\n"
)


cat(
  "Linhas:",
  nrow(base_v4),
  "\n"
)


cat(
  "Grids:",
  dplyr::n_distinct(
    base_v4$id_celula
  ),
  "\n"
)


cat(
  "Meses:",
  dplyr::n_distinct(
    base_v4$data_mes
  ),
  "\n"
)


cat(
  "Exposicao mensal:",
  N_DOMICILIOS,
  "\n"
)


cat(
  "Total de sinistros:",
  sum(
    base_v4$sinistros
  ),
  "\n"
)


cat(
  "Media mensal de sinistros:",
  sum(
    base_v4$sinistros
  ) /
    length(
      meses
    ),
  "\n"
)


cat(
  "Frequencia mensal observada:",
  sum(
    base_v4$sinistros
  ) /
    sum(
      base_v4$exposicao
    ),
  "\n"
)


cat(
  "Frequencia anualizada aproximada:",
  12 *
    sum(
      base_v4$sinistros
    ) /
    sum(
      base_v4$exposicao
    ),
  "\n"
)


cat(
  "Grid-meses com shock:",
  sum(
    base_v4$y_bar
  ),
  "\n"
)


cat(
  "Percentual grid-meses com shock:",
  mean(
    base_v4$y_bar
  ) *
    100,
  "%\n"
)


cat(
  "Media spatial lag Queen:",
  mean(
    base_v4$spatial_lag_queen_true
  ),
  "\n"
)


cat(
  "Media spatial lag Rook:",
  mean(
    base_v4$spatial_lag_rook
  ),
  "\n"
)


# ============================================================
# 27. IMPORTANCIA DO COMPONENTE ESPACIAL
# ============================================================

ativos <- base_v4 %>%
  
  dplyr::filter(
    spatial_lag_queen_true >
      0
  )


cat(
  "\nShare espacial quando ativo:\n"
)


print(
  
  summary(
    
    ativos$share_spatial_true
    
  )
  
)


cat(
  "\nShare espacial medio quando ativo:",
  mean(
    ativos$share_spatial_true
  ),
  "\n"
)


cat(
  "Share espacial mediano quando ativo:",
  median(
    ativos$share_spatial_true
  ),
  "\n"
)


# ============================================================
# 28. SINISTROS POR GRID
# ============================================================

sinistros_grid <- base_v4 %>%
  
  dplyr::group_by(
    
    id_celula,
    linha,
    coluna
    
  ) %>%
  
  dplyr::summarise(
    
    exposicao =
      dplyr::first(
        exposicao
      ),
    
    sinistros =
      sum(
        sinistros
      ),
    
    frequencia_mensal =
      sum(
        sinistros
      ) /
      sum(
        exposicao
      ),
    
    shocks =
      sum(
        y_bar
      ),
    
    share_spatial_medio_ativo =
      mean(
        share_spatial_true[
          spatial_lag_queen_true >
            0
        ],
        na.rm = TRUE
      ),
    
    .groups =
      "drop"
    
  )


print(
  sinistros_grid,
  n = Inf
)


# ============================================================
# 29. ORGANIZAR COLUNAS
# ============================================================

base_v4 <- base_v4 %>%
  
  dplyr::select(
    
    # --------------------------------------------------------
    # Identificacao
    # --------------------------------------------------------
    
    id_celula,
    linha,
    coluna,
    
    ano,
    mes,
    data_mes,
    
    
    # --------------------------------------------------------
    # Exposicao
    # --------------------------------------------------------
    
    exposicao,
    
    
    # --------------------------------------------------------
    # Composicao - Pisos
    # --------------------------------------------------------
    
    pct_pisos_1,
    pct_pisos_2,
    pct_pisos_3mais,
    
    
    # --------------------------------------------------------
    # Composicao - Qualidade
    # --------------------------------------------------------
    
    pct_qualidade_boa,
    pct_qualidade_media,
    pct_qualidade_baixa,
    
    
    # --------------------------------------------------------
    # Composicao - Idade
    # --------------------------------------------------------
    
    pct_idade_0_10,
    pct_idade_11_20,
    pct_idade_21_30,
    pct_idade_30mais,
    
    
    # --------------------------------------------------------
    # Clima original
    # --------------------------------------------------------
    
    prec_mensal_soma,
    prec_mensal_max,
    dias_acima_threshold,
    prec_mensal_soma_lag1,
    
    
    # --------------------------------------------------------
    # Clima padronizado
    # --------------------------------------------------------
    
    z_prec_mensal,
    z_prec_max,
    z_dias_extremos,
    z_prec_lag1,
    
    
    # --------------------------------------------------------
    # Carteira padronizada
    # --------------------------------------------------------
    
    z_pct_pisos_2,
    z_pct_pisos_3mais,
    
    z_pct_qualidade_media,
    z_pct_qualidade_baixa,
    
    z_pct_idade_11_20,
    z_pct_idade_21_30,
    z_pct_idade_30mais,
    
    
    # --------------------------------------------------------
    # Processo espacial
    # --------------------------------------------------------
    
    y_bar,
    
    spatial_lag_rook,
    
    spatial_lag_queen_true,
    
    
    # --------------------------------------------------------
    # Processo gerador
    # --------------------------------------------------------
    
    taxa_base_true,
    
    taxa_spatial_true,
    
    share_spatial_true,
    
    lambda_true,
    
    
    # --------------------------------------------------------
    # Resposta
    # --------------------------------------------------------
    
    sinistros,
    
    
    # --------------------------------------------------------
    # Informacoes do threshold
    # --------------------------------------------------------
    
    lambda_ref_shock,
    threshold_ybar
    
  ) %>%
  
  dplyr::arrange(
    
    data_mes,
    linha,
    coluna
    
  )


# ============================================================
# 30. SALVAR BASE V4
# ============================================================

readr::write_csv(
  
  base_v4,
  
  ARQUIVO_SAIDA
  
)


# ============================================================
# 31. SALVAR MATRIZES
# ============================================================

A_rook_csv <-
  
  as.data.frame(
    A_rook
  ) %>%
  
  tibble::rownames_to_column(
    "id_celula"
  )


A_queen_csv <-
  
  as.data.frame(
    A_queen
  ) %>%
  
  tibble::rownames_to_column(
    "id_celula"
  )


readr::write_csv(
  
  A_rook_csv,
  
  "matriz_adjacencia_rook_v4.csv"
  
)


readr::write_csv(
  
  A_queen_csv,
  
  "matriz_adjacencia_queen_v4.csv"
  
)


# ============================================================
# 32. SALVAR PARAMETROS VERDADEIROS
# ============================================================

parametros_beta_true_v4 <- tibble::tibble(
  
  parametro =
    names(
      BETA_TRUE
    ),
  
  valor_true =
    as.numeric(
      BETA_TRUE
    )
  
)


readr::write_csv(
  
  parametros_beta_true_v4,
  
  "parametros_beta_true_v4.csv"
  
)


parametros_psi_true_v4 <- tibble::tibble(
  
  id_celula =
    names(
      psi_true
    ),
  
  psi_true =
    as.numeric(
      psi_true
    )
  
)


readr::write_csv(
  
  parametros_psi_true_v4,
  
  "parametros_psi_true_v4.csv"
  
)


parametros_gerais_v4 <- tibble::tibble(
  
  parametro =
    c(
      
      "PHI_TRUE",
      
      "QUANTIL_SHOCK",
      
      "LAMBDA_SHOCK_REF",
      
      "TARGET_SPATIAL_SHARE",
      
      "S_REF",
      
      "PSI_BASE",
      
      "N_DOMICILIOS"
      
    ),
  
  valor =
    c(
      
      PHI_TRUE,
      
      QUANTIL_SHOCK,
      
      LAMBDA_SHOCK_REF,
      
      TARGET_SPATIAL_SHARE,
      
      S_REF,
      
      PSI_BASE,
      
      N_DOMICILIOS
      
    )
  
)


readr::write_csv(
  
  parametros_gerais_v4,
  
  "parametros_gerais_true_v4.csv"
  
)


# ============================================================
# 33. SALVAR RESUMO POR GRID
# ============================================================

readr::write_csv(
  
  sinistros_grid,
  
  "resumo_sinistros_grid_v4.csv"
  
)


# ============================================================
# 34. RESUMO FINAL
# ============================================================

cat(
  "\n=============================================\n"
)

cat(
  "BASE TRATADA V4 CRIADA\n"
)

cat(
  "=============================================\n"
)


cat(
  "\nArquivo principal:\n"
)

cat(
  "- base_tratada_v4.csv\n"
)


cat(
  "\nMatrizes:\n"
)

cat(
  "- matriz_adjacencia_rook_v4.csv\n"
)

cat(
  "- matriz_adjacencia_queen_v4.csv\n"
)


cat(
  "\nParametros verdadeiros:\n"
)

cat(
  "- parametros_beta_true_v4.csv\n"
)

cat(
  "- parametros_psi_true_v4.csv\n"
)

cat(
  "- parametros_gerais_true_v4.csv\n"
)


cat(
  "\nDiagnostico:\n"
)

cat(
  "- resumo_sinistros_grid_v4.csv\n"
)


cat(
  "\nDiretorio:\n",
  getwd(),
  "\n"
)

cat(
  "=============================================\n"
)