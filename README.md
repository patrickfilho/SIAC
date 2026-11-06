# SP-NB Queen: modelagem espaço-temporal de sinistros climáticos

Projeto de Iniciação Científica do LabMA/UFRJ, apresentado na SIAc 2026.

## Sobre o projeto

Eventos climáticos extremos, como chuvas intensas, tendem a atingir áreas vizinhas ao mesmo tempo. Com isso, os sinistros de regiões próximas deixam de ser independentes. Modelos usuais de contagem tratam cada região isoladamente, uma vez dadas as covariáveis, e ignoram essa co-ocorrência.

Este projeto compara três modelos para prever o número mensal de sinistros em uma malha de 25 grids (5×5) no entorno de Taquari (RS), de 2001 a 2025:

- **Binomial Negativa (NB):** benchmark estatístico de contagem;
- **Random Forest (RF):** benchmark de machine learning;
- **SP-NB Queen:** extensão da NB que adiciona um termo espacial aditivo, baseado no número de vizinhos (contiguidade Queen) com sinistros acima do esperado no mês anterior, seguindo Gudmundarson, Tzougas e Christopoulos (2026).

**Sobre os dados:** a precipitação é real (CHIRPS). Os sinistros e as características da carteira são simulados.

## Estrutura do repositório

```
├── [apresentacoes]/   # slides da apresentação em PDF
├── [scripts]/         # códigos de preparação dos dados, ajuste e avaliação dos modelos
└── [bases]/           # bases externas utilizadas
```

### Apresentações
Slides apresentados na SIAc 2026, com motivação, modelagem e resultados.

### Scripts
Códigos em R para montar o painel grid × mês, ajustar os três modelos (treino 2001–2020, teste 2021–2025) e calcular as métricas de comparação.

### Bases externas
- **CHIRPS** (Climate Hazards Group InfraRed Precipitation with Station data): precipitação mensal sobre a área de estudo.
- [outras bases, se houver]

## Autores

- João Victor Barboza Machado das Neves, LabMA/UFRJ
- Patrick Oliveira da Silva Filho, LabMA/UFRJ

Orientação: Viviana G. R. Lobo, Thais C. O. Fonseca e Mariane B. Alves.

## Referência principal

GUDMUNDARSON, R. L.; TZOUGAS, G.; CHRISTOPOULOS, D. Insurance ratemaking for climate-related claim counts using mixed Poisson spatio-temporal and temporal Besag regression models. *European Actuarial Journal*, 2026. DOI: [10.1007/s13385-025-00443-6](https://doi.org/10.1007/s13385-025-00443-6).
