# -----------------------------------------------------------------
# Script de Coleta e tratamento dos dados demonstrativos financeiros


library(GetDFPData2)
library(dplyr)
library(lubridate)
library(janitor)
library(purrr)
library(tidyr)
library(rlang)

# Define o diretório dos arquivos baixados
cache_folder = "cache_folder_shiny2"

# Código CVM das empresas ativas
info_companies <- GetDFPData2::get_info_companies(cache_folder = cache_folder)

# Filtra as empresas ativas, listadas na bolsa e retira bancos e seguradoras
names_companies <- info_companies |> 
  dplyr::filter(SIT_REG == 'ATIVO' 
                & TP_MERC == "BOLSA" 
                & !SETOR_ATIV %in% c("Bancos", 
                                     "Intermedia\u00e7\u00e3o Financeira",
                                     "Seguradoras e Corretoras")) |> 
  dplyr::arrange(DENOM_SOCIAL) |> 
  dplyr::pull(DENOM_SOCIAL)

# Cria lista de empresas
cvm_codes <- info_companies |> 
  dplyr::filter(DENOM_SOCIAL %in% names_companies) |> 
  dplyr::pull(CD_CVM)

# Define as informações da função get_dfp_data
first_date = 2011
last_date = lubridate::year(Sys.Date()) # Ano atual da coleta
type_docs = c("BPA", "BPP", "DFC_MI", "DRE")
type_format = "con"

# Coleta os dados das empresas
l_dfp <- GetDFPData2::get_dfp_data(companies_cvm_codes = cvm_codes, 
                                   first_year =  first_date,
                                   last_year = last_date, 
                                   type_docs = type_docs,
                                   type_format = type_format,
                                   use_memoise = TRUE,
                                   cache_folder = cache_folder)

# Realiza a limpeza da lista e empilha em um data frame
df_dfp <- l_dfp |>
  rlang::set_names(type_docs) |> 
  dplyr::bind_rows(.id = "label") |>
  dplyr::select(label, DT_REFER, DENOM_CIA, ESCALA_MOEDA, CD_CONTA, DS_CONTA, VL_CONTA) |> 
  janitor::clean_names() |>



df_dfp <- df_dfp |>
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(cd_conta),
    id_cols = c(label, denom_cia, dt_refer, escala_moeda, ds_conta),
    values_fn = list(vl_conta = mean)
  )

  # Transformar colunas numéricas corretamente
df_dfp <- df_dfp |> 
  dplyr::mutate(across(where(is.list), ~ as.numeric(unlist(.)), .names = "num_{.col}"))

  print(names(df_dfp))

# BP -----------------------------------------------
bp <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Ativo Total",
                                "Passivo Total",
                                "Patrimônio Líquido Consolidado",
                                "Ativo Circulante",
                                "Ativo Não Circulante",
                                "Passivo Circulante",
                                "Passivo Não Circulante") &
                cd_conta %in% c("1", "2", "1.01", "1.02", "2.01", "2.02", "2.03")) |>
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(ds_conta),
    id_cols = c(label, dt_refer, denom_cia, escala_moeda, cd_conta),
    values_fn = list(vl_conta = mean) # Usa a média para consolidar valores duplicados
  ) |> 
  janitor::clean_names()




# DRE -----------------------------------------------
dre <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Receita de Venda de Bens e/ou Servi\u00e7os",
                               "Lucro/Preju\u00edzo Consolidado do Per\u00edodo",
                               "Custo dos Bens e/ou Servi\u00e7os Vendidos")
         & cd_conta %in% c("3.11", "3.01", "3.02")) |>
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(ds_conta),
              id_cols = c(label, denom_cia, dt_refer, escala_moeda)) |> 
  janitor::clean_names()

# DFC ------------------------------------------------
dfc <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Caixa Líquido Atividades Operacionais",
                                "Caixa Líquido Atividades de Investimento",
                                "Caixa Líquido Atividades de Financiamento",
                                "Aumento (Redução) de Caixa e Equivalentes",
                                "Saldo Inicial de Caixa e Equivalentes",
                                "Saldo Final de Caixa e Equivalentes") &
                cd_conta %in% c("6.01", "6.02", "6.03", "6.04", "6.05", "6.05.01", "6.05.02")) |> 
  tidyr::pivot_wider(
    names_from = ds_conta,         # Define os nomes das colunas
    values_from = vl_conta,        # Define os valores das colunas
    id_cols = c(label, denom_cia, dt_refer, escala_moeda),
    values_fn = list(vl_conta = mean) # Consolida valores duplicados
  ) |> 
  janitor::clean_names()



# Cria os indicadores --------------------------------

## Indicadores de Liquidez ---------------------------
indic_liq <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Ativo Circulante",
                                "Ativo Não Circulante",
                                "Passivo Circulante",
                                "Passivo Não Circulante",
                                "Caixa e Equivalentes de Caixa",
                                "Estoques",
                                "Despesas Antecipadas") & 
                cd_conta %in% c("1.01.01", "2.01", "1.01", "1.01.04", "1.01.07", "1.02", "2.02")) |>
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(ds_conta),
    id_cols = c(label, denom_cia, dt_refer, escala_moeda),
    values_fn = list(vl_conta = mean)
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(across(
    c(caixa_e_equivalentes_de_caixa, passivo_circulante, ativo_circulante, estoques, 
      despesas_antecipadas, ativo_nao_circulante, passivo_nao_circulante), 
    ~ as.numeric(.), 
    .names = "num_{.col}"
  )) |> 
  dplyr::mutate(
    liquidez_imediata = num_caixa_e_equivalentes_de_caixa / num_passivo_circulante,
    liquidez_seca = (num_ativo_circulante - num_estoques - num_despesas_antecipadas) / num_passivo_circulante,
    liquidez_corrente = num_ativo_circulante / num_passivo_circulante,
    liquidez_geral = (num_ativo_circulante + num_ativo_nao_circulante) / (num_passivo_circulante + num_passivo_nao_circulante)
  ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(denom_cia)


## Indicadores de Endividamento -------------------------------------
indic_end <- df_dfp |> 
  dplyr::filter(cd_conta %in% c("1", "2.01", "2.02", "2.01.04", "2.02.01", "2.03", "3.05")) |> 
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(cd_conta),
    id_cols = c(label, denom_cia, dt_refer, escala_moeda),
    values_fn = list(vl_conta = mean) # Consolida valores duplicados
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(across(
    starts_with("x"), 
    ~ as.numeric(.), 
    .names = "num_{.col}" # Converte para numérico
  )) |> 
  dplyr::mutate(
    across(
      starts_with("num_"),
      ~ ifelse(is.na(.), 0, .) # Substitui NA por 0
    )
  ) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(
    divida_pl = (num_x2_01_04 + num_x2_02_01) / num_x2_03,
    divida_ativos = (num_x2_01_04 + num_x2_02_01) / num_x1,
    divida_ebit = (num_x2_01_04 + num_x2_02_01) / num_x3_05,
    pl_ativos = num_x2_03 / num_x1,
    passivos_ativos = (num_x2_01 + num_x2_02) / num_x1
  ) |> 
  dplyr::ungroup()


## Indicadores de Eficiência --------------------------------------
indic_enf <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Receita de Venda de Bens e/ou Serviços",
                                "Resultado Bruto",
                                "Resultado Antes do Resultado Financeiro e dos Tributos",
                                "Resultado antes dos Tributos sobre o Lucro",
                                "Lucro ou Prejuízo Líquido Consolidado do Período",
                                "Lucro/Prejuízo Consolidado do Período") &
                cd_conta %in% c("3.01", "3.03", "3.05", "3.11")) |> 
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(cd_conta),
    id_cols = c(label, denom_cia, dt_refer, escala_moeda),
    values_fn = list(vl_conta = mean) # Consolida duplicatas
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(across(
    starts_with("x"), 
    ~ as.numeric(.), # Converte para numérico
    .names = "num_{.col}"
  )) |> 
  dplyr::mutate(
    across(
      starts_with("num_"),
      ~ ifelse(is.na(.), 0, .) # Substitui NA por 0
    )
  ) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(
    margem_bruta = num_x3_03 / num_x3_01,
    margem_liquida = num_x3_11 / num_x3_01,
    margem_ebit = num_x3_05 / num_x3_01
  ) |> 
  dplyr::ungroup()



## Indicadores de Rentabilidade --------------------------------------
indic_rent <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Ativo Total",
                                "Passivo Total",
                                "Resultado Antes do Resultado Financeiro e dos Tributos",
                                "Resultado antes dos Tributos sobre o Lucro",
                                "Imposto de Renda e Contribuição Social sobre o Lucro",
                                "Lucro ou Prejuízo Líquido Consolidado do Período",
                                "Lucro/Prejuízo Consolidado do Período",
                                "Patrimônio Líquido Consolidado") &
                cd_conta %in% c("1", "2", "2.03", "3.05", "3.08", "3.11")) |> 
  tidyr::pivot_wider(
    values_from = vl_conta,
    names_from = c(cd_conta),
    id_cols = c(label, denom_cia, dt_refer, escala_moeda),
    values_fn = list(vl_conta = mean) # Consolida valores duplicados
  ) |> 
  janitor::clean_names() |> 
  dplyr::mutate(across(
    starts_with("x"), 
    ~ as.numeric(.), # Converte para numérico
    .names = "num_{.col}"
  )) |> 
  dplyr::mutate(
    across(
      starts_with("num_"),
      ~ ifelse(is.na(.), 0, .) # Substitui NA por 0
    )
  ) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(
    roic = (num_x3_05 - num_x3_08) / num_x2,   # (EBIT - Impostos) / Passivo Total
    roe = num_x3_11 / num_x2_03,               # Lucro Líquido / Patrimônio Líquido
    roa = num_x3_11 / num_x1                   # Lucro Líquido / Ativo Total
  ) |> 
  dplyr::ungroup()



# Salva os dados em um arquivo .Rdata
# Use para remover objetos pesados do enviroment e não utilizado no Dashboard
rm(l_dfp)
rm(df_dfp)
save(
  list = ls(),
  file = file.path("dados.Rdata"),
  envir = environment()
)
