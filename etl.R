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
cache_folder = "cache_folder_shiny"

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
  janitor::clean_names()

# BP -----------------------------------------------
bp <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Ativo Total",
                               "Passivo Total",
                               "Patrimônio Líquido Consolidado",
                               "Ativo Circulante",
                               "Ativo Não Circulante",
                               "Passivo Circulante",
                               "Passivo Não Circulante")
         & cd_conta %in% c("1", "2", "1.01", "1.02", "2.01", "2.02", "2.03")) |>
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(ds_conta),
              -c(label, cd_conta, escala_moeda)) |> 
  janitor::clean_names() |> 
  dplyr::mutate(passivo = passivo_total - patrimonio_liquido_consolidado)


# DRE -----------------------------------------------
dre <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Receita de Venda de Bens e/ou Servi\u00e7os",
                               "Lucro/Preju\u00edzo Consolidado do Per\u00edodo",
                               "Custo dos Bens e/ou Servi\u00e7os Vendidos")
         & cd_conta %in% c("3.11", "3.01", "3.02")) |>
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(ds_conta),
              -c(label, cd_conta, escala_moeda)) |> 
  janitor::clean_names()

# DFC ------------------------------------------------
dfc <- df_dfp |> 
  dplyr::filter(ds_conta %in% c("Caixa Líquido Atividades Operacionais",
                               "Caixa Líquido Atividades de Investimento",
                               "Caixa Líquido Atividades de Financiamento",
                               "Aumento (Redução) de Caixa e Equivalentes",
                               "Saldo Inicial de Caixa e Equivalentes",
                               "Saldo Final de Caixa e Equivalentes"
                         )
  & cd_conta %in% c("6.01", "6.02", "6.03", "6.04", "6.05", "6.05.01", "6.05.02")) |>
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(ds_conta),
              -c(label, cd_conta, escala_moeda)
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
                                 "Despesas Antecipadas"
                         ) & cd_conta %in%
                        c("1.01.01", # Caixa
                          "2.01",    # Passivo Circulante
                          "1.01",    # Ativo Circulante
                          "1.01.04", # Estoques
                          "1.01.07", # Despesas Antecipadas
                          "1.02",    # Ativo não circulante
                          "2.02"    # Passivo não circulante 
                         ) 
         ) |> 
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(ds_conta),
              -c(label, cd_conta)) |>
  janitor::clean_names() |> 
  dplyr::group_by(denom_cia, dt_refer) |>  
  dplyr::transmute(liquidez_imediata = `caixa_e_equivalentes_de_caixa` / `passivo_circulante`, 
                   liquidez_seca = (`ativo_circulante` -  `estoques` - `despesas_antecipadas`) / `passivo_circulante`,
                   liquidez_corrente = `ativo_circulante` / `passivo_circulante`,
                   liquidez_geral = (`ativo_circulante` + `ativo_nao_circulante`) / (`passivo_circulante` + `passivo_nao_circulante`)
                   ) |> 
  dplyr::ungroup() |> 
  dplyr::arrange(denom_cia)

## Indicadores de Endividamento -------------------------------------
indic_end <- 
  df_dfp |> 
  dplyr::filter(cd_conta %in% c("1", # Ativo Total
                                "2.01", # Passivo Circulante
                                "2.02", # Passivo não circulante
                                "2.01.04", # Empréstimos e Financiamento de CP
                                "2.02.01", # Empréstimos e Financiamento de LP
                                "2.03", # Patrimônio Líquido
                                "3.05" # EBIT
  )
  ) |> 
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(cd_conta),
              -c(label, ds_conta, escala_moeda)) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(divida_pl = (`2.01.04` + `2.02.01`)/ `2.03`,
                   divida_ativos = (`2.01.04` + `2.02.01`) / `1`,
                   divida_ebit = (`2.01.04` + `2.02.01`) / `3.05`,
                   pl_ativos = `2.03` / `1`,
                   passivos_ativos =   (`2.01` + `2.02`) / `1`) |> 
  dplyr::ungroup()

## Indicadores de Eficiência --------------------------------------
indic_enf <- 
  df_dfp |>
  dplyr::filter(ds_conta %in% c("Receita de Venda de Bens e/ou Serviços",
                                "Resultado Bruto",
                                "Resultado Antes do Resultado Financeiro e dos Tributos",
                                "Resultado antes dos Tributos sobre o Lucro",
                                "Lucro ou Prejuízo Líquido Consolidado do Período",
                                "Lucro/Prejuízo Consolidado do Período"
                                ) &
    cd_conta %in% c("3.01", # Receita de Venda de Bens e/ou Serviços
                    "3.03", # Resultado Bruto
                    "3.05", # EBIT
                    "3.11"  # Lucro/Prejuízo Consolidado do Período
  )
  ) |> 
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(cd_conta),
                -c(label, ds_conta, escala_moeda)) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(margem_bruta = `3.03` / `3.01`,
                   margem_liquida = `3.11` / `3.01`,
                   margem_ebit =  `3.05` / `3.01`
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
                cd_conta %in% c("1", # Ativo Total 
                                "2", # Passivo total
                                "2.03", # Patrimônio Líquido
                                "3.05", # EBIT
                                "3.08", # Imposto de Renda e Contribuição Social sobre o Lucro
                                "3.11"  # Lucro/Prejuízo Consolidado do Período
                )
  )|> 
  tidyr::pivot_wider(values_from = vl_conta,
              names_from = c(cd_conta),
              -c(label, ds_conta, escala_moeda)) |> 
  dplyr::group_by(denom_cia, dt_refer) |> 
  dplyr::transmute(
                roic = (`3.05` - `3.08`) / (`2`),
                roe = (`3.11`) / (`2.03`),
                roa = (`3.11`) / (`1`)
                )|> 
  dplyr::ungroup()

# Salva os dados em um arquivo .Rdata
save(
  list  = ls(),
  file  = file.path("dados.Rdata"),
  envir = environment()
)
