#' Downloads consumerist issues's TPU
#'
#' @param regex_consumerista string or regex containing the content to match with the legals subjects in the TPU.
#'
#' @export
build_tpu_consumerista <- function(regex_consumerista = " ([Ll]ei )?8\\.?078((/90)| |$)|CDC|[Cc]ódigo (de )?[Dd]efesa (do )?[Cc]onsumidor| ([Ll]ei )?7\\.?347((/85)| |$)"){

  tpur::download_table("assunto","estadual","primeiro grau") %>%
    tpur:::build_table_all_nodes() %>%
    dplyr::filter(
      stringr::str_detect(dispositivo_legal, regex_consumerista)|
        #or
      stringr::str_detect(n1, "DIREITO DO CONSUMIDOR"))

}

#' Filter only consumer lawsuits
#'
#' @param loaded_tjmt data_frame obtained from load_tjmt
#'
#' @export
filter_tjmt <- function(loaded_tjmt){

  assuntos_consumeristas_estatiticos <- assuntos::class_automatica_abj %>%
    dplyr::filter(leaf != "")
  control_table <- tpur::control_table
  assuntos_consumeristas_tpu <- build_tpu_consumerista() %>%
    dplyr::filter(codigo != "")

  codigos_consumeristas <- unique(c(assuntos_consumeristas_estatiticos$leaf,
                                    assuntos_consumeristas_tpu$codigo))

  processos <- loaded_tjmt %>%
    group_by(selo_seq_cabecalho) %>%
    summarise(tem_juridica_no_polo_passivo = any(selo_pessoa_polo == "PA" &
                                                   selo_pessoa_tipo == "JURIDICA")) %>%
    filter(tem_juridica_no_polo_passivo) %>%
    with(selo_seq_cabecalho)

  base_filtrada <- loaded_tjmt %>%
    filter(cod_assunto %in% codigos_consumeristas, selo_seq_cabecalho %in% processos)
}

#' Filter only consumer lawsuits
#'
#' @param loaded_tjdft data_frame obtained from load_tjmt
#'
#' @export
filter_tjdft <- function(loaded_tjdft){

  assuntos_consumeristas_estatiticos <- assuntos::class_automatica_abj %>%
    dplyr::filter(leaf != "")
  control_table <- tpur::control_table
  assuntos_consumeristas_tpu <- build_tpu_consumerista() %>%
    dplyr::filter(codigo != "")

  codigos_consumeristas <- unique(c(assuntos_consumeristas_estatiticos$leaf,
                                    assuntos_consumeristas_tpu$codigo))

  processos <- loaded_tjdft %>%
    group_by(selo_seq_cabecalho) %>%
    summarise(tem_juridica_no_polo_passivo = any(selo_pessoa_polo == "PA" &
                                                   selo_pessoa_tipo == "JURIDICA")) %>%
    filter(tem_juridica_no_polo_passivo) %>%
    with(selo_seq_cabecalho)

  base_filtrada <- loaded_tjdft %>%
    filter(cod_assunto %in% codigos_consumeristas, selo_seq_cabecalho %in% processos)
}

#' Filter only consumer lawsuits
#'
#' @param loaded_tjrs data_frame obtained from load_tjmt
#'
#' @export
filter_tjrs <- function(loaded_tjrs){

  d_tjrs <- loaded_tjrs %>%
    mutate(nome2 = tidyML::fix_nomes(reu) %>% tidyML::aggregate_major_companies())

  regex_empresa <- tidyML:::vec2regex("@s@a@","@l@t@d@a@","estad","municip","secretari","@m@e@",
                                      "prefeit","companhia","cidade","associacao","cooperativa",
                                      "regex","eireli","empresa","cia","banco","confederacao","servico",
                                      "advogados","sindicato","condominio","sociedade","camara","ministerio",
                                      "fazenda","hospital","universidade","associacao","edificio","cooperativa",
                                      "instituto","fundacao")

  d_tjrs <- d_tjrs %>%
    tidyr::separate(assunto, into = c('assunto_2', 'assunto_pai'), remove = F, sep = "::") %>%
    mutate(assunto_final = ifelse(str_detect(assunto, "::"), assunto_2, assunto),
           assunto_final = stringr::str_trim(assunto_final),
           tem_pessoa = stringr::str_detect(nome2, regex_empresa)) %>%
    filter((assunto_final %in% stringr::str_trim(assuntos_consumeristas)), tem_pessoa)


  #especifico tjrs
  {especifico_tjrs <- c("Abatimento proporcional do preço",
                              "Cadastro de Análise de Crédito",
                              "Cobrança indevida de ligações ",
                              "Cobrança Indevida de Serviços",
                              "Comercialização de dados cadastrais de consumidores",
                              "Contratos de Participacao Financeira",
                              "Medicamento / Tratamento / Cirurgia de Eficácia não comprovada",
                              "Metodologia de Reajuste de Tarifa - IRT",
                              "Notificação",
                              "Registro em Cadastro de Análise de Crédito",
                              "Registro em Cadastro",
                              "Registro em Cadastro de Inadimplentes",
                              "Registro para comercialização de dados cadastrais de consumidores",
                              "Reparação de Danos")}

  assuntos_consumeristas_estatiticos <- assuntos::class_automatica_abj %>%
    dplyr::filter(leaf != "")
  control_table <- tpur::control_table
  assuntos_consumeristas_tpu <- build_tpu_consumerista() %>%
    dplyr::filter(codigo != "")

  codigos_consumeristas <- unique(c(assuntos_consumeristas_estatiticos$leaf,
                                    assuntos_consumeristas_tpu$codigo))

  codigos_consumeristas_puros <- assuntos_consumeristas_tpu$codigo

  tpu <- tpur::download_table("assunto","estadual","1 grau") %>%
    tpur::build_table() %>%
    dplyr::filter(codigo != "") %>%
    dplyr::select(dplyr::contains("n"), codigo, cod_pai)

  l_tpu <- tpu %>%
    tidyr::gather(key, val, -codigo, -cod_pai) %>%
    dplyr::mutate(key = as.numeric(stringr::str_replace(key,"n",""))) %>%
    dplyr::group_by(codigo, cod_pai) %>%
    dplyr::summarise(val_node = val[max(key[val != ''])],
              val_pai = val[sort(key[val != ''])[sum(val != '')-1]]) %>%
    with(dplyr::data_frame(codigo = c(codigo, cod_pai),
                    assunto = c(val_node, val_pai))) %>%
    dplyr::distinct(codigo, assunto)

  assuntos_consumeristas <- l_tpu %>%
    dplyr::filter(codigo %in% codigos_consumeristas) %>%
    with(assunto) %>%
    c(especifico_tjrs) %>%
    unique

  assuntos_consumeristas_puros <- l_tpu %>%
    dplyr::filter(codigo %in% codigos_consumeristas_puros) %>%
    with(assunto) %>%
    c(especifico_tjrs) %>%
    unique

  base_filtrada <- d_tjrs %>%
    filter(assunto %in% assuntos_consumeristas, tipo_pessoa)
}

