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
