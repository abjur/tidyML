#' Carrega os dados brutos do Tribunal para iniciar limpeza
#'
#' Carrega os dados brutos do Tribunal para iniciar limpeza. Basicamente faz a
#' seleção das colunas de interesse para essa fase da pesquisa e arruma os
#' formatos para ficar tudo igual.
#'
#' @param raw_data data.frame lido diretamente dos raw_data passados pelo
#'   Tribunal.
#' @param tj nome do Tribunal. Pode ser \code{"tjrs"}, \code{"tjsp"},
#'   \code{"tjms"} ou \code{"tjba"}.
#'
#' @return tibble com as colunas \itemize{ \item{"n_processo"}{Número do
#'   processo} \item{"dt_dist"}{Data de distribuição} \item{"classe"}{Classe}
#'   \item{"assunto"}{Assunto} \item{"comarca"}{Comarca} \item{"foro"}{Foro}
#'   \item{"autor"}{Autor} \item{"reu"}{Réu} \item{"vl_causa"}{Valor da causa} }
#'
#' @export
load_tj <- function(raw_data, tj = 'tjsp') {
  tj <- tolower(tj)
  fun_str <- paste0('load_', tj)
  eval(parse(text = fun_str))(raw_data)
}

#' @rdname load_tj
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_tjrs <- function(raw_data) {
  raw_data %>%
    mutate(dt_dist = dmy_hms(data_distribuicao),
           nproc_len = str_length(n_processo),
           ncnj = nproc_len == 20,
           ano_cnj = if_else(ncnj,
                             as.integer(str_sub(n_processo, 10, 13)),
                             NA_integer_)) %>%
    arrange(desc(dt_dist)) %>%
    # distinct(numero_processo, .keep_all = TRUE) %>%
    # filter(year(dt_dist) >= min(anos_validos),
    #        year(dt_dist) <= max(anos_validos)) %>%
    mutate(tj = "TJRS") %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca,
           foro, autor, reu, vl_causa = valor_causa)
}

#' @rdname load_tj
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_tjsp <- function(raw_data) {
  raw_data %>%
    mutate(dt_dist = dmy_hms(data_ultima_distribuicao),
           nproc_len = str_length(numero_processo),
           ncnj = nproc_len == 25,
           ano_cnj = if_else(ncnj,
                             as.integer(str_sub(numero_processo, 12, 15)),
                             NA_integer_)) %>%
    arrange(desc(dt_dist)) %>%
    # distinct(numero_processo, .keep_all = TRUE) %>%
    mutate(n_processo = str_replace_all(numero_processo, '[^0-9]', '')) %>%
    mutate(tj = "TJSP") %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca = nme_comarca,
           foro = nme_foro, autor, reu, vl_causa = vlr_causa)
}

#' @rdname load_tj
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_tjmt <- function(assuntos, processos, partes) {
  assuntos <- assuntos %>%
    janitor::clean_names() %>%
    dplyr::select(selo_seq_cabecalho, cod_assunto, assunto_nome, assunto_situacao)

  processos <- processos %>%
    janitor::clean_names() %>%
    set_names(abjutils::rm_accent(names(.))) %>%
    select(selo_seq_cabecalho, sigla, selo_processo_numero_unico, selo_data_de_ajuizamento,
           selo_codigo_localidade, selo_codigo_orgao_julgador, selo_nome_orgao_julgador, selo_instancia_orgao_julgador,
           selo_orgao_julgador_codigo_ibge, cod_classe, classe_nome)

  base_consolidada <- partes %>%
    janitor::clean_names() %>%
    dplyr::inner_join(assuntos, by = 'selo_seq_cabecalho') %>%
    dplyr::inner_join(processos, by = 'selo_seq_cabecalho')

  return(base_consolidada)
}


load_tjba <- function(raw_data) {

}

#' @rdname load_senacon
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_senacon <- function(raw_data){
  raw_data %>%
    purrr::set_names(unlist(.[1,])) %>%
    dplyr::as_data_frame() %>%
    dplyr::filter(Sexo %in% c("F","M")) %>%
    janitor::clean_names() %>%
    purrr::set_names(abjutils::rm_accent(names(.))) %>%
    dplyr::mutate(data_finalizacao = dmy(data_finalizacao),
                  tempo_reposta = as.numeric(tempo_reposta),
                  procurou_empresa = ifelse(procurou_empresa == "S",T,F),
                  respondida = ifelse(respondida == "S",T,F))
}
