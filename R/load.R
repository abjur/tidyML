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
load_tjms <- function(raw_data) {

}

load_tjba <- function(raw_data) {

}
