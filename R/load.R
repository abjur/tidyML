#' Carrega os dados brutos do Tribunal para iniciar limpeza
#'
#' Carrega os dados brutos do Tribunal para iniciar limpeza. Basicamente faz a
#' seleção das colunas de interesse para essa fase da pesquisa e arruma os
#' formatos para ficar tudo igual.
#'
#' @param path data.frame lido diretamente dos path passados pelo
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
load_tj <- function(path, tj = 'tjsp') {
  tj <- tolower(tj)
  fun_str <- paste0('load_', tj)
  eval(parse(text = fun_str))(path)
}

#' @rdname load_tj
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_tjrs <- function(path) {
  path %>%
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
load_tjsp <- function(path) {
  path %>%
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
load_tjam <- function(path) {
  ## leitura do master para checks
  # master_file_tjam <- dir(paths$tjam, full.names = TRUE, pattern = 'master.rds')
  # master_tjam <- readRDS(master_file_tjam)
  ## lista de arquivos
  all_files <- dir(path, full.names = TRUE,
                   pattern = '[0-9].rds')
  ## leitura inicial
  d_tjam <- all_files %>%
    map_df(readRDS) %>%
    mutate(html = basename(arq)) %>%
    filter(arq != 'erro')
  ## filtros iniciais
  tjam_infos <- d_tjam %>%
    select(html, infos) %>%
    unnest(infos) %>%
    filter(is.na(erro)) %>%
    select(-erro) %>%
    filter(str_length(key) < 50) %>%
    group_by(html, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    spread(key, value) %>%
    filter(!is.na(area), area == 'Cível') %>%
    filter(!is.na(assunto)) %>%
    filter(is.na(processo_principal))
  ## movimentacoes
  tjam_first_mov <- d_tjam %>%
    select(html, movs) %>%
    semi_join(tjam_infos, 'html') %>%
    unnest(movs) %>%
    mutate(data_mov = lubridate::dmy(data_mov)) %>%
    arrange(data_mov) %>%
    filter(str_detect(titulo, regex('distr', ignore_case = TRUE))) %>%
    group_by(html) %>%
    slice(1) %>%
    ungroup() %>%
    select(html, dt_dist = data_mov)
  ## partes
  tjam_partes <- d_tjam %>%
    select(html, partes) %>%
    semi_join(tjam_infos, 'html') %>%
    unnest(partes) %>%
    select(-adv) %>%
    separate(parte, c('parte', 'adv'), sep = '[\t\n\f\r]',
             extra = 'merge', fill = 'right') %>%
    mutate_at(vars(parte, adv), funs(str_trim)) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., ' *[\n\t\f\r]+ *', '@'))) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., '@+', '@'))) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., ':[[:space:]]?@', '|'))) %>%
    mutate(adv = str_split(adv, '@') %>%
             map(~str_split_fixed(.x, fixed('|'), 2) %>%
                   as_tibble() %>%
                   setNames(c('forma_adv', 'adv')))) %>%
    filter(str_detect(forma, 'req|recl|exe')) %>%
    mutate(polo = case_when(
      str_detect(forma, '[ae]nte$') ~ 'autor',
      str_detect(forma, '[ai]d[ao]$') ~ 'reu'
    )) %>%
    select(html, polo, parte) %>%
    # suposição: primeiro nome é o nome principal
    group_by(html, polo) %>%
    mutate(parte_all = paste(parte, collapse = '\n')) %>%
    # basta mudar aqui caso queira considerar parte_all no lugar da primeira parte.
    summarise(parte = first(parte)) %>%
    # summarise(parte = first(parte_all)) %>%
    ungroup() %>%
    spread(polo, parte)
  # juntando tudo
  loc <- locale(decimal_mark = ',', grouping_mark = '.')
  tjam_final <- tjam_infos %>%
    mutate(n_processo = str_replace_all(html, '[^0-9]', ''),
           tj = 'TJAM',
           comarca = if_else(str_detect(lugar, 'apital'), 'Manaus', 'Outro'),
           foro = str_match(lugar, '(Fórum |Foro )(.*)$')[, 3],
           valor = parse_number(valor_da_acao, locale = loc)) %>%
    inner_join(tjam_first_mov, 'html') %>%
    inner_join(tjam_partes, 'html') %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca,
           foro, autor, reu, valor)
  tjam_final
}

#' @rdname load_tj
#' @import dplyr
#' @import stringr
#' @import lubridate
#' @export
load_tjba <- function(path) {
  all_files <- dir(paths$tjba, full.names = TRUE,
                   pattern = '[0-9].rds')
  ## leitura inicial
  d_tjba <- all_files %>%
    map_df(readRDS) %>%
    mutate(html = basename(arq)) %>%
    filter(arq != 'erro')
  ## filtros iniciais
  tjba_infos <- d_tjba %>%
    select(html, infos) %>%
    unnest(infos) %>%
    filter(is.na(erro)) %>%
    select(-erro) %>%
    filter(str_length(key) < 50) %>%
    group_by(html, key) %>%
    summarise(value = paste(value, collapse = '\n')) %>%
    ungroup() %>%
    spread(key, value) %>%
    filter(!is.na(area), area == 'Cível') %>%
    filter(!is.na(assunto)) %>%
    filter(is.na(processo_principal))
  ## movimentacoes
  tjba_first_mov <- d_tjba %>%
    select(html, movs) %>%
    semi_join(tjba_infos, 'html') %>%
    unnest(movs) %>%
    mutate(data_mov = lubridate::dmy(data_mov)) %>%
    arrange(data_mov) %>%
    filter(str_detect(titulo, regex('distr', ignore_case = TRUE))) %>%
    group_by(html) %>%
    slice(1) %>%
    ungroup() %>%
    select(html, dt_dist = data_mov)
  ## partes
  tjba_partes <- d_tjba %>%
    select(html, partes) %>%
    semi_join(tjba_infos, 'html') %>%
    unnest(partes) %>%
    select(-adv) %>%
    separate(parte, c('parte', 'adv'), sep = '[\t\n\f\r]',
             extra = 'merge', fill = 'right') %>%
    mutate_at(vars(parte, adv), funs(str_trim)) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., ' *[\n\t\f\r]+ *', '@'))) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., '@+', '@'))) %>%
    mutate_at(vars(parte, adv), funs(str_replace_all(., ':[[:space:]]?@', '|'))) %>%
    mutate(adv = str_split(adv, '@') %>%
             map(~str_split_fixed(.x, fixed('|'), 2) %>%
                   as_tibble() %>%
                   setNames(c('forma_adv', 'adv')))) %>%
    filter(str_detect(str_trim(forma), 'req|recl|exe|reu|autor|^re$|^impe')) %>%
    mutate(polo = case_when(
      str_detect(forma, '[ae]nte$|autor|qte$') ~ 'autor',
      str_detect(forma, '[ai]d[ao]$|^reu?$|cd[oa]$') ~ 'reu'
    )) %>%
    select(html, polo, parte) %>%
    # suposição: primeiro nome é o nome principal
    group_by(html, polo) %>%
    mutate(parte_all = paste(parte, collapse = '\n')) %>%
    # basta mudar aqui caso queira considerar parte_all no lugar da primeira parte.
    summarise(parte = first(parte)) %>%
    # summarise(parte = first(parte_all)) %>%
    ungroup() %>%
    spread(polo, parte)
  # juntando tudo
  loc <- locale(decimal_mark = ',', grouping_mark = '.')
  tjba_final <- tjba_infos %>%
    mutate(n_processo = str_replace_all(html, '[^0-9]', ''),
           tj = 'TJBA',
           comarca = if_else(str_detect(lugar, 'apital'), 'Manaus', 'Outro'),
           foro = str_trim(str_match(lugar, ' -([^-]+)$')[, 2]),
           valor = parse_number(valor_da_acao, locale = loc)) %>%
    inner_join(tjba_first_mov, 'html') %>%
    inner_join(tjba_partes, 'html') %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca,
           foro, autor, reu, valor)
}

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
    dplyr::mutate(data_finalizacao = lubridate::dmy(data_finalizacao),
                  tempo_reposta = as.numeric(tempo_resposta),
                  procurou_empresa = ifelse(procurou_empresa == "S",T,F),
                  respondida = ifelse(respondida == "S",T,F))
}
