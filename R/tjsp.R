tidy_tjsp <- function(d_tjsp) {
  anos_validos <- 2000:2015
  d_tjsp %>%
    mutate(dt_dist = dmy_hms(data_ultima_distribuicao),
           nproc_len = str_length(numero_processo),
           ncnj = nproc_len == 25,
           ano_cnj = if_else(ncnj,
                             as.integer(str_sub(numero_processo, 12, 15)),
                             NA_integer_)) %>%
    arrange(desc(dt_dist)) %>%
    distinct(numero_processo, .keep_all = TRUE) %>%
    filter(year(dt_dist) >= min(anos_validos),
           year(dt_dist) <= max(anos_validos)) %>%
    mutate(n_processo = stringr::str_replace_all(numero_processo,
                                                 '[^0-9]', '')) %>%
    mutate(tj = "TJSP") %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca = nme_comarca,
           foro = nme_foro, autor, reu, vl_causa = vlr_causa)
}
