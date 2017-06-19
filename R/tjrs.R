
tidy_tjrs <- function(d_tjrs) {
  anos_validos <- 2000:2016
  d_tjrs %>%
    mutate(dt_dist = dmy_hms(data_distribuicao),
           nproc_len = str_length(n_processo),
           ncnj = nproc_len == 20,
           ano_cnj = if_else(ncnj,
                             as.integer(str_sub(n_processo, 10, 13)),
                             NA_integer_)) %>%
    arrange(desc(dt_dist)) %>%
    # distinct(numero_processo, .keep_all = TRUE) %>%
    filter(year(dt_dist) >= 2009, year(dt_dist) <= 2015) %>%
    mutate(tj = "TJRS") %>%
    select(tj, n_processo, dt_dist, classe, assunto, comarca,
           foro, autor, reu, vl_causa = valor_causa)
}
