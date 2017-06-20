#as funções dessa parte do pacote são responsáveis por criar as colunas que serão efetivamente analisadas no relatório

arruma_sa <- function(string_vec){

  regex_sa_geral <- stringr::regex("([^d][^e]) s *[\\./]?a *\\.?( [(].+[)])?$")

  string_vec_phase1 <- stringr::str_replace(string_vec, regex_sa_geral,"\\1 @s@a@\\2")

  regex_sa_particular <- regex("s/ *a|s\\. *a\\.?|( )sa$|( )+s ?a($| )")

  string_vec_phase2 <- stringr::str_replace(string_vec_phase1, regex_sa_particular," @s@a@\\2")

  regex_estacio_de_sa <- regex('estacio de sa')

  string_vec_phase3 <- stringr::str_replace(string_vec_phase2, regex_estacio_de_sa,"estacio de @s@a@")


  return(stringr::str_trim(stringr::str_replace_all(string_vec_phase3, "[:space:]+", " ")))
}

arruma_ltda <- function(string_vec){

  regex_ltda_geral <- stringr::regex("l[ \\.]*t[ \\.]*d[ \\.]*a")

  string_vec_phase1 <- stringr::str_replace(string_vec, regex_ltda_geral,"@l@t@d@a@")

  #regex_sa_particular <- regex("s/a")

  #string_vec_phase2 <- stringr::str_replace(string_vec_phase1, regex_sa_particular,"@l@t@d@a@")

  return(string_vec_phase1)
}

arruma_me <- function(string_vec){
  regex_me_geral <- stringr::regex("([^de]) m *[\\./]?e *\\.?( [(].+[)])?$")

  string_vec_phase1 <- stringr::str_replace(string_vec, regex_me_geral,"\\1 @m@e@\\2")

  regex_me_particular <- regex("m/e")

  string_vec_phase2 <- stringr::str_replace(string_vec_phase1, regex_me_particular,"@m@e@")

  return(string_vec_phase2)
}

clean_nomes <- function(names_column){
  names_column %>%
    arruma_sa() %>%
    arruma_ltda() %>%
    arruma_me() %>%
    abjutils::rm_accent()
}
