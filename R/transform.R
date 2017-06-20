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

#'Fix column of brazillian company names
#'
#'This function receives a string vector of company names,
#'substitutes all types of "s/a" by "@s@a@", all types of "ltda." by "@l@t@d@a" and all types of "m.e." by "@m@e@".
#'At the end, it trims the vector, replaces multiple spaces by one and removes accentuation
#'
#'@param names_column a string vector of company names
#'@return a better version of names_column
#'
#'@export
fix_nomes <- function(names_column){
  names_column %>%
    stringr::str_to_lower() %>%
    arruma_sa() %>%
    arruma_ltda() %>%
    arruma_me() %>%
    abjutils::rm_accent() %>%
    tm::removePunctuation()
}

vec2regex <- function(...){
  stringr::str_c(c(...), collapse = "|")
}

#' Collapes names of major companies
#'
#' In a vector of company names the same company usually appears with various names.
#' This function lists some of them and aggregates them unde one main name.
#'
#' @param names_column a string vector of company names
#' @return the string vector with and aggregation of the names of major companies
#'
#' @export
aggregate_major_companies <- function(names_column){

  lista_de_regex <- list(
    regex_claro = vec2regex("claro", "embratel", "bcp"),
    regex_rio_claro = vec2regex("rio claro"),
    regex_nextel = vec2regex("nextel"),
    #a telemar foi comprada pela oi em algum momento.
    #Essa regex é muito perigosa porque ela transforma a oi na maior demandada do Rio.
    regex_oi = vec2regex("telemar","( |^)oi( |$)", "tnl", "brasil telecom"),
    regex_itau = vec2regex("itau", "citicard"),
    regex_bradesco = vec2regex("bradesc"),
    regex_light = vec2regex("light","ligth"),
    regex_rio = vec2regex("estado do rio de janeiro"),
    regex_santander = vec2regex("santander"),
    regex_tim = vec2regex("tim"),
    regex_ampla = vec2regex("ampla"),
    regex_vvar = vec2regex("via varejo","vvar","casa[s]? bahia", "globex"),
    regex_bmg = vec2regex("bmg"),
    regex_bb = vec2regex("banco do brasil", "bb"),
    regex_sky = vec2regex("sky"),
    regex_vivo = vec2regex("vivo", "telefonica", "telesp", 'gvt', 'global village telecom'),
    regex_ricardoeletro = vec2regex("ricardo ?eletro", "rn comercio"),
    regex_net = vec2regex("^net$", "^net ", " net ", " net$"),
    regex_panamericano = vec2regex("banco pan"),
    regex_cedae = vec2regex("cedae", "(cia[.]?|companhia) estadual de aguas"),
    regex_leader = vec2regex("leader"),
    regex_bv = vec2regex("bv financeira","^bv "),
    regex_amil = vec2regex("amil"),
    regex_lider = vec2regex("seguradora lider"),
    regex_cnova = vec2regex("cnova", "nova ?ponto ?com"),
    regex_samsung = vec2regex("samsung"),
    regex_tam = vec2regex("tam"),
    regex_lame = vec2regex("lojas americanas"),
    regex_ca = vec2regex("c&a"),
    regex_unimed = vec2regex("unimed"),
    regex_b2w = vec2regex("b2w"),
    regex_electrolux = vec2regex("ele[c]?trolux"),
    regex_citi = vec2regex("citibank"),
    regex_mrv = vec2regex("mrv"),
    regex_estacio_de_sa = vec2regex("estacio de sa"),
    regex_qualicorp = vec2regex("qualicorp"),
    regex_carrefour = vec2regex("carrefour"),
    regex_daycoval = vec2regex("daycoval"),
    regex_hsbc = vec2regex("hsbc"),
    regex_bonsucesso = vec2regex("bonsucesso"),
    regex_ibi = vec2regex("ibi"),
    regex_cruzeiro_do_sul = vec2regex("cruzeiro do sul"),
    regex_bgn = vec2regex("bgn"),
    regex_cielo = vec2regex("cielo"),
    regex_zurich = vec2regex("zurich"),
    regex_gol = vec2regex("vrg","gol"),
    regex_sony = vec2regex("sony"),
    regex_sendas = vec2regex("sendas"),
    regex_safra = vec2regex("safra"),
    regex_golden_cross = vec2regex("golden cross"),
    regex_banco_mercantil = vec2regex("banco mercantil"),
    regex_avista = vec2regex("avista"),
    regex_digibras = vec2regex("digibras"),
    regex_inss = vec2regex("inss","instituto nacional do seguro social"),
    #coloquei um x aqui pra não pegar na regex de baixo
    regex_banco_vxolkswagen = vec2regex("banco volkswagen"),
    regex_volkswagen = vec2regex("volkswagen"),
    regex_energisa = vec2regex("energisa"),
    regex_secretaria_de_saude_mt = vec2regex("estado de mato grosso secretaria (estadual|de estado) de saude"),
    regex_azul = vec2regex("azul linhas"),
    regex_serasa = vec2regex("serasa"),
    regex_cemat = vec2regex("cemat"),
    regex_porto_seguro = vec2regex("porto seguro"),
    regex_avon = vec2regex("avon"),
    regex_banco_finasa = vec2regex("finasa"),
    regex_natura = vec2regex("natura","rodobens"),
    regex_tokio_marine = vec2regex("tokio marine"),
    #coloquei um x em fxord pra não pegar na regex de baixo
    regex_banco_fxord = vec2regex("banco ford"),
    regex_ford = vec2regex("ford"),
    regex_basa = vec2regex("banco da amazoinia","basa"),
    regex_pdg = vec2regex("pdg", "goldfarb"),
    regex_furnas = vec2regex("furnas"),
    regex_cef = vec2regex("cef","caixa economica federal"),
    regex_mapfre = vec2regex("mapfre"),
    regex_cvc = vec2regex("cvc"),
    #coloquei um x em fxiat pra não pegar na regex de baixo
    regex_banco_fxiat = vec2regex("banco fiat"),
    regex_ford = vec2regex("fiat"),
    regex_banco_hxonda = vec2regex("banco honda"),
    regex_honda = vec2regex("honda"),
    regex_catho = vec2regex("catho"),
    regex_buscape = vec2regex("buscape"),
    regex_abn = vec2regex("banco abn amro"),
    regex_riachuelo = vec2regex("riachuelo"),
    regex_banco_rural = vec2regex("banco rural"),
    regex_banco_gmac = vec2regex("banco gmac"),
    regex_renova = vec2regex("renova"),
    regex_modelo = vec2regex("supermercado modelo"),
    regex_cav)

  #considerando que eu vou fazer um for depois, realmente vale a pena construir esse objeto?
  colunas_bl <- purrr::map(lista_de_regex, str_detect, string = names_column)

  for(i in seq_along(colunas_bl)){
    names_column <- ifelse(colunas_bl[[i]], names(colunas_bl)[i], names_column)
  }
  names_column
}
