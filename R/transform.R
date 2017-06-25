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
    stringr::str_replace_all("[!#$%&'()*+,-./:;<=>?^_`{|}~.]","")
}

vec2regex <- function(...){
  stringr::str_c(c(...), collapse = "|")
}

#' Collapes names of major companies
#'
#' In a vector of company names the same company usually appears with various names.
#' This function lists some of them and aggregates them under one main name.
#'
#' @param names_column a string vector of company names
#' @return the string vector with an aggregation of the names of major companies
#'
#' @export
aggregate_major_companies <- function(names_column){

  lista_de_regex <- list(
    regex_claro = vec2regex("claro", "embratel", "bcp"),
    regex_rio_claro = vec2regex("rio claro"),
    regex_nextel = vec2regex("nextel"),
    #a telemar foi comprada pela oi em algum momento.
    #Essa regex é muito perigosa porque ela transforma a oi na maior demandada do Rio.
    regex_oi = vec2regex("telemar","( |^)oi( |$)", "tnl", "(brasil|br) ?teleco[mn]"),
    regex_itau = vec2regex("unibanco", "itau", "citicard"),
    regex_renner = vec2regex("banco a ?j renner"),
    regex_crefisa = vec2regex("crefisa"),
    regex_cdl_porto_alegre = vec2regex("(cdl|camara( de)? dirigentes lojistas)"),
    regex_banco_credifibra = vec2regex("credifibra"),
    regex_farroupilha_consorcios = vec2regex("farroupilha"),
    regex_cacique = vec2regex("banco cacique"),
    regex_schahin = vec2regex("schahin"),
    regex_aymore = vec2regex("aymore"),
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
    regex_vivo = vec2regex("vivo", "telefonica", "telesp", 'gvt', 'global village teleco[mn]'),
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
    regex_banco_vxolkswagen = vec2regex("banco volks[wv]age[nm]"),
    regex_volkswagen = vec2regex("volks[wv]age[nm]"),
    regex_energisa = vec2regex("energisa","cemat"),
    regex_secretaria_de_saude_mt = vec2regex("estado de mato grosso secretaria (estadual|de estado) de saude"),
    regex_azul = vec2regex("azul linhas"),
    regex_serasa = vec2regex("serasa"),
    regex_spc = vec2regex("spc","scpc"),
    regex_porto_seguro = vec2regex("porto seguro"),
    regex_avon = vec2regex("avon"),
    regex_banco_finasa = vec2regex("finasa"),
    regex_natura = vec2regex("natura","rodobens"),
    regex_tokio_marine = vec2regex("tokio marine"),
    #coloquei um x em fxord pra não pegar na regex de baixo
    regex_banco_fxord = vec2regex("banco ford"),
    regex_ford = vec2regex("ford"),
    regex_basa = vec2regex("banco da amazonia","basa"),
    regex_pdg = vec2regex("pdg", "goldfarb"),
    regex_furnas = vec2regex("furnas"),
    regex_cef = vec2regex("cef","caixa economica federal","caixa"),
    regex_mapfre = vec2regex("mapfre"),
    regex_cvc = vec2regex("cvc"),
    #coloquei um x em fxiat pra não pegar na regex de baixo
    regex_banco_fxiat = vec2regex("banco fiat"),
    regex_ford = vec2regex("fiat"),
    regex_banco_hxonda = vec2regex("banco honda"),
    regex_honda = vec2regex("honda"),
    regex_catho = vec2regex("catho"),
    regex_buscape = vec2regex("buscape"),
    regex_abn = vec2regex("abn amro"),
    regex_riachuelo = vec2regex("riachuelo"),
    regex_banco_rural = vec2regex("banco rural"),
    regex_banco_gmac = vec2regex("banco gmac"),
    regex_renova = vec2regex("renova"),
    regex_modelo = vec2regex("supermercado modelo"),
    regex_philips = vec2regex("philips|phillips"),
    regex_positivo = vec2regex("positivo"),
    regex_lg = vec2regex("lg"),
    regex_lenovo = vec2regex("lenovo"),
    regex_magazine_luiza = vec2regex("magazine luiza"),
    regex_pernambucanas = vec2regex("pernambucanas"),
    regex_brastempo = vec2regex("brastemp"),
    regex_consul = vec2regex("consul"),
    regex_esmaltec = vec2regex("esmaltec"),
    regex_atlas = vec2regex("atlas"),
    regex_latina = vec2regex("latina"),
    regex_mueller = vec2regex("mueller"),
    regex_multiplus = vec2regex("multiplus"),
    regex_smiles = vec2regex("smiles"),
    regex_dotz = vec2regex("dotz"),
    regex_livelo = vec2regex("livelo"),
    regex_avianca = vec2regex("avianca"),
    regex_cab_cuiaba = vec2regex("cab cuiaba"),
    regex_losango = vec2regex("losango"),
    regex_novo_mundo_moveis_mt = vec2regex("novo ?mundo ?moveis"),
    regex_calcar = vec2regex("calcard"),
    regex_ativos = vec2regex("ativos"),
    regex_sanecap_mt = vec2regex("sanecap"),
    regex_prime_mt = vec2regex("prime incorporacoes"),
    regex_general_motors = vec2regex("general motors"),
    regex_lotufo_engenharia = vec2regex("lotufo"),
    regex_sulamerica = vec2regex("sul ?america"),
    regex_omni = vec2regex("omni financeira"),
    regex_brookfield_incorporadora = vec2regex("brookfield"),
    regex_banrisul = vec2regex("banrisul","banco do estado do rio grande do sul"),
    regex_boa_vista = vec2regex("boa vista"),
    regex_ace = vec2regex("ace"),
    regex_aes_sul = vec2regex("aes sul"),
    regex_ceee = vec2regex("ceee","companhia estadual de energia elétrica"),
    regex_cifra = vec2regex("cifra"),
    regex_corsan = vec2regex("corsan"),
    regex_rio_grande_energia = vec2regex("rio grande energia"),
    regex_sofisa = vec2regex("sofisa"),
    regex_cce = vec2regex("cce"))

  #considerando que eu vou fazer um for depois, realmente vale a pena construir esse objeto?
  colunas_bl <- purrr::map(lista_de_regex, str_detect, string = names_column)

  for(i in seq_along(colunas_bl)){
    names_column <- ifelse(colunas_bl[[i]], names(colunas_bl)[i], names_column)
  }
  names_column
}

#' Creates column of companies segment
#'
#' Receives a vector of company names and return a vector os segments.
#'
#' @param names_column a string vector of company names
#' @return the string vector of company segments
#'
#' @export
market_segments <- function(names_column){

  lista_de_regex <- list(
    telecomunicacoes = vec2regex("vivo","oi","claro","tim","sky","net","nextel"),
    bancos_cartoes_financeiras = vec2regex("losango","american express","amex","credicard","banco","ford","sofisa","schahin","cacique","credifibra","farroupilha","aymore","cifra","banco( |_)mercantil","herval","bonsucesso","bgn","citi","gmac","abn","daycoval","safra","cruzeiro( |_)do( |_)sul","banrisul","volkswage[nm]","renner","finasa","cef","itau","bb","bradesco","santander","bmg","panamericano","bv","ourocard","hsbc"),
    comercio_eletronico = vec2regex("centaurocombr","peixe urbano","cnova","kabum","nova pontocom","wmb","buscape","submarino","americanascom","casasbahiacom","pontofriocom","magazineluizacom"),
    banco_de_dados = vec2regex("spc","serasa","scpc","boa( |_)vista","cdl"),
    fabricantes_eletrocnicos = vec2regex("cielo","sony","samsung","lg","positivo","philips","aoc","lenovo","semp toshipa","d-link","cce","lexmark","compaq"),
    varejo = vec2regex("lame","magazine( |_)luiza","vvar","ricardoeletro","pernambucanas","sendas","riachuelo"),
    transporte_aereo = vec2regex("tam","latam","_azul","gol","avianca","lufthansa"),
    seguros = vec2regex("segur","confianca","ace","porto( |_)seguro","lider","mapfre","cardif","caixa segur","bradesco auto","bb seguro auto","garantec","bradesco vida","itau segur","assurant","zurich","luizaseg","sulamerica"),
    energia_gas_agua_esgoto = vec2regex("corsan","rio( |_)grande( |_)energia","ceee","aes( |_)sul","sanecap","cab( |_)cuiaba","furnas","cedae","energisa","light","cpfl","(^| )rge( |$)","copel","comgas","cagece","coelce","ampla","fenosa","saae"),
    fabricantes_linha_branca = vec2regex("brastemp","consul","electrolux","esmaltec","atlas","latina","mueller"),
    agencias_de_viagem = vec2regex("decolar","viajanet","cvc"),
    programas_de_fidelidade = vec2regex("smiles","multiplus","dotz","livelo"),
    planos_de_saude = vec2regex("amil","bradesco saude","qualicorp","sulamerica odontologico","sulamerica saude","odontoprev","unimed","golden cross"),
    recuperacao_credito = vec2regex("crefisa","ibi","ativos","recovery"),
    perfumaria = vec2regex("natura","avon","boticario","eudora"),
    supermercados = vec2regex("extra","carrefour","pao de acucar","walmart","big","hiper"),
    pagamento_eletronico = vec2regex("getnet","paypal","pagseguro","ebanx","moip"))

  #considerando que eu vou fazer um for depois, realmente vale a pena construir esse objeto?
  colunas_bl <- purrr::map(lista_de_regex, str_detect, string = names_column)

  for(i in seq_along(colunas_bl)){
    names_column <- ifelse(colunas_bl[[i]], names(colunas_bl)[i], names_column)
  }
  names_column

}
