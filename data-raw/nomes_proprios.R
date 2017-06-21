nomes <- NULL

i <- 1

url <- 'https://www.dicionariodenomesproprios.com.br/nomes-masculinos/'
a <- rvest::html_session(url) %>%
  rvest::html_nodes(".lista-nome") %>%
  rvest::html_text()

while(a[1] != "Matheus"| i == 1){
  i <- i + 1
  url <- sprintf('https://www.dicionariodenomesproprios.com.br/nomes-masculinos/%s/',i)

  a <- rvest::html_session(url) %>%
    rvest::html_nodes(".lista-nome") %>%
    rvest::html_text()

  nomes <- c(nomes,a)

  print(i)
}

nomes <- unique(nomes)

nomes_femininos <- NULL

i <- 1

url <- 'https://www.dicionariodenomesproprios.com.br/nomes-femininos/'
a <- rvest::html_session(url) %>%
  rvest::html_nodes(".lista-nome") %>%
  rvest::html_text()

while(a[1] != "Alice"| i == 1){
  i <- i + 1
  url <- sprintf('https://www.dicionariodenomesproprios.com.br/nomes-femininos/%s/',i)

  a <- rvest::html_session(url) %>%
    rvest::html_nodes(".lista-nome") %>%
    rvest::html_text()

  nomes_femininos <- c(nomes_femininos,a)

  print(i)
}

nomes <- stringr::str_to_lower(abjutils::rm_accent(nomes))
nomes_femininos <- stringr::str_to_lower(abjutils::rm_accent(nomes_femininos))

nomes_proprios <- data_frame(nome_proprio = c(nomes, nomes_femininos), genero = c(rep("M",length(nomes)), rep("F", length(nomes_femininos))))
