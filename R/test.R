library(magrittr)

get_citations_publications = function(cid)
{
  auths = ""
  url_template = "http://scholar.google.com/scholar?oi=bibs&hl=es&scisbd=2&cites=%s&hl=en"
  url = sprintf(url_template, as.character(cid))
  
  #https://scholar.google.com/scholar?hl=es&as_sdt=0%2C5&sciodt=0%2C5&cites=4539944020768348881&scipsc=&as_ylo=2019&as_yhi=2019
  #url_template <- "http://scholar.google.com/citations?hl=en&user=%s&cstart=%d&pagesize=%d&sortby=pubdate"
  #url <- sprintf(url_template, "jwxhFAQAAAAJ", 0, 100)
  
  url1<- get_scholar_resp(url) %>% read_html()
  pubs<-rvest::html_nodes(url1,xpath="//div[@class='gs_ri']")
  return(pubs)
}

#' Search publication
#' 
#' Searches publications in Google Scholar.
#'
#' @param text Text to search
#' @param exact_text Exact text to search
#' @param author Author from publication
#' @param from Starting point of search
#' @param until Ending point of search
#'
#' @return Publication dataframe
#' @export
search_publication = function(text, exact_text="", author="",from=0,until=10)
{
  pubs<-search_publication_one_page(text,exact_text,author,from)
  from <- from+10
  while (from < until) {
    print("hola")
    pub<-search_publication_one_page(text,exact_text,author,from)
    if(!is.data.frame(pub)){
      from<-until
    }
    else{
      pubs %<>% dplyr::bind_rows()
      from <- from+10
    }
  }
  return(pubs)
}

search_publication_one_page = function(text,exact_text,author,from)
{
  url_template = "http://scholar.google.com/scholar?as_q=%s&as_epq=%s&as_sauthors=%s&start=%s&hl=en"
  url = sprintf(url_template, encode(text), encode(exact_text),encode(author),from)
  
  #https://scholar.google.com/scholar?start=10&as_q=ecj%3A+a+java-based+evolutionary&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=luke&as_publication=&as_ylo=&as_yhi=&hl=es&as_sdt=0%2C5
  
  page<- get_scholar_resp(url) %>% read_html()
  
  pubs<-rvest::html_nodes(page,xpath="//div[@class='gs_r gs_or gs_scl']")
  
  pubid <- pubs %>% rvest::html_attr("data-cid")
  title <- pubs %>% rvest::html_nodes(xpath="//h3[@class='gs_rt']") %>% 
    rvest::html_text() %>% stringr::str_replace("\\[.*\\] ","")
  
  info <- pubs %>% rvest::html_nodes(xpath="//div[@class='gs_a']") 
  
  authors<-lapply(info,FUN=function(data){
    links<- rvest::html_nodes(data,xpath="a")
    link_authors<- links %>% html_text()
    ids<- links %>% rvest::html_attr("href") %>%
      stringr::str_extract("user=[a-zA-Z0-9_\\-]*&") %>% stringr::str_sub(6,-2)
    new_data <- data %>% rvest::html_text() %>% stringr::str_split("- ") %>% unlist()
    auths<-new_data[1] %>% stringr::str_sub(1,-2) %>% stringr::str_replace("…","") %>%
      stringr::str_split(",") %>% unlist() %>% stringr::str_trim()
    authids<-rep(NA,length(auths))
    index<-match(link_authors,auths)
    authids[index]=ids
    data.frame(author=auths,id=authids)
  })
  
  journals<-sapply(info,FUN=function(data){
    new_data <- data %>% rvest::html_text() %>% stringr::str_split("- ") %>% unlist()
    new_data[2] %>% stringr::str_sub(1,-8)
  })
  
  years<-sapply(info,FUN=function(data){
    new_data <- data %>% rvest::html_text() %>% stringr::str_split("- ") %>% unlist()
    new_data[2] %>% stringr::str_sub(-5,-1) %>% as.integer()
  })
  
  extra_info <- pubs %>% rvest::html_nodes(xpath="//div[@class='gs_fl']")
  
  cid <- sapply(extra_info, FUN=function(data){
    tags<-data %>% rvest::html_nodes("a") %>% rvest::html_attr("href") %>%
      stringr::str_extract("cites=[a-zA-Z0-9_\\-]*&") %>% stringr::str_sub(7,-2)
    pos<-(!is.na(tags)) %>% which()
    tags[pos]
  })
  
  cites<-sapply(extra_info, FUN=function(data){
    data_split <- rvest::html_text(data) %>% stringr::str_split(" ") %>% unlist()
    keyword_array<-stringr::str_detect(data_split,"by")
    if (any(keyword_array))
    {
      pos<-keyword_array %>% which()+1
      as.integer(data_split[pos])
    }
    else
      0
  })
  
  ## Put it all together
  publications <- tibble::tibble(title=title,
                             author=authors,
                             journal=journals,
                             cites=cites,
                             year=years,
                             cid=cid,
                             pubid=pubid)
  
  
  return(publications)
}

encode = function(text)
{
  return(gsub(" ", "+", gsub(":","%3A", text)))
}

get_num_results = function(page)
{
  results<-rvest::html_nodes(page,xpath="//div[@class='gs_ab_mdw']")[2] %>% rvest::html_text()
  results_split<-unlist(stringr::str_split(results," "))
  pos<-stringr::str_detect(results_split,"result") %>% which()-1
  result<-results_split[pos]
  if (length(result) > 0)
    return(as.integer(result))
  else
    return(0)
}

#' Get number of citations in a year
#' 
#' Gets the number of citations of a publication in a year.
#'
#' @param cid Citation id of the publication
#' @param year Year to search
#'
#' @return Number of publications in a year
#' @export
get_citation_year = function(cid,year)
{
  url_template = "http://scholar.google.com/scholar?cites=%s&as_ylo=%s&as_yhi=%s&hl=en"
  url = sprintf(url_template, as.character(cid),year,year)
  
  page <- get_scholar_resp(url) %>% read_html()
  
  return(get_num_results(page))
}

#' Get citations per year
#' 
#' Gets the number of citations of a publication per year.
#'
#' @param cid Citation id of the publication
#' @param year Year of publication
#'
#' @return Dataframe with number of cites per year
#' @export
get_citations_per_year = function(cid,year)
{
  years<-year:lubridate::year(Sys.Date())
  cites<-sapply(years,get_citation_year, cid=cid)
  
  i=1;
  cite<-get_citation_year(cid,year-i)
  while (cite>0) {
    years <- c(year-i,years)
    cites<-c(cite,cites)
    i<-i+1
    cite<-get_citation_year(cid,year-i)
  }
  
  return(data.frame(year=years,cites=cites))
}