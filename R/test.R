get_citations_publications = function(cid)
{
  auths = ""
  url_template = "http://scholar.google.com/scholar?oi=bibs&hl=es&scisbd=2&cites=%s"
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
#' @param text 
#' @param exact_text 
#' @param author 
#' @param from 
#'
#' @return
#' @export
#'
#' @examples
search_publication = function(text, exact_text="", author="",from=0)
{
  url_template = "http://scholar.google.com/scholar?as_q=%s&as_epq=%s&as_sauthors=%s&start=%s"
  url = sprintf(url_template, encode(text), encode(exact_text),encode(author),from)
  
  #https://scholar.google.com/scholar?start=10&as_q=ecj%3A+a+java-based+evolutionary&as_epq=&as_oq=&as_eq=&as_occt=any&as_sauthors=luke&as_publication=&as_ylo=&as_yhi=&hl=es&as_sdt=0%2C5
  
  page<- get_scholar_resp(url) %>% read_html()
  
  pubs<-rvest::html_nodes(page,xpath="//div[@class='gs_r gs_or gs_scl']")
  
  pubid <- pubs %>% rvest::html_attr("data-cid")
  title <- pubs %>% rvest::html_nodes(xpath="//h3[@class='gs_rt']") %>% 
    rvest::html_text() %>% stringr::str_replace("\\[.*\\] ","")
  
  info <- pubs %>% rvest::html_nodes(xpath="//div[@class='gs_a']") 
  authid<- rvest::html_node(info,xpath="a") %>% rvest::html_attr("href") %>%
    stringr::str_extract("user=[a-zA-Z0-9_\\-]*&") %>% stringr::str_sub(6,-2)
  
  authors<-sapply(info,FUN=function(data){
    new_data <- data %>% rvest::html_text() %>% stringr::str_split("- ") %>% unlist()
    new_data[1] %>% stringr::str_sub(1,-2)
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
  publications <- data.frame(title=title,
                     author=authors,
                     journal=journals,
                     authorid=authid,
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
#' @param cid 
#' @param year 
#'
#' @return
#' @export
#'
#' @examples
get_citation_year = function(cid,year)
{
  url_template = "http://scholar.google.com/scholar?cites=%s&as_ylo=%s&as_yhi=%s"
  url = sprintf(url_template, as.character(cid),year,year)
  
  page <- get_scholar_resp(url) %>% read_html()
  
  return(get_num_results(page))
}