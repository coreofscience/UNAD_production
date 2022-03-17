library(tidyverse)
library(rvest)
library(here)
library(openxlsx)
library(scholar)
library(stringi)
library(margaret)

researchers <- read.csv("investigadores.csv", header = T)
scholar_researchers <- researchers |> filter(!is.na(id_scholar)) |> 
  filter(!is.na(url)) |> 
  select(researcher, id_scholar, url)


data_grupos_all <- 
  apply(scholar_researchers, 1, function(x){
    URL <- read_html(x['url'])})

grupo_df <- 
  tibble(grupo = character(),
         producto = character(),
         categoria = character())

for (i in 1:length(scholar_researchers$url)) {
  
  grupo <- 
    data_grupos_all[[i]] |> 
    html_table()
  
  for (j in 1:length(grupo[])) {
    
    df_1 = 
      grupo %>% 
      tibble() %>% 
      slice(j) %>% 
      unlist %>% 
      tibble() %>% 
      rename(producto = ".") %>% 
      mutate(grupo = scholar_researchers$researcher[[i]])
    
    if (length(df_1$producto) > 1) {
      
      df_2 =
        df_1 %>% 
        filter(producto != "") %>% 
        mutate(categoria = df_1$producto[1]) %>%
        select(grupo, producto, categoria)
      
    } else {
      
      df_2 = 
        df_1 %>% 
        mutate(categoria = df_1$producto[1],
               producto = "NO TIENE") %>% 
        select(grupo, producto, categoria) %>% 
        unique()
      
    }
    
    grupo_df <- 
      bind_rows(df_2,
                grupo_df) 
    
  }
}

articulos <- grupo_df |> filter(categoria == "Artículos") |> 
  mutate(DOI = str_extract(producto, "DOI.*")) |> 
  filter(!is.na(DOI)) |> 
  mutate(DOI = str_remove(DOI, ".*DOI:"),
         DOI = str_trim(DOI)) |> 
  filter(DOI != "")

library(tidyverse)
library(here)
library(revtools)
library(ggpubr) 
library(gt)
library(dplyr)
library(formattable)
library(readxl)
library(googlesheets4)
library(sjrdata)
library(knitr)
library(kableExtra)
library(apaTables)
library(bibliometrix)
library(igraph)
library(tosr)
library(rebus)
library(lubridate)
library(zoo)
library(XML)
library(plyr)
library(sjrdata)
library(journalabbr)



references <- data.frame(DI = character(), 
                         PU = character(), 
                         SO = character(), 
                         J9 = character(), 
                         PD = character(), 
                         PY = character(), 
                         TI = character(), 
                         AF = character(),
                         Autor = character(),
                         stringsAsFactors = FALSE)
authors <- data.frame(doi = character(), 
                      author = character(), 
                      year = character(), 
                      month = character(), 
                      stringsAsFactors = FALSE)

for (i in articulos$DOI) {
  for(j in articulos$grupo){
    doi <- i
    url <- paste0("http://api.crossref.org/works/", doi, ".xml")
    xml_data_1 = try(xmlParse(url), silent = TRUE);
    if (class(xml_data_1) == "try-error") {
      next
    } else  {
      xml_data_2 <- xmlToList(xml_data_1)
      
      notfound =try(as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article", silent = TRUE);
      if (class(notfound) == "try-error"){
        next
      }else{
        
        if (as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi"]][[".attrs"]]) == "journal_article"){
          
          # PUBLISHER-NAME
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])){
            PU <- as.character(NA) 
          } else {
            
            publisher0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["crm-item"]])
            publisher <- data.frame(publisher0)
            if(nrow(publisher) == 0){
              PU <- as.character(NA)
            }else{
              PU <- as.character(publisher$text[1])
            }
          }
          
          # JOURNAL FULL TITLE 
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])){
            SO <- as.character(NA) 
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["full_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              SO <- as.character(NA)
            }else{
              SO <- as.character(journal[1,1])
            }
          }
          
          # JOURNAL ABBREV TITLE 
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])){
            J9 <- as.character(NA) 
          } else {
            
            journal0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_metadata"]][["abbrev_title"]])
            journal <- data.frame(journal0)
            if(nrow(journal) == 0){
              J9 <- as.character(NA)
            }else{
              J9 <- as.character(journal[1,1])
            }
          }
          
          # MONTH
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])){
            PD <- as.character(NA) 
          } else {
            
            month0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["month"]])
            month <- data.frame(month0)
            if(nrow(month) == 0){
              PD <- as.character(NA)
            }else{
              PD <- as.character(month[1,1]) 
            }
          }
          
          # YEAR
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])){
            PY <- as.character(NA) 
          } else {
            
            Year0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_issue"]][["publication_date"]][["year"]])
            Year <- data.frame(Year0)
            if(nrow(Year) == 0){
              PY <- as.character(NA)
            }else{
              PY <- as.character(Year[1,1]) 
            }
          }
          
          # TITLE
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])){
            TI <- as.character(NA) 
          } else {
            
            title0 <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["titles"]][["title"]])
            title <- try(data.frame(title0), silent = TRUE);
            
            if(class(title) == "try-error"){
              titlex <- try(ldply(title0, data.frame), silent = TRUE);
              if(class(titlex) == "try-error"){
                TI <- as.character(NA)
              }else{
                TI0 <- as.character(titlex[1,2])
                TI <- trimws(TI0)
              }
            }else{
              if(nrow(title) == 0){
                TI <- as.character(NA)
              }else{
                TI <- as.character(title[1,1])
              }
            }
          }
          
          # CONTRIBUTORS
          
          
          if (is.null(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])){
            AF <- as.character(NA)
          } else {
            
            author_ref <- as.list(xml_data_2[["query_result"]][["body"]][["query"]][["doi_record"]][["crossref"]][["journal"]][["journal_article"]][["contributors"]])
            
            author_1_ref <- ldply(author_ref, data.frame)
            author_2_ref <- author_1_ref[author_1_ref$.attrs == "author", c(2,3) ]
            
            authorss <- data.frame(doi= doi, author = paste0(author_2_ref$surname, ", ", author_2_ref$given_name), year = PY, month = PD)
            
            authors = rbind(authors, authorss)
            authorss$author <- trim(authorss$author)
            AF <- as.character(paste(authorss$author, collapse = ";   "))
          }
          
          references0 <- data.frame(DI = doi, PU = PU, SO = SO, J9 = J9, PD = PD, PY = PY, TI = TI, AF = AF, Autor=j)
          references = rbind(references, references0) 
          
        }else {
          next}
      }
    }
  }
} 
