rm(list = ls())
options(scipen = 999)
options(warn = -1)

setwd("/Users/anishganguli/Personal Project/Github Repo/IMDB Movies Data Analysis")

library(dplyr)
library(readxl)
library(data.table)
library(stringr)

## Required function for creating proper case

toproper <- function(x){
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

## Reading the Movie Credit Datasets and appending them

pattern1 = list.files(pattern = "xlsx")
data_name = gsub(".xlsx","",pattern1)
data_name = tolower(data_name)

for(i in 1:length(pattern1)){
  assign(data_name[i], (read_excel(pattern1[i],sheet = 1)))
}

rm(i, pattern1)

extra_obs = names(movie_credits_part_2)
names(movie_credits_part_2) = names(movie_credits_part_1)
movie_credits_part_2 = rbind(extra_obs,movie_credits_part_2)
movie_credits = rbind(movie_credits_part_1,movie_credits_part_2) %>% as.data.frame()

rm(movie_credits_part_1,movie_credits_part_2,extra_obs,data_name)

## Reading the Movie Dataset

movie_data = fread("tmdb_5000_movies.csv",stringsAsFactors = F) %>% as.data.frame()

## Reading the ISO 639 -1 language codes

language_code = read_excel("Language.xlsx",sheet = 1)
language_code = language_code[,c("ISO639-1Code","LanguageName")]
language_code = subset(language_code, is.na(language_code$`ISO639-1Code`) != TRUE)
names(language_code)[1] = "original_language"

## Fixing column names for the master dataset

genre_cols = c("id","name")
cast_cols = c("cast_id","character","credit_id","gender","id","name","order")
crew_cols = c("credit_id","department","gender","id","job","name")
prod_comp_cols = c("name","id")
prod_coun_cols = c("iso_3166_1","name")


#############################################


## Functions for restructuring the columns

split_vals = function(data_input,var,col_names){
  list_var1 = NULL
  e1 = NULL
  
  n = length(col_names)
  
  for(i in 1:(length(data_input[,var]))){
    
    a = data_input[,var][i]
    b = strsplit(a,",")[[1]]
    c = gsub("\\[|\\]$","",b)
    d = strsplit(c,": ")

    e=data.frame()

    if(length(d) < n){
      e = matrix("Not Available",nrow=1,ncol=n) %>% as.data.frame()
    }
    else{
      for(iii in 1:(floor(length(d)/n))){
        for(k in 0:(n-1)){
          d[[(n*iii)-k]] = str_extract_all(d[[(n*iii)-k]],"[A-z]+|\\d") %>% unlist()
          e[iii,k+1] = paste(d[[(n*iii)-k]][-1],collapse = " ")
        }
      }
      
      for(l in 1:n){
        for(ll in 1:nrow(e)){
          if(is.numeric(as.numeric(as.character(substr(e[ll,l],1,1)))) == T & substr(e[ll,l],2,2) == " "){
            e[ll,l] = gsub(" ","",e[ll,l])
          }
          else{
            e[ll,l] = e[ll,l]
          }
        }
      }
    }
    
    if(n<=2){
      names(e)[1] = col_names[2]
      names(e)[2] = col_names[1]
    }
    else{
      names(e) = sort(col_names,decreasing = T)
    }


    list_var = NULL
    for(m in 1:n){
      list_var[m] = paste(e[,m],collapse = ", ")
    }
    
    list_var1 = rbind(list_var1,list_var) %>% as.data.frame()
    e1 = rbind(e1,e) %>% as.data.frame()
    print(paste0("Index - ",i))
  }
  names(list_var1) = names(e)
  names(e1) = names(e)
  
  e1 = subset(e1, e1[,1] != e1[,2])
  
  if(var == "cast"){
    e1[,1] = NULL
  }
  
  rownames(list_var1) = NULL
  e1 = distinct(e1)
  
  names(list_var1) = paste0(toproper(var),"-",names(list_var1))
  
  fwrite(e1,paste0(toproper(var),"_Master_Data_",Sys.Date(),".csv"))
  
  
  final_list = list(list_var1,e1)
  return(final_list) 
}

## Structured columns in a data frame

genres = split_vals(movie_data,"genres",genre_cols)[[1]]
cast = split_vals(movie_credits,"cast",cast_cols)[[1]]
crew = split_vals(movie_credits,"crew",crew_cols)[[1]]
prod_comp = split_vals(movie_data,"production_companies",prod_comp_cols)[[1]]
prod_country = split_vals(movie_data,"production_countries",prod_coun_cols)[[1]]

## Master data based on Cast_ID, Crew_ID, Genre_ID

genres_level = split_vals(movie_data,"genres",genre_cols)[[2]]
cast_level = split_vals(movie_credits,"cast",cast_cols)[[2]]
crew_level = split_vals(movie_credits,"crew",crew_cols)[[2]]
prod_comp_level = split_vals(movie_data,"production_companies",prod_comp_cols)[[2]]
prod_country_level = split_vals(movie_data,"production_countries",prod_coun_cols)[[2]]

## Fixing the language column in Movie Data

movie_data = movie_data %>% left_join(language_code,by="original_language")
movie_data$LanguageName[movie_data$original_language == "cn"] = "Chinese"
movie_data$LanguageName[movie_data$original_language == "xx"] = "Invented Language"

## Final structured Movie and Credits dataset

movie_data_structured = cbind(movie_data,genres,prod_comp,prod_country) %>% as.data.frame()
movie_data_structured = movie_data_structured %>% dplyr::select(-genres,-keywords,-overview,-production_companies,-production_countries,-original_language,-spoken_language)
names(movie_data_structured)[3] = "movie_id"

######################

movie_credits_structured = cbind(movie_credits,cast,crew) %>% as.data.frame()
movie_credits_structured = movie_credits_structured %>% dplyr::select(-crew,-cast)

## Clearing up the environment

rm(cast,cast_cols,crew,crew_cols,genres,genre_cols,prod_comp,prod_comp_cols,prod_country,prod_coun_cols)

## Exporting the structured dataframes and language data to the working directory

fwrite(movie_data_structured,paste0("Movie_Data_structured_",Sys.Date(),".csv"))
fwrite(movie_credits_structured,paste0("Movie_Credits_structured_",Sys.Date(),".csv"))
fwrite(language_code,paste0("Language_ISO639-1_Code_",Sys.Date(),".csv"))
