library(RMySQL)
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(RODBC)
library(DBI)
library(pool)
# library(odbc)
# sort(unique(odbcListDrivers()[[1]]))
# ------------------------------------------------------------- #
#library(golem)
#library(gt)
#library(RJDBC)
#install.packages("RODBC", dependencies = T)

#install.packages(c("pool", "shinyWidgets", "bs4Dash"))
#install.packages("odbc")
#devtools::install_github("r-lib/vctrs")
#install.packages("DBI", dependencies = T)
#install.packages("shinymaterial")



#install.packages("golem")
#install.packages("RJDBC",dep=TRUE)
#remotes::install_github("rstudio/gt")
# ------------------------------------------------------------- #
utas_red <<- "#e42312"
utas_black <<- "#000000"
utas_white <<- "#FFFFFF"
BUTTON_STYLE <<- "bordered"
BUTTON_SIZE <<- "sm"
TEXT_AREA_WIDTH <<- '465px'
# ------------------------------------------------------------- #
databaseName <<- "g6Mj2lugZA"
# ------------------------------------------------------------- #
options(mysql = list(
    "host" = "remotemysql.com",
    "port" = 3306,
    "user" = "g6Mj2lugZA",
    "password" = "ipYc79lTd0"
))

db <<- data_connection()

pool <<- dbPool(
    drv = RMySQL::MySQL(),
    dbname = "g6Mj2lugZA",
    host = "remotemysql.com",
    username = "g6Mj2lugZA",
    password = "ipYc79lTd0"
)


# ------------------------------------------------------------- #
options(shiny.trace=TRUE)
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# DATA BASE CALLS #######
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
data_connection <- function()
{
   return(dbConnect(MySQL()
                    , dbname = databaseName
                    , host = options()$mysql$host
                    , port = options()$mysql$port
                    , user = options()$mysql$user
                    , password = options()$mysql$password)) 
}
# ------------------------------------------------------------- #
load_nav_bar_menu <- function() 
{
    #db <- data_connection()
    colleges <- tbl(db, "colleges") %>% collect()
    #dbDisconnect(db)
    return(colleges)
}
# ------------------------------------------------------------- #
write_strategy <- function(c_id, s_name, s_description)
{
    #db <- data_connection()
    df <- data.frame(strategy_id = round(runif(1, 1, 90000000),0)
                       , strategy_name = s_name
                       , strategy_description = s_description
                       , college_id = c_id
                       , active_strategy = "y") %>% 
                    mutate_if(is.factor, as.character)

    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')"
                        , "strategy"
                        , paste(names(df), collapse = ", ")
                        , paste(df, collapse = "', '"))
    
    rs <- dbSendStatement(db, query)
    #dbDisconnect(db)
    return(dbHasCompleted(rs))
    
}
# ------------------------------------------------------------- #
read_strategies <- function(c_id)
{
    #db <- data_connection()
    return(dbReadTable(db, "strategy") %>% 
                    filter(college_id == c_id) %>% 
                    filter(active_strategy == "y"))
    #dbDisconnect(db)
}
# ------------------------------------------------------------- #
write_initiative <- function(in_name, s_date, e_date, in_description,  s_id, c_id)
{
    # in_name <- "cose initiative"
    # s_date <-  "2019-08-13"
    # e_date <- "2019-08-17"
    # in_description <- "initiative description text"
    # s_id <- as.integer(6176566)
    # c_id <- as.integer(11)
    #db <- data_connection()
    df <- data.frame(
              initiative_id = round(runif(1, 1, 90000000),0)
            , initiative_description = in_description
            , initiative_name = in_name
            , start_date = s_date
            , end_date = e_date
            , strategy_id = s_id
            , college_id = c_id
            , initiative_active = "y") %>% 
        mutate_if(is.factor, as.character)

    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')"
                     , "initiatives"
                     , paste(names(df), collapse = ", ")
                     , paste(df, collapse = "', '"))
    
    rs <- dbSendStatement(db, query)
    return(dbHasCompleted(rs))
    #dbDisconnect(db)
}
# ------------------------------------------------------------- #
# UI MODALS #######
# ------------------------------------------------------------- #
strategy_load <- function(failed = FALSE) 
{
    modalDialog(
          size = "m"
        , easyClose = F
        , fade = T
        , title = uiOutput("collegeTitle")
        , uiOutput("new_strategy_title")
        , uiOutput("new_strategy_description")
        , footer = tagList(
            modalButton("Cancel"),
            actionButton("strategy_ok", "OK")))
}
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
initiative_load <- function(failed = FALSE) 
{
    modalDialog(
        size = "m"
        , easyClose = F
        , fade = T
        , title = uiOutput("collegeTitle")
        , uiOutput("strategy_picker")
        , uiOutput("new_initiative_title")
        , uiOutput("new_initiative_description")
        , footer = tagList(
            modalButton("Cancel"),
            actionButton("initiative_ok", "OK")))
}
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #