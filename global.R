# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
# global.R
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(RJDBC)
library(RMySQL)
library(DBI)
library(rJava)
library(timevis)
# sort(unique(odbcListDrivers()[[1]]))
#install.packages("timevis")
# ------------------------------------------------------------- #
#library(golem)
#library(gt)
#remotes::install_github("rstudio/gt")
# ------------------------------------------------------------- #
utas_red <<- "#e42312"
utas_black <<- "#000000"
utas_white <<- "#FFFFFF"
BUTTON_STYLE <<- "bordered"
BUTTON_SIZE <<- "sm"
TEXT_AREA_WIDTH <<- '465px'
MILESTONE_TEXT_AREA_HEIGHT <<- '265px'
DROP_SHADOW_TEXT <<- " -webkit-box-shadow: 3px 3px 3px 0px rgba(181,181,181,1); -moz-box-shadow: 3px 3px 3px 0px rgba(181,181,181,1); box-shadow: 3px 3px 3px 0px rgba(181,181,181,1);"
# ------------------------------------------------------------- #
databaseName <<- "g6Mj2lugZA"
# ------------------------------------------------------------- #
options(mysql = list(
    "host" = "remotemysql.com",
    "port" = 3306,
    "user" = "g6Mj2lugZA",
    "password" = "ipYc79lTd0"
))
# ------------------------------------------------------------- #
# UTAS CONNECTION STRING ####
# ------------------------------------------------------------- #
#jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath=paste0(getwd(),"/OJDBC/ojdbc6.jar"))
# db <<- dbConnect(jdbcDriver
#                , "jdbc:oracle:thin:@//exa2-scan.its.utas.edu.au:1521/edwdev_maa"
#                , rstudioapi::askForPassword("Database user")
#                , rstudioapi::askForPassword("Database password"))
# ------------------------------------------------------------- #
jdbcDriver <- JDBC(driverClass="com.mysql.cj.jdbc.Driver"
                   ,classPath=paste0(getwd(),"/OJDBC/mysql-connector-java-8.0.17.jar")
                   , identifier.quote="`")
db <<- dbConnect(jdbcDriver, "jdbc:mysql://remotemysql.com:3306/g6Mj2lugZA","g6Mj2lugZA","ipYc79lTd0")
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
    colleges <- dbGetQuery(db, "SELECT * FROM colleges")
    return(colleges)
}
# ------------------------------------------------------------- #
write_strategy <- function(c_id, s_name, s_description)
{
    df <- data.frame(strategy_id = round(runif(1, 1, 90000000),0)
                        , strategy_name = s_name
                        , strategy_description = s_description
                        , college_id = c_id
                        , active_strategy = "y"
                        , date_created = Sys.Date()) %>% 
                    mutate_if(is.factor, as.character)

    rs <- dbWriteTable(db, "strategy", df, append = T, overwrite = F)
    return(rs)
    
}
# ------------------------------------------------------------- #
read_strategies <- function(c_id)
{
    df <- dbGetQuery(db, "SELECT * FROM strategy") %>% 
                    filter(college_id == as.integer(c_id)) %>% 
                    filter(active_strategy == "y")
    return(df)
}
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
read_initiative <- function(c_id, s_id)
{
    df <- dbGetQuery(db, "SELECT * FROM initiatives") %>% 
               filter(college_id == as.integer(c_id)) %>%
               filter(strategy_id == as.integer(s_id)) %>%
               filter(initiative_active == "y")
    return(df)
}
# ------------------------------------------------------------- #
write_initiative <- function(in_name, in_description,  s_id, c_id)
{
    df <- data.frame(
              initiative_id = round(runif(1, 1, 90000000),0)
            , initiative_name = in_name
            , initiative_description = in_description
            , strategy_id = s_id
            , college_id = c_id
            , initiative_active = "y"
            , date_created = Sys.Date()) %>% 
        mutate_if(is.factor, as.character)
    
    rs <- dbWriteTable(db, "initiatives", df, append = T, overwrite = F)
    return(rs)
}
# ------------------------------------------------------------- #
write_milestone <- function(mile_name, mile_description, in_id,mile_start_date, mile_end_date)
{
    df <- data.frame(
                  milestone_id = round(runif(1, 1, 90000000),0)
                , milestone_name = mile_name
                , milestone_description = mile_description
                , user_id = 123
                , start_date = mile_start_date
                , end_date = mile_end_date
                , initiative_id = in_id
                , milestone_active ="y"
                , milestone_progress_status = "on track"
                , date_created = Sys.Date())
    rs <- dbWriteTable(db, "milestones", df, append = T, overwrite = F)
    return(rs)
}
# ------------------------------------------------------------- #
main_modul_read_initiative <- function(id_list)
{
    df <-  dbGetQuery(db, "SELECT * FROM initiatives") %>%
               filter(strategy_id %in% id_list) %>%
               filter(initiative_active == "y") %>%
               select(-date_created, -college_id, -initiative_active)
    return(df)
}
# ------------------------------------------------------------- #
main_modul_read_milestone <- function(id_list)
{
    df <- dbGetQuery(db, "SELECT * FROM milestones") %>%
               filter(initiative_id %in% id_list) %>%
               filter(milestone_active == "y") %>%
               select(-date_created, -milestone_active)
   return(df)
}
# ------------------------------------------------------------- #
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
milestone_load <- function(failed = FALSE) 
{
    modalDialog(
        size = "m"
        , easyClose = F
        , fade = T
        , title = uiOutput("collegeTitle")
        , uiOutput("strategy_picker")
        , uiOutput("initiative_picker")
        , uiOutput("start_date_picker")
        , uiOutput("end_date_picker")
        , uiOutput("new_milestone_title")
        , uiOutput("new_milestone_description")
        , footer = tagList(
            modalButton("Cancel"),
            actionButton("milestone_ok", "OK")))
}
# ------------------------------------------------------------- #
# MODULE FUNCTION CALLS ########
# ------------------------------------------------------------- #
