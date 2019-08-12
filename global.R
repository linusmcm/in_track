library(RMySQL)
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
library(shinymaterial)
# ------------------------------------------------------------- #
#library(golem)
#library(gt)
#library(RJDBC)
#install.packages("shinymaterial")
#install.packages("golem")
#install.packages("RJDBC",dep=TRUE)
#remotes::install_github("rstudio/gt")
# ------------------------------------------------------------- #
utas_red <<- "#e42312"
utas_black <<- "#000000"
utas_white <<- "#FFFFFF"
buttonStyle <<- "bordered"
buttonSize <<- "sm"
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
options(shiny.trace=TRUE)
# ------------------------------------------------------------- #
saveData <- function(db, data) 
{
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s')",
        table, 
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '")
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    #dbDisconnect(db)
}
# ------------------------------------------------------------- #
load_nav_bar_menu <- function() 
{
    db <- dbConnect(MySQL()
                    , dbname = databaseName
                    , host = options()$mysql$host
                    , port = options()$mysql$port
                    , user = options()$mysql$user
                    , password = options()$mysql$password)
    
    colleges <- tbl(db, "colleges") %>% collect()
    dbDisconnect(db)
    return(colleges)
}
# ------------------------------------------------------------- #
write_strategy <- function(c_id, s_name, s_description)
{
    db <- dbConnect(MySQL()
                    , dbname = databaseName
                    , host = options()$mysql$host
                    , port = options()$mysql$port
                    , user = options()$mysql$user
                    , password = options()$mysql$password)
    
    #s_name <- "test name"
    #s_description <- "description blah blah"
    t_df <- data.frame(strategy_id = round(runif(1, 1, 90000000),0)
                       , strategy_name = s_name
                       , strategy_description = s_description
                       , college_id = c_id
                       , active_strategy = "y")
    
    t_df <- t_df %>% mutate_if(is.factor, as.character)
    table <- "strategy"
    query <- sprintf("INSERT INTO %s (%s) VALUES ('%s')"
                        , table
                        , paste(names(t_df), collapse = ", ")
                        , paste(t_df, collapse = "', '"))
    
    dbGetQuery(db, query)
    dbDisconnect(db)
}
#temp <- load_nav_bar_menu()
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #
strategy_load <- function(failed = FALSE) 
{
    modalDialog(
          size = "m"
        , easyClose = T
        , fade = T
        , title = uiOutput("collegeTitle")
        , uiOutput("new_title")
        , uiOutput("new_description")
        #, uiOutput("theCalender")
        , 
        if (failed)
            div(tags$b(" ", style = "color: red;")),
        
        footer = tagList(
           # h6(textOutput("versionText")),
            #modalButton("Cancel"),
            actionButton("strategy_ok", "OK")
        )
    )
}
# ------------------------------------------------------------- #
# ------------------------------------------------------------- #