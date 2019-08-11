library(RMySQL)
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)

library(shinymaterial)
# ------------------------------------------------------------- #
#install.packages("golem")
library(golem)
#install.packages("shinymaterial")
library(gt)
library(RJDBC)
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
#dbDisconnect(db)
temp <- load_nav_bar_menu()
# ------------------------------------------------------------- #

