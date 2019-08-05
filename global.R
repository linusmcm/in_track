library(RMySQL)
library(shiny)
library(tidyverse)
library(dbplyr)
library(bs4Dash)
library(echarts4r)
library(shinyWidgets)
#install.packages("dbplyr")
#install.packages("echarts4r", dependencies = T)
# ------------------------------------------------------------- #
utas_red <<- "#e42312"
utas_black <<- "#000000"
utas_white <<- "#FFFFFF"
buttonStyle <<- "bordered"
buttonSize <<- "md"
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
table <- "colleges"
db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
                port = options()$mysql$port, user = options()$mysql$user, 
                password = options()$mysql$password)
# ------------------------------------------------------------- #
saveData <- function(db, data) {
    # Connect to the database

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
loadData <- function(table) 
{
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    #dbDisconnect(db)
    return(data)
}
# ------------------------------------------------------------- #
loadData(table)
# ------------------------------------------------------------- #

