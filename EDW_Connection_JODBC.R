# Load library
library(RJDBC)
library(RMySQL)
library(DBI)
library(rJava)
library(pool)
install.packages("RJDBC",dep=TRUE)





#jdbcDriver <- JDBC("oracle.jdbc.OracleDriver",classPath=paste0(getwd(),"/OJDBC/ojdbc6.jar"))
# myconn  <- dbConnect(jdbcDriver
#                , "jdbc:oracle:thin:@//exa2-scan.its.utas.edu.au:1521/edwdev_maa"
#                , rstudioapi::askForPassword("Database user")
#                , rstudioapi::askForPassword("Database password"))

jdbcDriver <- JDBC(driverClass="com.mysql.cj.jdbc.Driver"
                   ,classPath=paste0(getwd(),"/OJDBC/mysql_connector/mysql-connector-java-8.0.17.jar")
                   , identifier.quote="`")
myconn  <- dbConnect(jdbcDriver, "jdbc:mysql://remotemysql.com:3306/g6Mj2lugZA","g6Mj2lugZA","ipYc79lTd0")


dbDisconnect(db)
poolClose(pool)

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

dbListTables(myconn)
dbListFields(conn, "help_category", "n%")


# Retrieve and list all the tables 
tblist <- dbGetTables(myconn)
View(tblist)


# Retrieve and view one table to test
colleges <- dbReadTable(myconn,"g6Mj2lugZA.colleges")
View(tb_UTAS_MODEL)


# Test to insert table
# dbWriteTable(myconn, 
#         tb_UTAS_MODEL,
#         tablename = "DATA_MART.STG_INITIATIVE_TRACKING", 
#         append = TRUE, 
#         rownames = FALSE)


# Test to retrieve column owner from target table
owner <- dbGetQuery(myconn, "select OWNER from DATA_MART.STG_INITIATIVE_TRACKING")

# Test to delete row from target table
dbGetQuery(myconn, "DELETE FROM DATA_MART.STG_INITIATIVE_TRACKING WHERE STRATEGY = 5")

# Close connection
dbDisconnect(myconn)

