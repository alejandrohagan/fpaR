

## load libraries
library(tidyverse)


library(DBI)
diamonds |> colnames()
con <- DBI::dbConnect(duckdb())

## use this to create tables in your databe -- this will be empty until you insert values into it

dbExecute(con, "CREATE TABLE items ( cut VARCHAR, x INTEGER, y INTEGER)")

DBI::dbCreateTable(con,name = "diamonds",diamonds)

DBI::dbListTables(con)

# insert two items into the table

##manually like this -- you can create a statemen that you continous update
rs <-  dbSendStatement(con, "INSERT INTO items VALUES ('jeans',  1,1), ('hammer', 2,1)")

## or you can use place holder value 
dat <- diamonds |> select(cut,x,y)



## or you can put places holders and insert lists or unnamed datframe to the place holders
### table is reserved word so you need to escape it
### this updates the databse but just doesn't reutrn any results

rs <- dbSendStatement(con, "INSERT INTO items (cut,x,y) VALUES (?,?,?)",params=dat |> unname())
dbSendStatement(con, "INSERT INTO diamonds (carat,cut,color,clarity,depth,\"table\",price,x,y,z) VALUES (?,?,?,?,?,?,?,?,?,?)",params=diamonds |> unname())

## or use a statement placeholder and use dbBind to update

stmt <- dbSendStatement(con, "INSERT INTO items VALUES (?, ?,?)")

dbBind(stmt, list('iphone', 300, 2))

dbBind(stmt, list('test', 3.5, 1))

## another example of dynamically passint args and returns a dataframe

dbGetQuery(con, "SELECT cut,x,y FROM items WHERE y < ? AND cut=?", list(10,'Good'))

## clear statement with dbClearResult

dbClearResult(rs)

#---- dbFetch is used after dbi query to return the query
## dbGetRowcount follows dbFetch to get the total rows that were returned 
## dbsendQuery doesn't execute the satement
rs <- dbSendQuery(con, "SELECT* from diamonds ") 
rs |> dbFetch()
## dbFetch returns the returned results
dbGetRowCount(rs)
dbGetQuery(con, "SELECT price FROM diamonds WHERE value < ?", list(200))
dbGetQuery(con, "SELECT * FROM diamonds")
dbGetQuery(con, "SELECT * FROM items")

## Can also delete a table with dbSend Statement

dbSendStatement(con, "Drop table items")


dbGetQuery(con,sql("select * from iris limit 100"))

demo("is.things")
