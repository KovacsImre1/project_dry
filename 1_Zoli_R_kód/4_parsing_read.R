#KDD2B - Kovács Imre
#beolvassa az egyedi blogbejegyzes hyperlnikeket adatbazisbol Iminek

rm(list = ls()) #deleting the memory
# setwd("D:/KRE/Projekt/rawdata") #munkakonyvtar

#install.packages("rvest")
#install.packages("xml2")
#install.packages("Rcrawler")
#install.packages("httr")
#install.packages("xml2")
#install.packages("DBI")
#install.packages("RMariaDB")

system("java -version")
library("xml2")
library("Rcrawler")
library("rvest")
library("httr")
library("DBI")
library("RMariaDB")

dbcon <- DBI::dbConnect(drv = RMariaDB::MariaDB() , host = "139.162.172.118" , port = "3306" , dbname="bricoler_bloghu" , user = "bricoler_imre2", password = "imre2imre2" ) #csatlakozas Imi masok userenek adataival, IP-vedett az adatbazis

if (dbIsValid(dbcon)=="TRUE")
{
  #dbListTables(dbcon) #tablak nevei
  sqlqry1 <- dbSendQuery(dbcon, "SELECT DISTINCT hyperlink FROM blogbejegyzesek WHERE oszlopnev=?", list("asd")) #adatok lekerese a blogbejegyzesekbol
  #dbGetQuery - használunk, akkor az nem csak elküldi, hanem Fetcheli is.
	blogbejegyzesek1 <- dbFetch(sqlqry1) #aktualis blogbejegyzesek hyperlinkek adattablaba kiirasa
	dbClearResult(sqlqry1) #sql lekeres torlese, dbGetQuery-nel nem kell hasznalni!
	dbDisconnect(dbcon) #kapcsolat megszakitasa
}

dim(blogbejegyzesek1) #2020.11.12 :: [1] 110 690      1
