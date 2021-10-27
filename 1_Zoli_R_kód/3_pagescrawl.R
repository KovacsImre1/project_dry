#KDD2
#blogbejegyzesek leszedese a blog root cimekbol a blog root sitemap.xml/bol

rm(list = ls()) #deleting the memory
setwd("D:/KRE/Projekt/rawdata") #munkakonyvtar

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

#blogok kezdooldalanak beolvasasa
#load("blgcmk1_317.Rdata")
#blgcmk2<-unname(as.character(blgcmk1$pages)) #csak a blogcimek kinyerese
#l_blgcmk2 <- length(blgcmk2) #blogcimekszama -- data.frame(pages=as.character("blogcim_https"), lekerve=Sys.time(), userid=0, buserd=0)

today1<-Sys.Date()
blgcmk1 <- read.csv("blogrootpgs99.csv", header=TRUE, as.is=T, sep=";", fileEncoding = "UTF-8")
blgcmk2<-unname(as.character(blgcmk1$link))
l_blgcmk2 <- length(blgcmk2)
# blgcmk2[1726] <- "https://lelekangyal.blog.hu/"

#csatlakozas Imi masok userenek adataival, IP-vedett az adatbazis
dbcon <- DBI::dbConnect(drv = RMariaDB::MariaDB() , host = "139.162.172.118" , port = "3306" , dbname="bricoler_bloghu" , user = "bricoler_imre2", password = "imre2imre2" )

if (dbIsValid(dbcon)=="TRUE") #ellenorzes, hogy jo-e a kapcsolat
{

	for (i in c(2870:l_blgcmk2)) #vegigfutunk a blogcimeken, blgcmk1[2...vegig]
	{
		cat("----",i,"----",blgcmk2[i],"----","\n") #visszajelzes, hogy hol jarunk

		if ((http_status(GET(as.character(blgcmk2[i])))$message!="Success: (200) OK"))
			{cat(blgcmk2[i], "\n", file = "nem200asoldal.csv", append=TRUE)}
			else
		if (grepl("Blog.hu - nyitó",as.character(read_html(blgcmk2[i]))))
		  {cat(blgcmk2[i], "\n", file = "nem200asoldal.csv", append=TRUE)}
			else
		if (grepl("adult_css",as.character(read_html(blgcmk2[i]))))
	    {cat(blgcmk2[i], "\n", file = "adultoldal.csv", append=TRUE)}
	    else
		{
		#Ha letezik a site, nem adultoldal es nem is a Blog.hu nyitooldal, akkor mnezzuk a linkeket

		#Rcrawler(Website = blgcmk2[i] , no_cores = 1, no_conn = 1, Obeyrobots = TRUE, MaxDepth=5, crawlUrlfilter="(.2018.|.2019.|.2020.)", #crawlZoneXPath=c("//archive//",) #user keresenek megfeleloen

		mainxml1 <- as_list(read_xml(paste0(blgcmk2[i],"sitemap.xml"))) #blogbejegyzes fo sitemap.xml ami az xml-ket tartalmazza
		l_mainxml1 <- sum(sapply(mainxml1,length)) #az alxml-ek szama!
		alxml1 <- NULL #alxml-ek listaja, kiolvassa a mainxml-bol az alxml-eket

			for (j in c(1:l_mainxml1))
			{
				alxml1[j]<-as.character(unname(mainxml1$sitemapindex[j]$sitemap$loc[1]))
			}
			#j ciklus vegigfut az alxmleken, beolvassa oket, kinyeri az adatokat, k ciklus pedig beirja oket a webcmk1-be
			l_alxml1 <- sum(sapply(alxml1,length))

			for (j in c(1:l_alxml1))
			#k ciklus az alxml-bol kiolvassa es kiirja
				{
					tempxml1<-as_list(read_xml(alxml1[j])) #az alxml-t beolvassuk listakent
	 				l_tempxml1 <- sum(sapply(tempxml1,length)) #az alxml hossza

					for(k in c(1:l_tempxml1))
					{
					cat(as.character(unname(tempxml1$urlset[k]$url$loc[1])), "\n")
					cat(as.character(unname(tempxml1$urlset[k]$url$loc[1])) , ";" , blgcmk2[i] , ";" , k , ";" , today1 , "\n", file = "blogbejegyzesek18.csv", append=TRUE)

					if (dbIsValid(dbcon)=="TRUE") #ellenorzes, hogy jo-e a kapcsolat (iokozben is megszakadhatott)
					{
						beilleszt1<-data.frame(hyperlink=as.character(unname(tempxml1$urlset[k]$url$loc[1])), blogroot=as.character(unname(blgcmk2[i])), blogentryid=as.numeric(k,5),  retrdate=as.numeric(today1,5))
						sqlqry1 <- dbAppendTable(dbcon, "blogbejegyzesek", beilleszt1) #adatok lekerese a blogbejegyzesekbol
						#valasz1 <- dbFetch(sqlqry1)
						#cat ("dbvalasz a beillsztesre:", valasz1,"\n") #aktualis blogbejegyzesek adattablaba kiirasa
						#dbClearResult(sqlqry1)
					}

				  } #k ciklus vege

	    }

		} #letezik-e a site es nem-felnott tartalmas
	} #i ciklus vege, blogcimeken vegigfutas
dbDisconnect(dbcon) #kapcsolat megszakitasa
}#adatbaziskapcsolat ok-e
