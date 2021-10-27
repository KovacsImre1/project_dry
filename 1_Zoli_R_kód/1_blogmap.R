#KDD2
#1. blogok cimenek leszedese a sitemap.xmlekbol
#todo: SQL-re atirni

rm(list = ls()) #deleting the memory
setwd("D:/KRE/Projekt/rawdata") #munkakonyvtar

#install.packages("rvest")
#install.packages("xml2")
#install.packages("Rcrawler")
#install.packages("httr")
#install.packages("xml2")

system("java -version")
library("xml2")
library("Rcrawler")
library("rvest")
library("httr")
library(xml2)

#1.xml-bol a blogcimek leszedese es tarolasa
#oooooooooooooooooooooooooooooooooooooooooooooo

#mainxml-bol a sok-sok alxml leszedese es tarolasa
mainxml1 <- as_list(read_xml("https://blog.hu/blogs.xml")) #xml ami az xml-ket tartalmazza
l_mainxml1 <- sum(sapply(mainxml1,length))-1 #az alxml-ek szama!

alxml1 <- NULL #alxml-ek listaja, kiolvassa a mainxml-bol az alxml-eket
for (i in c(1:l_mainxml1)) {alxml1[i]<-as.character(unname(mainxml1$sitemapindex[i]$sitemap$loc[1]))}

#webcmk1: webblogcimek adattablaja
webcmk1 <- data.frame(oldanev=as.character("weboldal_https_cime"),lastmod="-0001-11-30T00:00:00+0100",changefreq=as.character("yearly"),priority=0.1, letoltve=as.character("2020-10-14 13:28:18 CEST"), alxmlid=0, alxmlnev=as.character("alxml_cime_adott_idopontban"))

#i ciklus vegigfut az alxmleken, beolvassa oket, kinyeri az adatokat, j ciklus pedig beirja oket a webcmk1-be
for (i in c(1:l_mainxml1))
	{
	cat(i,"\n")
	Sys.sleep(1.41) #kidob, ha nem varok picit
	tempxml1<-as_list(read_xml(alxml1[i]))
	l_tempxml1 <- sum(sapply(tempxml1,length))-1

			for(j in c(1:l_tempxml1))
			{
			oldalnev1<-as.character(unname(tempxml1$urlset[j]$url$loc[1]))
			lastmod1<-as.character(unname(tempxml1$urlset[j]$url$lastmod[1]))
			changefreq1<-as.character(unname(tempxml1$urlset[j]$url$changefreq[1]))
			priority1<-as.character(unname(tempxml1$urlset[j]$url$priority[1]))
			letoltve1<-as.character(Sys.time())
			alxmlid1<-i
			alxmlnev1<-as.character(alxml1[i])

			webcmk1<-rbind(webcmk1, data.frame(oldanev=as.character(oldalnev1),lastmod=lastmod1,changefreq=as.character(changefreq1),priority=as.numeric(priority1), letoltve=letoltve1, alxmlid=alxmlid1, alxmlnev=as.character(alxmlnev1)))
	   	} #j ciklus vege
	} #i ciklus vege

#file kiirasa ugyesen, Rdata-ban, eros tomoritessel, mert nagy
setwd("D:/KRE/Projekt/rawdata")
save(webcmk1, file="1_webcmk_backup.Rdata", compression_level=9, eval.promises = TRUE, precheck = TRUE)
