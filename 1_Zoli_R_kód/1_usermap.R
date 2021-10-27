#KDD2
#1. blogsok cimenek leszedese a usermap.xml-bol
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

#1.xml-bol a blogcimek leszedese es tarolasa
#oooooooooooooooooooooooooooooooooooooooooooooo

today1<-Sys.Date()

#mainxml-bol a sok-sok alxml leszedese es tarolasa
usrxml1 <- as_list(read_xml("https://blog.hu/users.xml")) #xml ami az xml-ket tartalmazza
l_mainxml1 <- sum(sapply(usrxml1,length))-1 #az alxml-ek szama, 1100 elvileg (2020.10.15)

alxml1 <- NULL #alxml-ek listaja, kiolvassa a mainxml-bol az alxml-eket
for (i in c(1:l_mainxml1)) {alxml1[i]<-as.character(unname(usrxml1$sitemapindex[i]$sitemap$loc[1]))}

#i ciklus vegigfut az alxmleken, beolvassa oket, kinyeri az adatokat, j ciklus pedig beirja oket file-ba, cattal
for (i in c(1:l_mainxml1))
	{
	cat(i,"\n")
	Sys.sleep(2) #kidob, ha nem varok picit
	tempxml1<-as_list(read_xml(alxml1[i]))
	l_tempxml1 <- sum(sapply(tempxml1,length))-1

			for(j in c(1:l_tempxml1))
			{
			oldalnev1<-as.character(unname(tempxml1$urlset[j]$url$loc[1]))
			lastmod1<-as.character(unname(tempxml1$urlset[j]$url$lastmod[1]))
			#changefreq1<-as.character(unname(tempxml1$urlset[j]$url$changefreq[1]))
			#priority1<-as.character(unname(tempxml1$urlset[j]$url$priority[1]))
			#alxmlid1<-i
			#alxmlnev1<-as.character(alxml1[i])

			cat(as.character(oldalnev1) , ";" , lastmod1 , ";" , i , ";", j , ";" , as.Date(today1), "\n", file = "userpgs.csv", append=TRUE)
			#meg lehet nyitni channelre

    #  cat( as.character(oldalnev1) , ";" , lastmod1 , ";" , as.character(changefreq1) , ";" , as.numeric(priority1) , ";" ,alxmlid1 , ";" , as.character(alxmlnev1) , "\n", file = "userpgs.csv", append=TRUE)
	   	} #j ciklus vege
	} #i ciklus vege
