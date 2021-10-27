#read in required packages
require(data.table)
library(dplyr)
getwd()
#create a list of the files from your target directory
list.files()


#Adatok egyesitese
file_list <- list.files(path = "./1_szal/my_data" , pattern = "my_data_")
file_list <- list.files(pattern = "my_data_")
file_list
file_list <- file_list[c(16,22)]
#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset <- data.frame()

#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- readRDS(file_list[i]) #read in files using the fread function from the data.table package
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}
ido <- format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
saveRDS(dataset, file = paste0("vegleges_1_", ido, ".rds"))

#############################################################
#indexek egyesitese
file_list <- list.files()
file_list <- list.files(path = "./1_szal/my_data" , pattern = "my_index_")
file_list <- list.files(pattern = "my_index_")
#initiate a blank data frame, each iteration of the loop will append the data from the given file to this variable
dataset2 <- data.frame()
#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- readRDS(file_list[i]) #read in files using the fread function from the data.table package
  dataset2 <- rbindlist(list(dataset2, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}
saveRDS(dataset2, file = paste0("vegleges_index_1_", ido, ".rds"))





#Utolagosan letoltott adatok beillesztese az adatbazisba
#adat potlas
kicsi1 <- vegleges_1[1:10,]
kicsi1[1,6] <- kicsi1[2,6]

hely <- NULL
length(which(nchar(vegleges_1$szoveg)==0 | vegleges_1$szoveg == 'none' | vegleges_1$szoveg == 'none_999'))
uj_adat <- dataset[-1,] #potlas
vegleges_1 #eredeti adatbazis

for(i in 1:length(uj_adat[,1])){
  hely <- which(vegleges_1[,1] %in%  uj_adat[i,1])
  vegleges_1[hely, 6] <- uj_adat[i,6]
}


###############################################################
setwd("C:/Users/imre9/Desktop/egyesites/Potlas0411/mydata")
list.files(pattern = "my_data_")
file_list <- list.files(pattern = "my_data_")

dataset <- data.frame()
#had to specify columns to get rid of the total column
for (i in 1:length(file_list)){
  temp_data <- readRDS(file_list[i]) #read in files using the fread function from the data.table package
  dataset <- rbindlist(list(dataset, temp_data), use.names = T) #for each iteration, bind the new data to the building dataset
}
View(dataset)
length(which(nchar(dataset$szoveg)==0 | dataset$szoveg == 'none' | dataset$szoveg == 'none_999'))
#saveRDS(dataset, file = "my_data_potlas.rds")
saveRDS(vegleges_1, file = "vegleges_2_potlassal.rds")

length(vegleges_1$szoveg)
View(vegleges_1[-1,])
length(unique(vegleges_1$blogcim))


require(stringr)
unlist(vegleges_1$szoveg)
str_count("one,   two three 4,,,, 5 6", "\\S+")
sum(str_count(vegleges_1$szoveg, "\\S+"), na.rm = TRUE)

library(stringr)
?str_split_fixed()
datomok <- str_split_fixed(vegleges_1$datum, "/", 3)
typeof(datomok)
vegleges_2_datumok <- cbind(vegleges_1, datomok) 

View(vegleges_2_datumok)
ggplot()
sum(vegleges_2_datumok$"1" == 2019 & vegleges_2_datumok$"2" == 2019)
sum(vegleges_2_datumok$"1" == 2020 )

library(dplyr)
evhonap <- 
  vegleges_2_datumok %>%
  group_by(vegleges_2_datumok[,11], vegleges_2_datumok[,12]) %>% 
  summarize((count =  n()))
ggplot()
View(evhonap)
plot()
barplot(evhonap,xlab,ylab,main, names.arg,col)

library(tidyverse)
library(ggplot2)
df.evhonap <- as.data.frame(evhonap)
View(df.evhonap)

df.evhonap %>% 
  rename(
    df.evhonap[,1] = ev,
    df.evhonap[,2] = honap,
    df.evhonap[,3] = szam
    )
df.evhonap
names(df.evhonap) <- ev
ggplot(df.evhonap, aes(x = vegleges_2_datumok[, 12], y = (count = n())))+
  geom_bar(stat = "identity")

install.packages("writexl")
library("writexl")
write_xlsx(df.evhonap,"dfevhonap.xlsx")


a <- list(c("asd", "csd"),c(as.character()))
which(a == "character(0)")
#hunspell stem probléma

j <- 1
for(i in 107781:107782){
  tryCatch({
      
        hunspell_stem(adat2$word[i], dict = dictionary2)
      
      },
      error = function(e){
        print("ffadfasda")
        hibasorszam <- append(hibasorszam, as.numeric(i))
        base::message('Caught an error!')
        #cat(paste(e), adat2$word[i], "\n Szo sorszama:",i,"\n", as.character(Sys.time(),"\n"), fill=FALSE)
        
        
      },
      warning = function(w){
        base::message('Caught a warning!')
        cat(paste(w),"\n",adat2$word[i], "\n",i,"\n", as.character(Sys.time(),"\n"))
      }
      ,
      finally = {
        # message('Kész \n', hyperlink1[i])
  }
  ) 
}

#hibas szavak kiszedese stem elott
hibasorszam_adat2 <- numeric()
for(i in 107781:length(adat2$word)){
asd <- try(

hunspell_stem(adat2$word[i], dict = dictionary2)

)
if(class(asd)== "try-error"){
  hibasorszam_adat2 <- append(hibasorszam_adat2 , i)
  cat(i , "\n", adat2$word[i])
}
}
adat3 <- adat2[-hibasorszam_adat2, ]

class(asd)== "try-error"
getwd()
save()
hibasorszam_adat2
saveRDS(hibasorszam_adat2, file = "rossz_stem_szavak.rds")
View(R_hunspell_stem)
