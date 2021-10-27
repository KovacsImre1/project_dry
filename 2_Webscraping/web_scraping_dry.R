library(netstat)
library(rvest)
library(stringr)
library(RSelenium)
library(httr)
library(gmailr)
library(dplyr) 


#RSelenium szervez lezarasa
#Ha valamiert manualisan kell ujrainditani a webbrowsert
# remDr$close()
# system("taskkill /im java.exe /f", intern=FALSE, ignore.stdout=FALSE)
# pingr::ping_port("localhost", 4444)

#Helyek megadasa
hanyadik1 <- 1
hanyadik2 <- 1
email_hanyadik <- "First"
getwd()

error_csv_path <- paste0("./Errors/Errors_",hanyadik1,".csv")
data_path <-paste0(hanyadik1,"_szal/my_data/") 
index_path <- paste0(hanyadik1,"_szal/index/")
my_data <- paste0("my_data_",hanyadik2,"_")
my_index <- paste0("index_",hanyadik2,"_")
nincs_szoveg <- paste0("./Errors/nincs_szoveg_",hanyadik1,".csv")

#Mappak letrehozasa az adatok tarolasahoz
#Csak egyszer szukseges
# for(i in hanyadik1){
#   dir.create(paste0("./",i,"_szal"))
#   dir.create(paste0("./",i,"_szal/my_data"))
#   dir.create(paste0("./",i,"_szal/index"))
# }

#Mappák létrehozása a htmlek tarolasahoz
# for(i in 2019:2020){
#   dir.create(paste0('./HTMLs/',i))
# 
#   for(j in 1:9)
#     dir.create(paste0('./HTMLs/',i, '/0', j,'/'))
#   for(j in 10:12)
#     dir.create(paste0('./HTMLs/',i, '/', j,'/'))
# }

#RSelenium
fprof <- makeFirefoxProfile(list(args = list('--headless')))
# now add this to new list
exCap <- list(firefox_profile = fprof$firefox_profile, 
              "moz:firefoxOptions" = list(args = list('--headless')))
port <- 4444L
# and use it here
rD <- rsDriver(browser = "firefox", port = port, extraCapabilities = exCap)
remDr <- rD$client
remDr$setTimeout(type = "implicit", milliseconds = 200000)
remDr$setTimeout(type = "page load", milliseconds = 400000)

#blog címek beolvasása melyeket Zoli gyujtott le
urls <- read.csv("bloghu1920.csv", header = T)
#az urlek egy reszenek vagy egeszenek kivalasztasa
hyperlink1 <- urls[1:180000,]

#Amennyiben ujra futtatnank a kodot azokon a bejegyzeseken, ahol nem talalt a script torzsszoveget
# empty_szoveg <- readRDS("empty_szoveg.rds")
# hyperlink1 <- empty_szoveg[1:20000]

urls <- NULL
rm(urls)

#e-mail alert, ha vegig futott a kod
#credentials.json szukseges hozza
email_to <- #please insert your e-mail adresse"
email_from <- #please insert your e-mail adresse"

#https://github.com/r-lib/gmailr
#gmail sender set up
gm_auth_configure(path ="credentials.json")
test_email <-
  gm_mime() %>%
  gm_to(email_to) %>%
  gm_from(email_from) %>%
  gm_subject(paste0(email_hanyadik ," running over")) %>%
  gm_text_body(paste0("The run of the ", email_hanyadik ," thread has been stopped!"))

# Verify it looks correct
#gm_create_draft(test_email)
# If all is good with your draft, then you can send it
gm_send_message(test_email)

#######################
#######Functions#######
#######################
{
    #Hiba kezeles
  hiba <- function(hiba_uzenet, elhelyezes){
    cat(unlist(t(c(hyperlink1[i], as.character(Sys.time()),hiba_uzenet))), file = elhelyezes,sep = ";", append = T, fill=FALSE)
    cat(c("\n",hyperlink1[i],"\n", as.character(Sys.time()),"\n",hiba_uzenet,"\nURL sorszáma:",i,"\n"))
  }
  
  ###Sablon blog func
  cim_fugg <- function(html){
    cim <- html %>%
      html_nodes("title") %>%
      html_text(trim = TRUE)
      cim <- paste(cim)
      cim <- str_match(cim,'(.*?)-')
      cim <- cim[1]
      if(nchar(cim)==0){
        cim <- "none_888"
      }
    return(cim)
    }
  
  
  ###Caffee sablongoz tartozo fuggevenyek
  {
  
  #4Coffee kommment
    komment_fugg <- function(html){
      komment_szam <- (html %>%
                         html_nodes(".comment-container"))
      komment_szam
      if(length(komment_szam) != 0){
        return(length(komment_szam))
      } else if(length(html %>% html_nodes(".no-comments-yet")) != 0){
        komment_szam <- NULL
        return(length(komment_szam))
      }else{
        komment_szam <- "Nem lehet"
        return(komment_szam)}
      
      return(length(komment_szam))
    }
  
  #1szerzo
  szerzo_fugg <- function(x){
    szerzo2 <- x %>%
      html_nodes(".author a") %>%
      html_text(trim = TRUE)
    if(nchar(szerzo2) == 0){
      szerzo2 <- "none"
    }
    return(paste(szerzo2))
  }
  
  #3szoveg 
  szoveg_fugg <- function(x){
    szoveg1 <- x %>%
      html_nodes(".coffee_post_entry") %>%
      html_nodes("p") %>%
      html_text(trim = TRUE)
    
    szoveg1 <- str_remove_all(szoveg1, "\\r")
    szoveg1 <- str_remove_all(szoveg1, "\\n")
    szoveg1 <- paste(szoveg1, collapse='')
    
    if(nchar(szoveg1)==0) {
      szoveg1 <- x %>%
        html_nodes(".coffee_post_entry") %>%
        html_text(trim = TRUE)
    }else{
      return(paste(szoveg1))
    }
    
    if(nchar(szoveg1)==0) { szoveg1 <- "none_999" 
    base::message(c(hyperlink1[i],"\n", as.character(Sys.time()),"\nNincs szoveg\n","URL sorszáma:",i,"\n"))
    cat(unlist(t(c(hyperlink1[i], as.character(Sys.time()),"Nincs szoveg\n"))), 
        file = nincs_szoveg ,sep = ";", append = T, fill=FALSE)
    return(paste(szoveg1))
    }else{
      szoveg1 <- str_remove_all(szoveg1, "\\r")
      szoveg1 <- str_remove_all(szoveg1, "\\n")
      szoveg1 <- paste(szoveg1, collapse='')
      return(paste(szoveg1))
    }
  }
  
  #5van-e link a bejegyzesben
  linkek_boolean_fugg <- function(x){
    linkek <-x %>%
      html_nodes(".coffee_post_entry a") %>%
      html_attr("href")
    
    if(length(linkek) != 0){
      link_boolean <- TRUE
    } else {link_boolean <- FALSE}
    return(link_boolean)
  }
  
  #6vannak-e kepek a bejegyzesben
  kepek_fugg <- function(html){
    kepek <- html %>%
      html_nodes(".coffee_post_entry img") %>%
      html_attr("src")
    
    if(length(kepek) != 0){
      kepek_boolean <- TRUE}
    else {kepek_boolean <- FALSE}
    
    return(kepek_boolean)
  }  
  }
}
#######################
#######Functions#######
##########End##########
#######################




#Meta adatok kinyerése
#Ev
#Honap
#cim
#
{#pattern <- "https:\\/\\/(.+).blog.hu\\/((2019|2020)\\/(\\d+)\\/\\d+)"
  pattern <- "https:\\/\\/(.+).hu\\/((2019|2020)\\/(\\d+)\\/\\d+)"
  pattern
  ev_boolean <- hyperlink1 %>%
    str_detect(pattern)
  ev_boolean
  
  blogadat <- hyperlink1 %>%
    str_match(pattern)
  blogadat[1,]
  
  blogcim <- blogadat[,2]
  blogcim
  blogadat[,2]
  datum<- blogadat[1,3]
  datum
  
  ev <- blogadat[1,4]
  
  honap <- blogadat[1,5]
  honap
}


#Üres data.framek letrehozasa, melyben az adatok eltarolasra kerulnek
df_html <- data.frame(hyperlink1=NA, hely=NA, html_fajl_nev=NA)
bejegyzes.dt <- data.frame(hyperlink1=NA,blogcim=NA, datum=NA, szerzo=NA, cim=NA
                           , szoveg=NA, olvasottsag=NA, komment_szam=NA
                           , link_boolean=NA, kepek_boolean=NA)

#sleeptime 5000 oldal letöltése után
sleeptime <- 0
sorokszama <- 0
tablakszama <- 1

#Amennyiben manualisan ujra kellene inditani az utoljara letarolt adatponttol
# ee1 <- readRDS("./1_szal/my_data/my_data_1_6_2021_03_18_15_58_14.rds")
# hossz <- which(hyperlink1 %in% tail(ee1[,1], 1)) + 1
# hossz

{
  for(i in 1:length(hyperlink1)){
    tryCatch(
      expr = {
        {#html betoltese
          
          #Ha nem tudjuk beolvasni a linket, akkor ujra inditja a RSeleniumot
          if(class(try(remDr$navigate(hyperlink1[i]))) == "try-error"){

            pid <- rD$server$process$get_pid()
            system(paste0("Taskkill /F /T" ," /PID ", pid))
            Sys.sleep(3)            
            # and use it here
            rD <- rsDriver(browser = "firefox", port = port, verbose=TRUE,check = FALSE, extraCapabilities = exCap)
            remDr <- rD$client
            remDr$setTimeout(type = "implicit", milliseconds = 200000)
            remDr$setTimeout(type = "page load", milliseconds = 400000)
            Sys.sleep(3) 
            remDr$navigate(hyperlink1[i])
          }
          html <- read_html(remDr$getPageSource()[[1]])
          
          #ellenorzes
          if(!grepl(".blog.hu",hyperlink1[i])){
            hiba("1_Blog.hu - hianyzik\n",
                 error_csv_path)
            next
          }
          
          if(class(try(http_status(GET(as.character(hyperlink1[i])))$message))=="try-error"){
            hiba("2_try-error\n",
                 error_csv_path)

            next
          }
          
          if(http_status(GET(as.character(hyperlink1[i])))$message != "Success: (200) OK"){
            hiba(paste(as.character(http_status(GET(as.character(hyperlink1[i])))$message),"\n"),
                 error_csv_path)
           
            next
          }
          
          if(grepl("Blog.hu - nyit",html)){
            hiba("4_Blog.hu_nyito\n",
                 error_csv_path)
            
            next
          }
          
          if(grepl("adult_css",html)){
            hiba("5_adult\n",
                 error_csv_path)
            
            next
          }
          
          if(grepl("<title>Index -",html)){
            hiba("6_index_nyito\n",
                 error_csv_path)
            next
          }
          
          
          #datum
          datum<- blogadat[i,3]
          if(length(datum)==0) { datum <- "none" }
          
          #blogcim
          blogcim <- blogadat[i,2]
          if(length(blogcim)==0) { datum <- "none" }
          
          #olvasottsag: post_id_counter
          olvasottsag <- html %>%
            html_nodes(xpath = '//*[@id="post_id_counter"]/text()') %>%
            html_text()
          if(length(olvasottsag)==0) { olvasottsag <- "NA" }
          
          if(length(html %>%
                    html_nodes(".coffee_post_entry")) != 0){
            szerzo <- szerzo_fugg(html)
            cim <- cim_fugg(html)
            szoveg <- szoveg_fugg(html)
            komment_szam <- komment_fugg(html)
            link_boolean <- linkek_boolean_fugg(html)
            kepek_boolean <- kepek_fugg(html)
          }
          else{

            #1szoveg, kulonbozo tarolokban lehetnek a sablontol fuggoen  
            if(grepl("entry", html)){
              szoveg <- html %>%
                html_nodes(".entry") %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)
            }else if(length(html %>%
                            html_nodes(".post")) != 0){
              szoveg <- html %>%
                html_nodes(".post") %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)
            }else if(grepl("post-body", html)){
                szoveg <- html %>%
                  html_nodes(".post-body") %>%
                  html_nodes("p") %>%
                  html_text(trim = TRUE)
            }else if(grepl("post_contenar", html)){
                  szoveg <- html %>%
                    html_nodes(".post_contenar") %>%
                    html_nodes("p") %>%
                    html_text(trim = TRUE)
            }else if(grepl("content-body", html)){
              szoveg <- html %>%
                html_nodes(".content-body") %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)
            }else if(grepl("content", html)){
              szoveg <- html %>%
                html_nodes(xpath = '//*[@id="content"]') %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)  
            }else if(grepl("entry-content", html)){
              szoveg <- html %>%
                html_nodes("entry-content") %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)   
            }else{
              szoveg <- html %>%
                html_nodes(".post_content") %>%
                html_nodes("p") %>%
                html_text(trim = TRUE)
            }

            szoveg <- str_remove_all(szoveg, "\\r")
            szoveg <- str_remove_all(szoveg, "\\n")
            szoveg <- paste(szoveg, collapse='')

            if(nchar(szoveg)==0) {
              if(grepl("entry", html)){
                szoveg <- html %>%
                  html_nodes(".entry") %>%
                  
                  html_text(trim = TRUE)
              }else if(length(html %>%
                              html_nodes(".post")) != 0){
                szoveg <- html %>%
                  html_nodes(".post") %>%
                  
                  html_text(trim = TRUE)
              }else if(grepl("post-body", html)){
                szoveg <- html %>%
                  html_nodes(".post-body") %>%
                  html_text(trim = TRUE)
              }else if(grepl("post_contenar", html)){
                szoveg <- html %>%
                  html_nodes(".post_contenar") %>%
                  html_text(trim = TRUE)
              }else if(grepl("content-body", html)){
                szoveg <- html %>%
                  html_nodes(".content-body") %>%
                  html_text(trim = TRUE)
              }else if(grepl("content", html)){
                szoveg <- html %>%
                  html_nodes(xpath = '//*[@id="content"]') %>%
                  html_text(trim = TRUE)
              }else if(grepl("entry-content", html)){
                szoveg <- html %>%
                  html_nodes("entry-content") %>%
                  html_text(trim = TRUE) 
              }else{
                szoveg <- html %>%
                  html_nodes(".post_content") %>%
                  html_text(trim = TRUE)
              }
              
            }

            szoveg <- str_remove_all(szoveg, "\\r")
            szoveg <- str_remove_all(szoveg, "\\n")
            szoveg <- str_remove_all(szoveg, "\\t")
            szoveg <- paste(szoveg, collapse='')
            
            #Ha nem talaltunk szoveges tartalmat a bejegyzesben
            if(nchar(szoveg)==0) { szoveg <- "none_999"
            
            base::message(c(hyperlink1[i],"\n", as.character(Sys.time()),"\nNincs szoveg\n","URL sorszáma:",i,"\n"))
            cat(unlist(t(c(hyperlink1[i], as.character(Sys.time()),"Nincs szoveg\n"))), 
                file = nincs_szoveg ,sep = ";", append = T, fill=FALSE)
            
            }
            
            #2szerzo
            szerzo <- html %>%
              html_nodes(".author a") %>%
              html_text(trim = TRUE)
            szerzo <- paste(szerzo)
            
            if(length(szerzo)==0) { szerzo <- "none" }
            
            
            #3cim
            cim <- cim_fugg(html)
            
            #4kommentek szama
            komment_szam <- komment_fugg(html)
            
            #5van-e link a bejegyzesben
            linkek <-html %>%
              html_nodes(".entry a") %>%
              html_attr("href")
            
            link_boolean <- NULL
            if(length(linkek) != 0){
              link_boolean <- TRUE
            } else {link_boolean <- FALSE}
            
            #6vannak-e k?pek a bejegyz?sben
            kepek <- html %>%
              html_nodes(".entry img") %>%
              html_attr("src")
            
            kepek_boolean <- NULL
            if(length(kepek) != 0){
              kepek_boolean <- TRUE}
            else {kepek_boolean <- FALSE}
          }
          
          #hyperlink <- hyperlink1[i]
          bejegyzes.dt <- rbind(bejegyzes.dt, list(hyperlink1[i] ,blogcim, datum, szerzo
                                                   , cim, szoveg, olvasottsag, komment_szam
                                                   , link_boolean, kepek_boolean))
          
          #HTML kiirasa
          html_fajl_nev <- paste0(blogcim,'_', i,'_', format(Sys.time(),"%Y_%m_%d_%H_%M_%S"))
          
          #hely
          hely <- paste0('./HTMLs/',blogadat[i,4], '/', blogadat[i,5] , '/')
          
          #HTML MENTESE
          write_html(html,
                     file=paste0(hely,html_fajl_nev,'.html'),
                     quote = FALSE,
                     col.names = FALSE,
                     row.names = FALSE)


          #csatolofajlhoz iras RDS
          df_html <- rbind(df_html,
                           list(hyperlink1[i], hely, html_fajl_nev))

          sleeptime <- sleeptime+1
          if(sleeptime == 5000){
            Sys.sleep(10)
            sleeptime <- 0}
          
          sorokszama <- sorokszama+1
          cat("\nKesz:",i)
          if(sorokszama == 10000){
            #tablamutato
            ido <- format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
            
            file1 <- paste0(data_path, my_data, tablakszama,"_", ido,".rds")
            saveRDS(bejegyzes.dt, file = file1)
            cat("my_data", tablakszama, format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), "\n")
            file2 <- paste0(data_path, my_index, tablakszama,"_",ido,".rds")
            saveRDS(df_html, file =  file2)
            
            sorokszama <- 0
            tablakszama <- tablakszama+1
            
            #Üres data.framek
            df_html <- NULL
            df_html <- data.frame(hyperlink1=NA, hely=NA, html_fajl_nev=NA)
            
            bejegyzes.dt <- NULL
            bejegyzes.dt <- data.frame(hyperlink1=NA,blogcim=NA, datum=NA, szerzo=NA, cim=NA
                                       , szoveg=NA, olvasottsag=NA, komment_szam=NA
                                       , link_boolean=NA, kepek_boolean=NA)
            base::message(file1)
            
            pid <- rD$server$process$get_pid()
            system(paste0("Taskkill /F /T" ," /PID ", pid))
            Sys.sleep(3)
            # and use it here
            rD <- rsDriver(browser = "firefox", port = port, verbose=TRUE ,check = FALSE, extraCapabilities = exCap)
            remDr <- rD$client
            remDr$setTimeout(type = "implicit", milliseconds = 200000)
            remDr$setTimeout(type = "page load", milliseconds = 400000)

            # rD <- rsDriver(browser="firefox", port=4444L, verbose=F, extraCapabilities = fprof)
            # remDr <- rD[["client"]]
            
          }
        }
        
      },
      error = function(e){
        base::message('Caught an error!')
        cat(paste(e), hyperlink1[i], "\n URL sorszáma:",i,"\n", as.character(Sys.time(),"\n"), fill=FALSE)
        cat(unlist(t(c(hyperlink1[i], as.character(Sys.time()),paste0(e)))), file = error_csv_path,sep = ";", append = T, fill=FALSE)
        Sys.sleep(2)
        #a write fuggveny jobb lehet, mert a cat neha nem kezdi ujsorban a logolast
        #write(unlist(t(c(hyperlink1[i], as.character(Sys.time()),paste0(e)))) , file = "Errors.csv",sep = ";", append = T, fill=FALSE)
      },
      warning = function(w){
        base::message('Caught a warning!')
        cat(paste(w),"\n", hyperlink1[i], "\n",i,"\n", as.character(Sys.time(),"\n"))
        cat(unlist(t(c(hyperlink1[i], as.character(Sys.time()),w[1]))), file = error_csv_path,sep = ";", append = T, fill = FALSE)
        #a write fuggveny jobb lehet, mert a cat neha nem kezdi ujsorban a logolast
        #write(unlist(t(c(hyperlink1[i], as.character(Sys.time()),paste0(e)))) , file = "Errors.csv",sep = ";", append = T, fill=FALSE)
      }
      ,
      finally = {
        message('Done \n', hyperlink1[i])
      }
    ) 
    
    
    
  }
  #UTOLSO KIIRAS
  cat("Vége:", as.character(Sys.time()), "\n")
  ido2 <- format(Sys.time(),"%Y_%m_%d_%H_%M_%S")
  file1_last <- paste0(data_path,my_data, "last_",ido2,".rds")
  saveRDS(bejegyzes.dt, file = file1_last)

  file2_last <- paste0(index_path, my_data, "last_",ido2,".rds")
  saveRDS(df_html, file = file2_last)

  gm_send_message(test_email)
}

