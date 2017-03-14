setwd("C:/Users/jonas.j.torres/Desktop/Initiatives-Projects/FY17/SearchEngine")

#### Create dummy data for testing purposes
rm(list=ls())

#id1 <- 1:8
#namess1 <-c("Jonas Jeremias Torres", "The Unilever Company Inc.", "Fannie Mae Int.", "Oscorp International", "Royal Bank of Scotland","Jonas Jeremias", "The fake Apple Corp", "jonas")

#id2 <- seq(1,12,2)
#namess2 <-c("Nada que ver", "The Unilever Company Inc.", "Fannie Mae Int.", "Jonas Jeremias Torres", "Casino Royal Bank", "Apple")

#sap <- data.frame(ID = id1, NamesUno = namess1, stringsAsFactors = FALSE)
#accenture <- data.frame(ID = id2, NamesDos = namess2, stringsAsFactors = FALSE)
#*******End of dummy objects
#dir()
sap <- read.csv("SAP_GB.csv", header = TRUE, stringsAsFactors = FALSE)
accenture <- read.csv("MMS_SAP_List.csv", header = TRUE, stringsAsFactors = FALSE)
#Start for loop to compare both data frames against each other
sap[,2]<-tolower(sap[,2])
sap[,3]<-tolower(sap[,3])
sap[,4]<-tolower(sap[,4])
sap[,5]<-tolower(sap[,5])

accenture[,2]<-tolower(accenture[,2])
accenture[,3]<-tolower(accenture[,3])
accenture[,4]<-tolower(accenture[,4])
accenture[,5]<-tolower(accenture[,5])

accenture$ID_SAP <- NA
accenture$Matching_Score <- 0
accenture$CountryScore <- 0
accenture$VendorScore <- 0

#sap <- sap[1:500,]
#accenture <- accenture[1:1000,]
names1 <- sap[,3]
names2 <- accenture[,3]

system.time(
    for(i in 1:length(names1)){
        gb <- names1[i]#stores the name of the first list "SAP GB"
        gb <- strsplit(gb, " ")[[1]] #split the name and store as a vector
        temp <- i
        #print(gb)
        #loops through the accenture list names
        for(j in 1:length(names2)){
            
            acc <- names2[j]
            acc <- tolower(acc)
            acc <- strsplit(acc, " ")[[1]]#split the name and store as a vector
            #create a matrix with vector names as row and columns
            classMat = matrix(0, nrow=length(acc), ncol=length(gb), dimnames = list(acc, gb))
            #create a penalty matrix
            penaltyMat <- matrix(0.9,nrow = nrow(classMat), ncol = ncol(classMat))
            loLim <- min(nrow(classMat),ncol(classMat)) #smaller dimension
            hiLim <- max(nrow(classMat), ncol(classMat)) #bigger dimension
            for(q in 1:loLim){
                penaltyMat[q,q] <- 1
            }
            #print("******************************")
            #print("***Classification MAtrix")
            #print(classMat)
            #print("******Penalty Matrix")
            #print(penaltyMat)
            #compare row vs column names
            for (x in 1:nrow(classMat)){
                for (y in 1:ncol(classMat)){
                    #first check the cell where x==y
                    if(nrow(classMat) > ncol(classMat)){# si hay mas filas
                        if (rownames(classMat)[y]==colnames(classMat)[y]){
                            classMat[y,y] <- 1
                            break # if it find a match, it breaks out of the loop(this avoids double counting a work i.e: Johnson & Johnson)
                        }
                    }
                    if(nrow(classMat) < ncol(classMat)){# sino
                        if (rownames(classMat)[x]==colnames(classMat)[x]){
                            classMat[x,x] <- 1
                            break
                        }
                    }
                    # si no encontro nada en las celdas donde i es igual a j, entonces que busque desde el prindicipio
                    # de aca para arriba hasta donde dice first check.. puedo borrar
                    if (rownames(classMat)[x]==colnames(classMat)[y]){
                        classMat[x,y] <- 1
                        break # break out of the loop if a match is found
                        
                    }
                }
            }
            #print("Matrix after scanning row vs columns")
            #print(classMat)
            score <- sum(classMat * penaltyMat)/hiLim #compute matching accuracy
            
            #print(paste("Puntaje: ", score))
            #print("Registro frame1:")
            #print(frame1[temp,])
            #print("Registro fram3:")
            #print(frame2[j,])
            #if the score is higher the the current, then replace it by the new one
            
            if(accenture$Matching_Score[j] <score){
                accenture$Matching_Score[j] <- score
                accenture$ID_SAP[j] <- sap$ID[temp]
                if(accenture$Selling.Country[j] == sap$Country[temp]){
                    accenture$CountryScore[j] <- 1
                } else if (sap$Country[temp]=="countrymissing"){
                    accenture$CountryScore[j] <- .7 # less penalty if country missing in SCM
                } else {
                    accenture$CountryScore[j] <- .5
                }
                
                if(accenture$HighLevel[j]==sap$High.Level[temp]){
                    accenture$VendorScore[j] <- 1
                } else{
                    accenture$VendorScore[j] <- 0.9 #there is a lesser penalty for a Vendor mismatch
                }
                
            }
        }
    }
)


write.csv(accenture, "Partial_Match.csv")