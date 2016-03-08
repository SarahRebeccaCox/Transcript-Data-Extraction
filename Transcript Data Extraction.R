#The transcript must be pasted as text. 
#Publishing this to GitHub long after the fact, and it seems my functions for getting the PDF transcripts to text have gone missing :(
#This file will take a group of PDF transcripts, correctly split them by student, and scrape data into a clean table.
#Transcripts must be formatted specifically the way the transcripts at NYU Poly were formatted for this to work smoothly. Modifications can be done for other schools.

transcript <- ""


###############
#Get ID number#
###############

getID <- function(transcript){
  id <- transcript[grep("[N][0-9]",transcript)] #Find any strings 	containing N+number
  id.dataframe <- data.frame(matrix(ncol=1,nrow=length(id)))
  id <- strsplit(id, " ") #Split strings by spaces
  
  for (i in 1:length(id)){
    id1 <- id[[i]][grep("^[N][0-9]{7}",id[[1]])] # Starts with N 	followed by 0-9
    id.dataframe[i,1] <- id1
  }
  
  return(id.dataframe)
}





##########
#Get GPAs#
##########

getGPA <- function(transcript){
  #Setup to get multiple GPAs
  
  #Lines for current gpa
  gpa1 <- transcript[grep("Current",transcript)] 
  gpa1 <- strsplit(gpa1, " ") 
  
  #Lines for cumulative gpa
  gpa2<- transcript[grep("Cumulative",transcript)] 
  gpa2 <- strsplit(gpa2, " ") 
  
  #generate a dataframe to hold all gpas.
  #each row is a semester. col 1 is current gpa, col 2 is cumulative #gpa
  gpa.dataframe <- data.frame(matrix(ncol=2,nrow=0))
  
  
  
  #loop to put all GPAs in the dataframe we just generated
  #gpa1 is current, gpa2 is cumulative
  for (i in 1:length(gpa1)){
    gpa.dataframe1 <- data.frame(matrix(ncol = 2, nrow = 1))
    gpa1[[i]] <- gpa1[[i]][gpa1[[i]]!=""]
    gpa3 <- gpa1[[i]][length(gpa1[[i]])]
    
    x <- gpa1[[i]][grep("^[0-9][^0-9][0-9][0-9][0-9]",gpa1[[i]])]
    if (gpa3 != x){
      gpa3 <- x[length(x)]
    }
    gpa3 <- as.numeric(sub('"', '', gpa3))
    gpa.dataframe1[1,1] <- gpa3
    
    gpa2[[i]] <- gpa2[[i]][gpa2[[i]]!=""]
    gpa4 <- gpa2[[i]][length(gpa2[[i]])]
    
    x <- gpa2[[i]][grep("^[0-9][^0-9][0-9][0-9][0-9]",gpa2[[i]])]
    if (gpa4 != x){
      gpa4 <- x[length(x)]
    }
    
    gpa4 <- as.numeric(sub('",', '', gpa4))
    
    gpa.dataframe1[1,2] <- gpa4
    gpa.dataframe <- rbind(gpa.dataframe,gpa.dataframe1)
  }
  
  return(gpa.dataframe)
}




###########
#Get Ahours#
###########
getAHours <- function(transcript){
  #Setup to get multiple GPAs
  
  
  
  #Lines for cumulative gpa
  hours2<- transcript[grep("Cumulative",transcript)] 
  hours2 <- strsplit(hours2, " ") 
  
  #generate a dataframe to hold all gpas.
  #each row is a semester. col 1 is current ehrs, col 2 is cumulative ehrs
  hours.dataframe <- data.frame(matrix(ncol=1,nrow=0))
  
  #loop to put all hours in the dataframe we just generated
  #hours1 is current, hours2 is cumulative
  for (i in 1:length(hours2)){
    hours.dataframe1 <- data.frame(matrix(ncol = 1, nrow = 0))
    hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+14]
    
    if(hours4==""){
      hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+17]
    }
    if(hours4==""){
      hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+18]
    }
    if(hours4==""){
      hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+16]
    }
    hours4 <- as.numeric(sub('",', '', hours4))
    
    hours.dataframe1[1,1] <- hours4
    hours.dataframe <- rbind(hours.dataframe,hours.dataframe1)
  }
  
  return(hours.dataframe)
}

getAHours(transcript)


###########
#Get Ehours#
###########
getEHours <- function(transcript){
  #Setup to get multiple GPAs
  
  #Lines for current gpa
  hours1 <- transcript[grep("Current",transcript)] 
  hours1 <- strsplit(hours1, " ") 
  
  #Lines for cumulative gpa
  hours2<- transcript[grep("Cumulative",transcript)] 
  hours2 <- strsplit(hours2, " ") 
  
  #generate a dataframe to hold all gpas.
  #each row is a semester. col 1 is current ehrs, col 2 is cumulative ehrs
  hours.dataframe <- data.frame(matrix(ncol=2,nrow=0))
  
  #loop to put all hours in the dataframe we just generated
  #hours1 is current, hours2 is cumulative
  for (i in 1:length(hours1)){
    hours.dataframe1 <- data.frame(matrix(ncol = 2, nrow = 1))
    hours3 <- hours1[[i]][grep("Current",hours1[[i]])+25]
    if(hours3==""){
      hours3 <- hours1[[i]][grep("Current",hours1[[i]])+26]
    }
    if(hours3==""){
      hours3 <- hours1[[i]][grep("Current",hours1[[i]])+27]
    }
    hours3 <- as.numeric(sub('"', '', hours3))
    hours.dataframe1[1,1] <- hours3
    
    
    hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+22]
    
    if(hours4==""){
      hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+20]
    }
    if(hours4==""){
      hours4 <- hours2[[i]][grep("Cumulative",hours2[[i]])+18]
    }
    hours4 <- as.numeric(sub('",', '', hours4))
    
    hours.dataframe1[1,2] <- hours4
    hours.dataframe <- rbind(hours.dataframe,hours.dataframe1)
  }
  
  return(hours.dataframe)
}

getEHours(transcript)



getHours <- function(transcript){
  a <- getAHours(transcript)
  b <- getEHours(transcript)
  c <- cbind(a,b)
  c<- c[with(c, order(c[,1])), ]
  return(c)
}


getHours(transcript)

#######################################
#Put data together for each transcript#
#######################################

getID(transcript)
getGPA(transcript)



#Split the PDF by transcript, split at phrase "End of Graduate Record"
#txtsplit <- strsplit(txt, "End of Graduate Record")

txtsplit2 <- strsplit(transcripts, "Institute of Brooklyn merged to form the Polytechnic Institute of New York")

transcript.data <- data.frame(matrix(ncol=49,nrow=100))
colnames(transcript.data) <- c("N Number","Sem 1 Cur Hours","Sem 1 Total Hours","Sem 1 Cur GPA","Sem 1 Cumul GPA","Sem 2 Cur Hours","Sem 2 Total Hours","Sem 2 Cur GPA","Sem 2 Cumul GPA","Sem 3 Cur Hours","Sem 3 Total Hours","Sem 3 Cur GPA","Sem 3 Cumul GPA","Sem 4 Cur Hours","Sem 4 Total Hours","Sem 4 Cur GPA","Sem 4 Cumul GPA","Sem 5 Cur Hours","Sem 5 Total Hours","Sem 5 Cur GPA","Sem 5 Cumul GPA","Sem 6 Cur Hours","Sem 6 Total Hours","Sem 6 Cur GPA","Sem 6 Cumul GPA","Sem 7 Cur Hours","Sem 7 Total Hours","Sem 7 Cur GPA","Sem 7 Cumul GPA","Sem 8 Cur Hours","Sem 8 Total Hours","Sem 8 Cur GPA","Sem 8 Cumul GPA","Sem 9 Cur Hours","Sem 9 Total Hours","Sem 9 Cur GPA","Sem 9 Cumul GPA","Sem 10 Cur Hours","Sem 10 Total Hours","Sem 10 Cur GPA","Sem 10 Cumul GPA","Sem 11 Cur Hours","Sem 11 Total Hours","Sem 11 Cur GPA","Sem 11 Cumul GPA","Sem 12 Cur Hours","Sem 12 Total Hours","Sem 12 Cur GPA","Sem 12 Cumul GPA")




for (i in 1:length(txtsplit2[[1]])){
  txt3 <- readLines(con=textConnection(as.character(txtsplit2[[1]][i])))
  trim <- function (x) gsub("^\\s+|\\s+$", "", x)
  txt <- trim(txt3)
  # Drop empties
  txt <- txt[txt != " "]
  txt <- txt[txt != ""]
  transcript.document <- txt
  id <- getID(transcript.document)
  gpa.dataframe <- getGPA(transcript.document)
  hours.dataframe <- getHours(transcript.document)
  
  
  transcript.data[i,1] <- id
  transcript.data[i,2] <- hours.dataframe[1,1]
  transcript.data[i,3] <- hours.dataframe[1,2]
  transcript.data[i,4] <- gpa.dataframe[1,1]
  transcript.data[i,5] <- gpa.dataframe[1,2]
  transcript.data[i,6] <- hours.dataframe[2,1]
  transcript.data[i,7] <- hours.dataframe[2,2]
  transcript.data[i,8] <- gpa.dataframe[2,1]
  transcript.data[i,9] <- gpa.dataframe[2,2]
  
  transcript.data[i,10] <- hours.dataframe[3,1]
  transcript.data[i,11] <- hours.dataframe[3,2]
  transcript.data[i,12] <- gpa.dataframe[3,1]
  transcript.data[i,13] <- gpa.dataframe[3,2]
  transcript.data[i,14] <- hours.dataframe[4,1]
  transcript.data[i,15] <- hours.dataframe[4,2]
  transcript.data[i,16] <- gpa.dataframe[4,1]
  transcript.data[i,17] <- gpa.dataframe[4,2]
  
  transcript.data[i,18] <- hours.dataframe[5,1]
  transcript.data[i,19] <- hours.dataframe[5,2]
  transcript.data[i,20] <- gpa.dataframe[5,1]
  transcript.data[i,21] <- gpa.dataframe[5,2]
  transcript.data[i,22] <- hours.dataframe[6,1]
  transcript.data[i,23] <- hours.dataframe[6,2]
  transcript.data[i,24] <- gpa.dataframe[6,1]
  transcript.data[i,25] <- gpa.dataframe[6,2]
  
  transcript.data[i,26] <- hours.dataframe[7,1]
  transcript.data[i,27] <- hours.dataframe[7,2]
  transcript.data[i,28] <- gpa.dataframe[7,1]
  transcript.data[i,29] <- gpa.dataframe[7,2]
  transcript.data[i,30] <- hours.dataframe[8,1]
  transcript.data[i,31] <- hours.dataframe[8,2]
  transcript.data[i,32] <- gpa.dataframe[8,1]
  transcript.data[i,33] <- gpa.dataframe[8,2]
  
  transcript.data[i,34] <- hours.dataframe[9,1]
  transcript.data[i,35] <- hours.dataframe[9,2]
  transcript.data[i,36] <- gpa.dataframe[9,1]
  transcript.data[i,37] <- gpa.dataframe[9,2]
  transcript.data[i,38] <- hours.dataframe[10,1]
  transcript.data[i,39] <- hours.dataframe[10,2]
  transcript.data[i,40] <- gpa.dataframe[10,1]
  transcript.data[i,41] <- gpa.dataframe[10,2]
  
  transcript.data[i,42] <- hours.dataframe[11,1]
  transcript.data[i,43] <- hours.dataframe[11,2]
  transcript.data[i,44] <- gpa.dataframe[11,1]
  transcript.data[i,45] <- gpa.dataframe[11,2]
  transcript.data[i,46] <- hours.dataframe[12,1]
  transcript.data[i,47] <- hours.dataframe[12,2]
  transcript.data[i,48] <- gpa.dataframe[12,1]
  transcript.data[i,49] <- gpa.dataframe[12,2]
  
}