# In this project, I will write a function (named "colley") that takes a year (any between 1961 and 2010)
# as input and calculates the Colley rankings using the matrix method for that year. The function should
# return a data frame with two columns, team name and colley score, and should be ordered from best to
# worst. I will get the data from web and my function should work without any data previously loaded into R.

# colley(v): calculates the Colley rankings using the matrix method for any year between 1961 and 2010
# v: the year to calculate the colley ranking of
# return value: a data frame with team name and colley score ordered from best to worst


colley = function(v){
  
  #Loading data
  v=as.character(v)
  a = read.fwf(paste("http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf","gms.txt",sep=v),c(11,28,3,28,3,27))
  
  
  
  #Modifying data type and names
  #ateam = awayteam, ascore =awayscore, hscore= homescore
  
  names(a) = c("year","ateam","ascore","hteam","hscore","location")
  a$ateam = as.character(a$ateam)
  a$hteam = as.character(a$hteam)
  
  a$ateam = gsub(" ","",a$ateam)
  a$hteam = gsub(" ","",a$hteam)
  
  
  
  #Separating awayteam(team1) and hometeam(team2)
  
  team1 = data.frame(sort(table(a$ateam)))
  team1$Var1 = as.character(team1$Var1)
  team2 = data.frame(sort(table(a$hteam)))
  team2$Var1 = as.character(team2$Var1)
  
  
  
  #Dropping teams played less than 6 times
  #After loops, df only contains team names that played 6 or more games
  
  df=data.frame(name=character()) 
  
  for(i in 1:length(team1$Var1)){
    for(j in 1:length(team2$Var1)){
      if(team1$Var1[i] == team2$Var1[j]){
        if(team1$Freq[i] + team2$Freq[j] > 5 | team1$Freq[i] >5 | team2$Freq[j] >5){
          df = rbind(df,data.frame(name=as.character(team1$Var1[i]), stringsAsFactors = F))
        }
      }
    }
  }

  
  df = unique(df)
  
  
  
  #Dropping games against teams that played fewer than 6 games
  
  newa = data.frame()
  
  for ( i in 1: length(a$ateam)){
    for( j in 1: length(df$name)){
      if(df$name[j] == a$ateam[i]){
        newa = rbind(newa,data.frame(ateam=a$ateam[i],ascore=a$ascore[i],hteam=a$hteam[i],hscore=a$hscore[i],stringsAsFactors = F))
      }
    }
  }
  
  newb = data.frame()
  
  for (i in 1: length(newa$hteam)){
    for(j in 1:length(df$name)){
      if(df$name[j] == newa$hteam[i]){
        newb = rbind(newb, data.frame(ateam=newa$ateam[i],ascore=newa$ascore[i],hteam=newa$hteam[i],hscore=newa$hscore[i]))
      }
    }
  }
  
  
  
  #Dropping ties and counting wins and loses for each team
  
  new = data.frame()
  
  for ( i in 1: length(newb$ateam)){
    if(newb$ascore[i] > newb$hscore[i]){
      new = rbind(new,data.frame(wins=newb$ateam[i],loses=newb$hteam[i]))
    }
    if(newb$ascore[i] < newb$hscore[i]){
      new = rbind(new,data.frame(wins=newb$hteam[i],loses=newb$ateam[i]))
    }
  }
  
  wins = data.frame(table(new$wins))
  loses = data.frame(table(new$loses))
  
  
  
  
  #final contains finalized team names, number of wins, and number of loses
  
  final = data.frame()
  
  for( i in 1:length(wins$Var1)){
    for( j in 1:length(loses$Var1)){
      if(wins$Var1[i]==loses$Var1[j]){
        final = rbind(final, data.frame(teams = wins$Var1[i], wins = wins$Freq[i], loses = loses$Freq[j] ))
      }
    }
  }
  
  
  
  #using hash table to index teams, creating opponents column
  
  final$teams = as.character(final$teams)
  e = new.env()
  for(i in 1:length(final$teams)){
    e[[final$teams[i]]] = i
  }
  
  new$wins = as.character(new$wins)
  new$loses = as.character(new$loses)
  
  list = list()
  c=c()
  
  for(i in 1:length(final$teams)){
    c=c()
    for( j in 1:length(new$wins)){
      if(final$teams[i] == new$wins[j]){
        c[length(c)+1]= e[[new$loses[j]]]
      }
      if(final$teams[i] == new$loses[j]){
        c[length(c)+1]= e[[new$wins[j]]]
      }
      list[[i]] = c
    }
  }
  
  
  
  #Adding opponents to final data frame
  
  final[["opponents"]] = c(list)
  
  
  
  #Time to generate rankings
  #Setting matrix
  
  N = length(final$teams)
  C = matrix(0, nrow= N, ncol= N)
  B = numeric(0)
  
  
  
  #Looping
  for(i in 1:N){
    C[i,i] = 2 + length(final$opponents[[i]])
    B[i] = 1 + ((final[i,2]-final[i,3])/2)
    for(j in 1:N){
      if(i != j){
        C[i,j] = -1*(sum(final$opponents[[i]] == j))
      }
    }
  }
  
  
  
  #Solving linear system
  x = solve(C,B)
  
  #New data frame with solution
  solution = data.frame("Team"=final[,1],"Score"=x)
  solution = solution[order(solution$Score, decreasing=T),]
  
  
  return(solution)
}

  