training = read.csv("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/income_tr.csv")
testing = read.csv("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/income_te.csv")
setwd("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/")

#training=training[1:100,]
#testing=testing[1:100,]

#function that calculates the mode
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#function that calculates the eucledian distances for each of the test set rows
euclid_income <- function(k, test, train) {
  final=data.frame()
  ncols = ncol(test)-1
  
  # imputing the mean for the attributes 'age' and 'hours_per_week'.
  # imputing the mode for the 2 ratio attributes 'capital_gain' and 'capital_loss'
  # since its almost always zero. Hence taking the mean for those would not be the right thing.
  # Imputing the Mode for all the nominal and ordinal attributes
  
  
  for(i in 1:nrow(train))
  {
    for( j in 2:(ncol(train)-1))
    {
      if(is.na(train[i,j]) || train[i,j]==" ?"){
        
        if(colnames(train)[j]== "age" || colnames(train)[j]== "hour_per_week"){
          train[i,j] = mean(train[,j])
          
        }else{
          train[i,j] = as.character( Mode(train[,j]) )
        }
        
      }
    }
  }
  
  #min max normalization for the 4 ratio attributes
  maxage = as.numeric(max(train$age))
  minage = as.numeric(min(train$age))
  diffage = maxage - minage
  
  maxgain = as.numeric(max(train$capital_gain))
  mingain = as.numeric(min(train$capital_gain))
  diffgain = maxgain - mingain 
  
  maxloss = as.numeric(max(train$capital_loss))
  minloss = as.numeric(min(train$capital_loss))
  diffloss = maxloss - minloss 
  
  maxhour = as.numeric(max(train$hour_per_week))
  minhour = as.numeric(min(train$hour_per_week))
  diffhour = maxhour - minhour 
  
  for(i in 1:nrow(train))
  {
    train[i,"age"] = (train[i,"age"]-minage) / diffage
    
    train[i,"capital_gain"] = (train[i,"capital_gain"]-mingain) / diffgain
    
    train[i,"capital_loss"] = (train[i,"capital_loss"]-minloss) / diffloss
    
    train[i,"hour_per_week"] = (train[i,"hour_per_week"]-minhour) / diffhour
  }
  
  #normalizing the test data
  for(i in 1:nrow(test))
  {
    if(test[i,"age"] != " ?") test[i,"age"] = (test[i,"age"]-minage) / diffage
    
    if(test[i,"capital_gain"] != " ?") test[i,"capital_gain"] = (test[i,"capital_gain"]-mingain) / diffgain
    
    if(test[i,"capital_loss"] != " ?") test[i,"capital_loss"] = (test[i,"capital_loss"]-minloss) / diffloss
    
    if(test[i,"hour_per_week"] != " ?") test[i,"hour_per_week"] = (test[i,"hour_per_week"]-minhour) / diffhour
  }
  
  for (i in 1:nrow(test))
  {
    distance=data.frame()
    for(j in 1:nrow(train))
    {
      
      #caclualte distance for nominal data
      
      if((as.character(test$workclass[i]) == as.character(train$workclass[j])) || (as.character(test$workclass[i])== " ?"))
        workclassdiff = 0
      else workclassdiff = 1
      
      
      if((as.character(test$marital_status[i]) == as.character(train$marital_status[j])) || (as.character(test$marital_status[i]) == " ?"))
        marital_statusdiff = 0
      else marital_statusdiff = 1
      
      if((as.character(test$occupation[i]) == as.character(train$occupation[j])) || (as.character(test$occupation[i]) == " ?"))
        occupationdiff = 0
      else occupationdiff = 1
      
      if((as.character(test$relationship[i]) == as.character(train$relationship[j])) || (as.character(test$relationship[i]) == " ?"))
        relationshipdiff = 0
      else relationshipdiff = 1
      
      if((as.character(test$race[i]) == as.character(train$race[j])) || (as.character(test$race[i]) == " ?"))
        racediff = 0
      else racediff = 1
      
      if((as.character(test$gender[i]) == as.character(train$gender[j])) || (as.character(test$gender[i]) == " ?")) 
        genderdiff = 0
      else genderdiff = 1
      
      if((as.character(test$native_country[i]) == as.character(train$native_country[j])) || (as.character(test$native_country[i]) == " ?"))
        native_countrydiff = 0
      else native_countrydiff = 1
      
      ndist=(workclassdiff+marital_statusdiff+occupationdiff+
               relationshipdiff+racediff+genderdiff+native_countrydiff)/7
      
      #Calculate Euclidean Distance for ratio data
      if(test$age[i] != " ?"){
        rdist = (test$age[i]-train$age[j])^2
      }else rdist = 0
      if(test$capital_gain[i] != " ?"){
        rdist = rdist + (test$capital_gain[i]-train$capital_gain[j])^2
      }
      if(test$capital_loss[i] != " ?"){
        rdist = rdist + (test$capital_loss[i]-train$capital_loss[j])^2
      }
      if(test$hour_per_week[i] != " ?"){
        rdist = rdist + (test$hour_per_week[i]-train$hour_per_week[j])^2
      }
      
      rdist=sqrt(rdist)
      
      #Calculate distance for ordinal data
      if(test$education_cat[i] != " ?")
        odist=abs(test$education_cat[i]-train$education_cat[j])/15
      else odist =0;
      
      #Final distance is the average of the ordinal, nominal and ratio distances
      dist=(ndist+rdist+odist)/3
      
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = test[i,1]
      distance[j,2]=train[j,1]
      distance[j,3]=dist
      
    }
    #sort the distance matrix by distance
    distance = distance[order(distance[,3]),]
    
    #pick the top k rows with least distances and write it to the final data frame in required format
    distance=distance[1:k,]
    final[i,1]=distance[1,1]
    count=2
    for (s in 1:k)
    {
      for (t in 2:3)
      {
        final[i,count]=distance[s,t]
        count=count+1  
      }
    }
  }
  
  #rename the columns of the final data frame
  colnames(final)[1]="Transaction ID"
  count=2
  for (c in 1:k)
  {
    
    colnames(final)[count]=paste("ID", as.character(c)) 
    count=count+1
    colnames(final)[count]=paste("Prox", as.character(c))
    count=count+1
  }
  final[,ncol(final)+1]=test$class
  colnames(final)[ncol(final)]="Class"
  #write.csv(final,"result_income_euclid.csv")
  final
}

#function that calculates the manhattan distances for each of the test set rows
manhat_income <- function(k, test, train) {
  final=data.frame()
  ncols = ncol(test)-1
  
  # imputing the mean for the attributes 'age' and 'hours_per_week'.
  # imputing the mode for the 2 ratio attributes 'capital_gain' and 'capital_loss'
  # since its almost always zero. Hence taking the mean for those would not be the right thing.
  # Imputing the Mode for all the nominal and ordinal attributes
  
  
  for(i in 1:nrow(train))
  {
    for( j in 2:(ncol(train)-1))
    {
      if(is.na(train[i,j]) || train[i,j]==" ?"){
        
        if(colnames(train)[j]== "age" || colnames(train)[j]== "hour_per_week"){
          train[i,j] = mean(train[,j])
          
        }else{
          train[i,j] = as.character( Mode(train[,j]) )
        }
        
      }
    }
  }
  
  #min max normalization for the 4 ratio attributes
  maxage = as.numeric(max(train$age))
  minage = as.numeric(min(train$age))
  diffage = maxage - minage
  
  maxgain = as.numeric(max(train$capital_gain))
  mingain = as.numeric(min(train$capital_gain))
  diffgain = maxgain - mingain 
  
  maxloss = as.numeric(max(train$capital_loss))
  minloss = as.numeric(min(train$capital_loss))
  diffloss = maxloss - minloss 
  
  maxhour = as.numeric(max(train$hour_per_week))
  minhour = as.numeric(min(train$hour_per_week))
  diffhour = maxhour - minhour 
  
  for(i in 1:nrow(train))
  {
    train[i,"age"] = (train[i,"age"]-minage) / diffage
    
    train[i,"capital_gain"] = (train[i,"capital_gain"]-mingain) / diffgain
    
    train[i,"capital_loss"] = (train[i,"capital_loss"]-minloss) / diffloss
    
    train[i,"hour_per_week"] = (train[i,"hour_per_week"]-minhour) / diffhour
  }
  
  #normalizing the test data
  for(i in 1:nrow(test))
  {
    if(test[i,"age"] != " ?") test[i,"age"] = (test[i,"age"]-minage) / diffage
    
    if(test[i,"capital_gain"] != " ?") test[i,"capital_gain"] = (test[i,"capital_gain"]-mingain) / diffgain
    
    if(test[i,"capital_loss"] != " ?") test[i,"capital_loss"] = (test[i,"capital_loss"]-minloss) / diffloss
    
    if(test[i,"hour_per_week"] != " ?") test[i,"hour_per_week"] = (test[i,"hour_per_week"]-minhour) / diffhour
  }
  
  for (i in 1:nrow(test))
  {
    distance=data.frame()
    for(j in 1:nrow(train))
    {
      
      #caclualte distance for nominal data
      
      if((as.character(test$workclass[i]) == as.character(train$workclass[j])) || (as.character(test$workclass[i])== " ?"))
        workclassdiff = 0
      else workclassdiff = 1
      
      
      if((as.character(test$marital_status[i]) == as.character(train$marital_status[j])) || (as.character(test$marital_status[i]) == " ?"))
        marital_statusdiff = 0
      else marital_statusdiff = 1
      
      if((as.character(test$occupation[i]) == as.character(train$occupation[j])) || (as.character(test$occupation[i]) == " ?"))
        occupationdiff = 0
      else occupationdiff = 1
      
      if((as.character(test$relationship[i]) == as.character(train$relationship[j])) || (as.character(test$relationship[i]) == " ?"))
        relationshipdiff = 0
      else relationshipdiff = 1
      
      if((as.character(test$race[i]) == as.character(train$race[j])) || (as.character(test$race[i]) == " ?"))
        racediff = 0
      else racediff = 1
      
      if((as.character(test$gender[i]) == as.character(train$gender[j])) || (as.character(test$gender[i]) == " ?")) 
        genderdiff = 0
      else genderdiff = 1
      
      if((as.character(test$native_country[i]) == as.character(train$native_country[j])) || (as.character(test$native_country[i]) == " ?"))
        native_countrydiff = 0
      else native_countrydiff = 1
      
      ndist=(workclassdiff+marital_statusdiff+occupationdiff+
               relationshipdiff+racediff+genderdiff+native_countrydiff)/7
      
      #Calculate Manhattan Distance for ratio data
      if(test$age[i] != " ?"){
        rdist = abs(test$age[i]-train$age[j])
      }else rdist = 0
      if(test$capital_gain[i] != " ?"){
        rdist = rdist + abs(test$capital_gain[i]-train$capital_gain[j])
      }
      if(test$capital_loss[i] != " ?"){
        rdist = rdist + abs(test$capital_loss[i]-train$capital_loss[j])
      }
      if(test$hour_per_week[i] != " ?"){
        rdist = rdist + abs(test$hour_per_week[i]-train$hour_per_week[j])
      }
      
      #Calculate distance for ordinal data
      if(test$education_cat[i] != " ?")
        odist=abs(test$education_cat[i]-train$education_cat[j])/15
      else odist =0;
      
      #Final distance is the average of the ordinal, nominal and ratio distances
      dist=(ndist+rdist+odist)/3
      
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = test[i,1]
      distance[j,2]=train[j,1]
      distance[j,3]=dist
      
    }
    #sort the distance matrix by distance
    distance = distance[order(distance[,3]),]
    
    #pick the top k rows with least distances and write it to the final data frame in required format
    distance=distance[1:k,]
    final[i,1]=distance[1,1]
    count=2
    for (s in 1:k)
    {
      for (t in 2:3)
      {
        final[i,count]=distance[s,t]
        count=count+1  
      }
    }
  }
  
  #rename the columns of the final data frame
  colnames(final)[1]="Transaction ID"
  count=2
  for (c in 1:k)
  {
    
    colnames(final)[count]=paste("ID", as.character(c)) 
    count=count+1
    colnames(final)[count]=paste("Prox", as.character(c))
    count=count+1
  }
  final[,ncol(final)+1]=test$class
  colnames(final)[ncol(final)]="Class"
  #write.csv(final,"result_income_euclid.csv")
  final
}

#function that predicts the class of the test dataset using knn
knn_income <- function(k, test, train, proximity) {
  
  #calculate the eucledian or manhattan distances depending on the parameter in 
  # the knn_income function
  if(proximity == "euclid") final = euclid_income (k, test, train)
  else if(proximity == "manhat") final = manhat_income (k, test, train)
  if(proximity == "euclid" || proximity == "manhat"){
    results=data.frame()
    
    #calucate the number in each class for the k nearest neighbors
    for(i in 1:nrow(final)){
      countLess=0
      countMore=0
      for(j in 1:k){
        row=final[i,j*2]
        if(as.character(train[train$ID==row,16]) == " <=50K") countLess=countLess+1
        if(as.character(train[train$ID==row,16]) ==" >50K") countMore=countMore+1
      }
      
      #calculate their probabilites
      probLess=countLess/k
      probMore=countMore/k
      
      #choose the max probability and assign that class
      if(max(probLess,probMore) == probLess)predClass = " <=50K"
      else if(max(probLess,probMore) == probMore)predClass = " >50K"
      
      #write this to the dataframe in the required format
      results[i,1] = final$`Transaction ID`[i]
      results[i,2] = final$Class[i]
      results[i,3] = predClass
      results[i,4] = max(probLess,probMore)
    }
    
    #rename the columns of the final dataframe
    colnames(results) = c("Transaction ID","Actual Class", "Predicted Class", "Probability")
    results
  }
  else print("Error! Enter either euclid or manhat for proximity")
}

#(void main equivalent)

results=knn_income(3, testing, training, "euclid")
#results=knn_income(3, testing, training, "manhat")
#results=knn_income(3, testing, training, "abc")
write.csv(results,"result_income.csv")
