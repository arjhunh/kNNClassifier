testing = read.csv("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/Iris_Test.csv")
training = read.csv("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/Iris.csv")
setwd("C:/Users/Avinash Vallur/Desktop/Arjhun/Data Mining/Datasets/")

#function that calculates the eucledian distances for each of the test set rows
Euclid_iris <- function(k, test, train) {
  ncols = ncol(test)-1
  final = data.frame()
  #calculating the euclidean distance
  for(i in 1:nrow(test))
  {
    distance = data.frame()
    for(j in 1:nrow(train))
    {
      dist=0
      
      for (n in 1:ncols)
      {
        dist = dist + ((test[i,n]-train[j,n])*(test[i,n]-train[j,n]))  
      }
      dist=as.numeric(sqrt(dist))
      
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = i
      distance[j,2]=j
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
  final
}

#function that calculates the manhattan distances for each of the test set rows
Manhat_iris <- function(k, test, train) {
  ncols = ncol(test)-1
  final = data.frame()
  #calculating the manhattan distance
  for(i in 1:nrow(test))
  {
    distance = data.frame()
    for(j in 1:nrow(train))
    {
      dist=0
      
      for (n in 1:ncols)
      {
        dist = dist + abs(test[i,n]-train[j,n])  
      }
      
      #Write this distance to the distance matrix(data frame)
      distance[j,1] = i
      distance[j,2]=j
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
  final
}

#function that predicts the class of the test dataset using knn
knn_iris <- function(k, test, train, proximity) {
  
  #calculate the eucledian or manhattan distances depending on the parameter in 
  # the knn_iris function
  if(proximity == "euclid") final = Euclid_iris (k, test, train)
  else if(proximity == "manhat") final = Manhat_iris (k, test, train)
  
  if(proximity == "euclid" || proximity == "manhat"){
    results=data.frame()
    
    #calucate the number in each class for the k nearest neighbors
    for(i in 1:nrow(final)){
      countSent=0
      countVers=0
      countVirg=0
      for(j in 1:k){
        row=final[i,j*2]
        if(train[row,5]=="Iris-setosa") countSent=countSent+1
        else if(train[row,5]=="Iris-versicolor") countVers=countVers+1
        else if(train[row,5]=="Iris-virginica") countVirg=countVirg+1
      }
      
      #calculate their probabilites
      probSent=countSent/k
      probVers=countVers/k
      probVirg=countVirg/k
      
      #choose the max probability and assign that class
      if(max(probSent,probVirg,probVers)==probSent)predClass = "Iris-setosa"
      else if(max(probSent,probVirg,probVers)==probVers)predClass = "Iris-versicolor"
      else if(max(probSent,probVirg,probVers)==probVirg)predClass = "Iris-virginica"
      
      #write this to the dataframe in the required format
      results[i,1] = final$`Transaction ID`[i]
      results[i,2] = final$Class[i]
      results[i,3] = predClass
      results[i,4] = max(probSent,probVirg,probVers)
    }
    
    #rename the columns of the final dataframe
    colnames(results) = c("Transaction ID","Actual Class", "Predicted Class", "Probability")
    results
  }
  else print("Error! Enter either euclid or manhat for proximity")
}

#(void main equivalent)

results=knn_iris(3, testing, training, "euclid")
#results=knn_iris(3, testing, training, "manhat")
#results=knn_iris(3, testing, training, "abc")
write.csv(results,"result_iris.csv")
