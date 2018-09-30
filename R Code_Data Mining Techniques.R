
library(dplyr)

######### Create the undirected graph ###############

fb1 <- facebook_combined
fb2 <- data.frame(fb1$X2,fb1$X1) ## create a second dataframe with transposed columns

fb1<-as.data.frame(fb1)
fb2<-as.data.frame(fb2)

colnames(fb1) <- c("a","b")
colnames(fb2) <- c("a","b")

fb3 <- merge(fb1, fb2, by=c("a","b"), all=TRUE)  ## merge the two dataframes
fb_undirected<- unique(fb3) ## keep only the unique pairs

## use the reshapre2 package and create a 4040*40389 df in the form "0" and "1" where "0" denotes non friendship and "1" stands for friendship between the users
install.packages("reshape2")
library(reshape2)

fb_undirected2 <- dcast(fb_undirected,a~b,value.var="b")
colnames(fb_undirected2)[1]="id"

fb_undirected2[!is.na(fb_undirected2)] <- 1
fb_undirected2[is.na(fb_undirected2)] <- 0
fb_undirected2$id <- seq(0,4038)

## convert the above dataframe to a matrix.This would be  a 4039*4039 matrix which is the total number of nodes in our case

fb <- fb_undirected2
fb <- data.matrix(fb[,-1]) ## convert to matrix
fb <- mapply(fb,FUN=as.numeric) ## convert matrix to numeric
fb <- matrix(fb , nrow =4039  , ncol =4039) ## reshape matrix dimensions

## create vectors containing the friends of each node in the form "0" and "1". Each vector will be multiplied with the graph data (fb_undirected2) in order to find the common friends each user has with the rest users

node107<- as.vector(fb_undirected2$"107")
node1126<- as.vector(fb_undirected2$"1126")
node14<- as.vector(fb_undirected2$"14")
node35<- as.vector(fb_undirected2$"35")

## create vectors containing the initial frieds of each node in order to exclude them from the final recommendation

friendsofnode107 <- fb_undirected%>% filter(a %in% c(107)) ## friends of node107
f107<- friendsofnode107$b

friendsofnode1126 <- fb_undirected%>% filter(a %in% c(1126)) ## friends of node107
f1126<- friendsofnode1126$b

friendsofnode14 <- fb_undirected%>% filter(a %in% c(14)) ## friends of node107
f14<- friendsofnode14$b

friendsofnode35 <- fb_undirected%>% filter(a %in% c(35)) ## friends of node107
f35<- friendsofnode35$b

## multiply fb matrix with the friends of each node in order to find all common friends that each node has with the rest of the users

commonfriends_node107  <- as.data.frame(fb*node107)
colnames(commonfriends_node107) <-colnames(fb_undirected2[-c(1)]) ## add ids

commonfriends_node1126 <- as.data.frame(fb*node1126)
colnames(commonfriends_node1126) <-colnames(fb_undirected2[-c(1)]) ## add ids

commonfriends_node14   <- as.data.frame(fb*node14)
colnames(commonfriends_node14) <-colnames(fb_undirected2[-c(1)]) ## add ids

commonfriends_node35   <- as.data.frame(fb*node35)
colnames(commonfriends_node35) <-colnames(fb_undirected2[-c(1)]) ## add ids


#################### fof algorithm #######################

## node107
ttlcommonfriends_node107 <- as.data.frame(colSums(commonfriends_node107)) ## the sum of each column in the ttlcommonfriends_node107 dataframe is the total number of common friends between node107 and all the other users
ttlcommonfriends_node107$id <- as.numeric(colnames(fb_undirected2[-c(1)])) ## add ids
colnames(ttlcommonfriends_node107)[1] <- c("comfriends")
sum2<- ttlcommonfriends_node107[order(-ttlcommonfriends_node107$comfriends,ttlcommonfriends_node107$id),] ## order comfriends in descending order and ids in ascending order
fof_rec_node107 <- subset(sum2,!(sum2$id %in% f107) & sum2$id != 107) ## exclude initial friends of node 107 and itself from the recommendation list
fof_rec_node107 <- fof_rec_node107[1:10,]  ## keep the first 10 recommendations
print(fof_rec_node107)

## node 1126
ttlcommonfriends_node1126 <- as.data.frame(colSums(commonfriends_node1126)) ## the sum of each column in the ttlcommonfriends_node1126 dataframe is the total number of common friends between node1126 and all the other users
ttlcommonfriends_node1126$id <- as.numeric(colnames(fb_undirected2[-c(1)])) ## add ids
colnames(ttlcommonfriends_node1126)[1] <- c("comfriends")
sum2<- ttlcommonfriends_node1126[order(-ttlcommonfriends_node1126$comfriends,ttlcommonfriends_node1126$id),] ## order comfriends in descending order and ids in ascending order
fof_rec_node1126 <- subset(sum2,!(sum2$id %in% f1126) & sum2$id != 1126) ## exclude initial friends of node1126 and itself from the recommendation list
fof_rec_node1126 <- fof_rec_node1126[1:10,]  ## keep the first 10 recommendations
print(fof_rec_node1126)

## node 14
ttlcommonfriends_node14 <- as.data.frame(colSums(commonfriends_node14)) ## the sum of each column in the ttlcommonfriends_node14 dataframe is the total number of common friends between node14 and all the other users
ttlcommonfriends_node14$id <- as.numeric(colnames(fb_undirected2[-c(1)])) ## add ids
colnames(ttlcommonfriends_node14)[1] <- c("comfriends")
sum2<- ttlcommonfriends_node14[order(-ttlcommonfriends_node14$comfriends,ttlcommonfriends_node14$id),] ## order comfriends in descending order and ids in ascending order
fof_rec_node14 <- subset(sum2,!(sum2$id %in% f14) & sum2$id != 14) ## exclude initial friends of node 14 and itself from the recommendation list
fof_rec_node14 <- fof_rec_node14[1:10,]  ## keep the first 10 recommendations
print(fof_rec_node14)

## node 35
ttlcommonfriends_node35 <- as.data.frame(colSums(commonfriends_node35)) ## the sum of each column in the ttlcommonfriends_node35 dataframe is the total number of common friends between node35 and all the other users
ttlcommonfriends_node35$id <- as.numeric(colnames(fb_undirected2[-c(1)])) ## add ids
colnames(ttlcommonfriends_node35)[1] <- c("comfriends")
sum2<- ttlcommonfriends_node35[order(-ttlcommonfriends_node35$comfriends,ttlcommonfriends_node35$id),] ## order comfriends in descending order and ids in ascending order
fof_rec_node35 <- subset(sum2,!(sum2$id %in% f35) & sum2$id != 35) ## exclude initial friends of node 35 and itself from the recommendation list
fof_rec_node35 <- fof_rec_node35[1:10,]  ## keep the first 10 recommendations
print(fof_rec_node35)

#################### jaccard algorithm #######################

## node107
ttlcommonfriends_node107$ttlfriends <- colSums(fb_undirected2[-c(1)]) ## add a column containing the total number of friends each node has
ttlcommonfriends_node107$union <- ttlcommonfriends_node107$ttlfriends+sum(fb_undirected2$"107")-ttlcommonfriends_node107$comfriends ## add a column containing  the union of node107 with each of the rest nodes
ttlcommonfriends_node107$jaccardscore <- ttlcommonfriends_node107$comfriends/ttlcommonfriends_node107$union ## compute the jaccard score for each node
j107 <- ttlcommonfriends_node107[order(-ttlcommonfriends_node107$jaccardscore,ttlcommonfriends_node107$id),c(2,5)]  ## order jaccard score in descending order and ids in ascending order
node107_jaccard_rec <- subset(j107,!(j107$id %in% f107) & j107$id != 107) ## exclude initial friends of node107 and itself from the recommendation list
node107_jaccard_rec <- node107_jaccard_rec[1:10,] ## keep top 10 recommendations
print(round(node107_jaccard_rec,3))

## node1126
ttlcommonfriends_node1126$ttlfriends <- colSums(fb_undirected2[-c(1)]) ## add a column containing the total number of friends each node has
ttlcommonfriends_node1126$union <- ttlcommonfriends_node1126$ttlfriends+sum(fb_undirected2$"1126")-ttlcommonfriends_node1126$comfriends ## add a column containing  the union of node1126 with each of the rest nodes
ttlcommonfriends_node1126$jaccardscore <- ttlcommonfriends_node1126$comfriends/ttlcommonfriends_node1126$union ## compute the jaccard score for each node
j1126 <- ttlcommonfriends_node1126[order(-ttlcommonfriends_node1126$jaccardscore,ttlcommonfriends_node1126$id),c(2,5)]  ## order jaccard score in descending order and ids in ascending order
node1126_jaccard_rec <- subset(j1126,!(j1126$id %in% f1126) & j1126$id != 1126) ## exclude initial friends of node1126 and itself from the recommendation list
node1126_jaccard_rec <- node1126_jaccard_rec[1:10,] ## keep top 10 recommendations
print(round(node1126_jaccard_rec,3))

## node14
ttlcommonfriends_node14$ttlfriends <- colSums(fb_undirected2[-c(1)]) ## add a column containing the total number of friends each node has
ttlcommonfriends_node14$union <- ttlcommonfriends_node14$ttlfriends+sum(fb_undirected2$"14")-ttlcommonfriends_node14$comfriends ## add a column containing  the union of node14 with each of the rest nodes
ttlcommonfriends_node14$jaccardscore <- ttlcommonfriends_node14$comfriends/ttlcommonfriends_node14$union ## compute the jaccard score for each node
j14 <- ttlcommonfriends_node14[order(-ttlcommonfriends_node14$jaccardscore,ttlcommonfriends_node14$id),c(2,5)]  ## order jaccard score in descending order and ids in ascending order
node14_jaccard_rec <- subset(j14,!(j14$id %in% f14) & j14$id != 14) ## exclude initial friends of node14 and itself from the recommendation list
node14_jaccard_rec <- node14_jaccard_rec[1:10,] ## keep top 10 recommendations
print(round(node14_jaccard_rec,3))

## node35
ttlcommonfriends_node35$ttlfriends <- colSums(fb_undirected2[-c(1)]) ## add a column containing the total number of friends each node has
ttlcommonfriends_node35$union <- ttlcommonfriends_node35$ttlfriends+sum(fb_undirected2$"35")-ttlcommonfriends_node35$comfriends ## add a column containing  the union of node35 with each of the rest nodes
ttlcommonfriends_node35$jaccardscore <- ttlcommonfriends_node35$comfriends/ttlcommonfriends_node35$union ## compute the jaccard score for each node
j35 <- ttlcommonfriends_node35[order(-ttlcommonfriends_node35$jaccardscore,ttlcommonfriends_node35$id),c(2,5)]  ## order jaccard score in descending order and ids in ascending order
node35_jaccard_rec <- subset(j35,!(j35$id %in% f35) & j35$id != 35) ## exclude initial friends of node35 and itself from the recommendation list
node35_jaccard_rec <- node35_jaccard_rec[1:10,] ## keep top 10 recommendations
print(round(node35_jaccard_rec,3))

################ Adamic and Adar function #######################3

## node 107
ttlcommonfriends_node107$aaf <- 1/log(colSums(fb_undirected2[-c(1)])) ## create a vector containing  the aaf score for each node. This would be the result of the 1/log|Nc| for each node
commonfriends_node107 <- as.data.frame(fb*node107) ## ultiply fb matrix with the friends of node 107 in order to find all common friends with the rest of the users
aaf_score_node107 <- as.vector(ttlcommonfriends_node107$aaf)*commonfriends_node107 ## multiply this matrix with the aaf score of each node. By summing these results we will have the output of the aaf function for node107 with each of the rest users. 
aaf_score_node107 <- as.data.frame(aaf_score_node107) ## convert to dataframe
aaf107 <- as.data.frame(colSums(aaf_score_node107,na.rm = TRUE)) ## sum the columns and get the total aaf score of node107 with the rest nodes. inf and NaN values will be excluded from the summation
aaf107$id <- as.numeric(colnames(fb_undirected2[-c(1)])) ## add ids
colnames(aaf107)[1] <- c("sumaaf")
node107_aaf_rec <- aaf107[order(-aaf107$sumaaf,aaf107$id),] ## order aaf score on descending order and ids in ascending order
node107_aaf_rec <- subset(node107_aaf_rec,!(node107_aaf_rec$id %in% f107) & node107_aaf_rec$id != 107) ## exclude initial friends of node107 and itself from the recommendation list
node107_aaf_rec <- node107_aaf_rec[1:10,] ## keep  top 10 recommendations
print(round(node107_aaf_rec,2))

## node 1126
ttlcommonfriends_node1126$aaf <- 1/log(colSums(fb_undirected2[-c(1)])) ##create a vector containing  the aaf score for each node. This would be the result of the 1/log|Nc| for each node
commonfriends_node1126 <- as.data.frame(fb*node1126) ## ultiply fb matrix with the friends of node 1126 in order to find all common friends with the rest of the users
aaf_score_node1126 <- as.vector(ttlcommonfriends_node1126$aaf)*commonfriends_node1126 ## multiply this matrix with the aaf score of each node. By summing these results we will have the output of the aaf function for node1126 with each of the rest users. 
aaf_score_node1126 <- as.data.frame(aaf_score_node1126) ## convert to dataframe
aaf1126 <- as.data.frame(colSums(aaf_score_node1126,na.rm = TRUE)) ##sum the columns and get the total aaf score of node1126 with the rest nodes. inf and NaN values will be excluded from the summation
aaf1126$id <- as.numeric(colnames(fb_undirected2[-c(1)]))  ## add ids
colnames(aaf1126)[1] <- c("sumaaf")
node1126_aaf_rec <- aaf1126[order(-aaf1126$sumaaf,aaf1126$id),] ## order aaf score on descending order and ids in ascending order
node1126_aaf_rec <- subset(node1126_aaf_rec,!(node1126_aaf_rec$id %in% f1126) & node1126_aaf_rec$id != 1126) ## exclude initial friends of node1126 and itself from the recommendation list
node1126_aaf_rec <- node1126_aaf_rec[1:10,] ## keep  top 10 recommendations
print(round(node1126_aaf_rec,2))

## node 14
ttlcommonfriends_node14$aaf <- 1/log(colSums(fb_undirected2[-c(1)])) ##create a vector containing  the aaf score for each node. This would be the result of the 1/log|Nc| for each node
commonfriends_node14 <- as.data.frame(fb*node14) ## ultiply fb matrix with the friends of node 14 in order to find all common friends with the rest of the users
aaf_score_node14 <- as.vector(ttlcommonfriends_node14$aaf)*commonfriends_node14 ## multiply this matrix with the aaf score of each node. By summing these results we will have the output of the aaf function for node14 with each of the rest users. 
aaf_score_node14 <- as.data.frame(aaf_score_node14) ## convert to dataframe
aaf14 <- as.data.frame(colSums(aaf_score_node14,na.rm = TRUE)) ##sum the columns and get the total aaf score of node14 with the rest nodes. inf and NaN values will be excluded from the summation
aaf14$id <- as.numeric(colnames(fb_undirected2[-c(1)]))  ## add ids
colnames(aaf14)[1] <- c("sumaaf")
node14_aaf_rec <- aaf14[order(-aaf14$sumaaf,aaf14$id),] ## order aaf score on descending order and ids in ascending order
node14_aaf_rec <- subset(node14_aaf_rec,!(node14_aaf_rec$id %in% f14) & node14_aaf_rec$id != 14) ## exclude initial friends of node14 and itself from the recommendation list
node14_aaf_rec <- node14_aaf_rec[1:10,] ## keep  top 10 recommendations
print(round(node14_aaf_rec,2))

## node 35
ttlcommonfriends_node35$aaf <- 1/log(colSums(fb_undirected2[-c(1)])) ##create a vector containing  the aaf score for each node. This would be the result of the 1/log|Nc| for each node
commonfriends_node35 <- as.data.frame(fb*node35) ## ultiply fb matrix with the friends of node 35 in order to find all common friends with the rest of the users
aaf_score_node35 <- as.vector(ttlcommonfriends_node35$aaf)*commonfriends_node35 ## multiply this matrix with the aaf score of each node. By summing these results we will have the output of the aaf function for node35 with each of the rest users. 
aaf_score_node35 <- as.data.frame(aaf_score_node35) ## convert to dataframe
aaf35 <- as.data.frame(colSums(aaf_score_node35,na.rm = TRUE)) ##sum the columns and get the total aaf score of node35 with the rest nodes. inf and NaN values will be excluded from the summation
aaf35$id <- as.numeric(colnames(fb_undirected2[-c(1)]))  ## add ids
colnames(aaf35)[1] <- c("sumaaf")
node35_aaf_rec <- aaf35[order(-aaf35$sumaaf,aaf35$id),] ## order aaf score on descending order and ids in ascending order
node35_aaf_rec <- subset(node35_aaf_rec,!(node35_aaf_rec$id %in% f35) & node35_aaf_rec$id != 35) ## exclude initial friends of node35 and itself from the recommendation list
node35_aaf_rec <- node35_aaf_rec[1:10,] ## keep  top 10 recommendations
print(round(node35_aaf_rec,2))

############## R code for functions (question 5)############################


###Load the dataset in R
library(readxl)
facebook_combined <- read_excel("C:/Users/user/Desktop/MSBusinessAnalytics/Mining Big Datasets/Assignment 1/facebook_combined.xlsx", 
                                col_types = c("numeric", "numeric"))


###Create a second dataframe by inverting the columns
facebook_combined2 <- facebook_combined[,c(2,1)]

###Give the names of columns same as the facebook_combined
colnames(facebook_combined2) <- colnames(facebook_combined)

###Union the data frames into one 
facebook_combined_all <- rbind(facebook_combined,facebook_combined2)

###Remove duplicates in case some of the connections already existed. In that way we have made the directed
###graph undirected since all connections are bidirectional now.
facebook_undirected <- unique(facebook_combined_all)




###For this question we are going to create a function that computes the score of FOF method.


FOF_Score <- function(NodeID){ ###It takes as an input a number which is the id of the Node
  library('dplyr')  ###We load the library dplyr that we are going to use
  
  if (is.numeric(NodeID)==FALSE)  ###We have a check and if the user gives a string we stop
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    Friends_of_Node <- data.frame(facebook_undirected%>% filter(Source %in% c(NodeID)))  ### keep the friends of the input node as an argument
    
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    for (i in 1:nrow(Friends_of_Node)){  ###For all friends of the node
      
      FOF_Node <- data.frame(facebook_undirected %>% filter(Source == as.numeric(Friends_of_Node[i,2]))) ###We find the friends of every friend of the node
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node)   ###We keep them in a data frame
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))  ###We exclude from the data frame with the friends of friends the node itself
      
    }
    
    ###The friend recommendations are calculated from the following expression. We do a count in the nodes we stored in the data frame FOF_Node_Friends. Then we sort the records by count descending and NodeID ascending.
    ###We exclude those nodes that are already friends with the node we examine. Finally we keep the top 10 recommendations.
    Recommendations_FOF <- data.frame(FOF_Node_Friends%>%dplyr::count(Target, sort=TRUE)%>%dplyr::arrange(desc(n),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2]))%>%head(10))
    
    return(Recommendations_FOF)
  }
  
}

###For this question we are going to create a function that computes the score of Jaccard method.


Jaccard_Score <- function(NodeID){ ###It takes as an input a number which is the id of the Node
  library('dplyr') ###We load the library dplyr that we are going to use
  
  if (is.numeric(NodeID)==FALSE) ###We have a check and if the user gives a string we stop
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    Friends_of_Node <- data.frame(facebook_undirected%>% filter(Source %in% c(NodeID)))   ###We keep the friends of the node we passed in as an argument
    
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    for (i in 1:nrow(Friends_of_Node)){  ###For all friends of the node
      
      FOF_Node <- data.frame(facebook_undirected %>% filter(Source == as.numeric(Friends_of_Node[i,2])))  ###We find the friends of every friend of the node
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node) ###We keep them in a data frame
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))   ###We exclude from the data frame with the friends of friends the node itself
      
    }
    
    Intersection <- data.frame(FOF_Node_Friends%>%dplyr::count(Target, sort=TRUE)%>%dplyr::arrange(desc(n),Target)) ##We keep in a data frame the intersection we need in order to estimate the Jaccard coefficient.
    ##So we do a count in the friends of friends and we sort the records by count descending and NodeID ascending.
    
    Union_Node <- data.frame(NULL,NULL,stringsAsFactors = F)
    
    
    for (i in 1:nrow(Intersection)){ ###For all the nodes of the intersection
      Fr_of_Intersection <- data.frame(Target=(facebook_undirected[which(facebook_undirected$Source==Intersection[i,1]),2])) ###We find the friends of every node of the intersection
      Node_Fr <- data.frame(Target=(Friends_of_Node[which(Friends_of_Node$Source==NodeID),2])) ###We keep in data data frame the friends of the node we passed in as argument
      Union_Node_1 <- data.frame(Target = Intersection[i,1],n = nrow(union(Fr_of_Intersection,Node_Fr))) ###We have a data frame where we keep for each node of the intersection the number of union friends between that node and the node we passes in as an argument
      Union_Node <- rbind(Union_Node,Union_Node_1)  ### We store the results of the Union in a data frame for all the nodes
    }
    
    ###The friend recommendations are calculated from the following expression. First we keep together the nodes, the number of intersection friends and the number of union friends.Then we group by the node and divide the number of intersection by the number of union. 
    ###After we sort the records by score descending and NodeID ascending. We exclude those nodes that are already friends with the node we examine. Finally we keep the top 10 recommendations.
    Recommendations_Jaccard <- data.frame(cbind(Intersection,n2=Union_Node[,2])%>%group_by(Target) %>% dplyr::summarise(Score = max(n/n2))%>%dplyr::arrange(desc(Score),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2]))%>%head(10))
    
    
    
    return(Recommendations_Jaccard)
  }
  
}


###For this question we are going to create a function that computes the score of Adamic method.


Adamic_Score <- function(NodeID){  ###It takes as an input a number which is the id of the Node
  
  library('plyr') ###We load the library dplyr that we are going to use
  library('dplyr') ###We load the library dplyr that we are going to use
  
  
  if (is.numeric(NodeID)==FALSE)  ###We have a check and if the user gives a string we stop
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    
    Friends_of_Node <- data.frame(facebook_undirected%>% filter(Source %in% c(NodeID)))  ###We keep the friends of the node we passed in as an argument
    
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    
    for (i in 1:nrow(Friends_of_Node)){  ###For all friends of the node
      
      FOF_Node <- data.frame(facebook_undirected %>% filter(Source == as.numeric(Friends_of_Node[i,2])))  ###We find the friends of every friend of the node
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node)   ###We keep them in a data frame
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))  ###We exclude from the data frame with the friends of friends the node itself
      
    }
    
    Node_Friends <- data.frame(facebook_undirected%>%dplyr::count(Source, sort = TRUE)) ###We count the appearence of every node in the dataset
    Recom_Adamic <-data.frame(join(FOF_Node_Friends,Node_Friends, by="Source", type="inner" ))  ###We join the two data frames
    
    ###The friend recommendations are calculated from the following expression.First we group by the joined data frame and calculate the score by the summary of 1/log(n).After we sort the records by score descending and NodeID ascending.
    ###We exclude those nodes that are already friends with the node we examine. Finally we keep the top 10 recommendations.
    Recommendations_Adamic <- data.frame(Recom_Adamic %>% group_by(Target) %>% dplyr::summarise(Score = sum(1/log(n)))%>%dplyr::arrange(desc(Score),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2]))%>%head(10))
    
    
    
  }
  return(Recommendations_Adamic)
}




###5)

###In this question first we have to evaluate the functions we created in the previous questions by applying them 
###to 40 users with ids multiple to 100

###We find the unique node ids
nodes <-unique(facebook_undirected[,1])
nodes <- as.data.frame(nodes)

###We keep all the nodes from 100 and above because we need the users that are multiple of 100 
nodes <- nodes[nodes>=100]


multiple100friends <- c(NULL)

for (i in 1:length(nodes)) ###For all the rows of the nodes data frame
{
  if  ((nodes[i])%%100==0) ###We keep the nodes that the mod with 100 is zero, which means that they multiply with number 100
  {multiple100friends <- c(multiple100friends,nodes[i]) 
  print(i)}
  else 
  {print(i)}
}


Recom_All_Methods <-data.frame("Node"=NULL,"FOF"=NULL,"Jaccard"=NULL,"Adamic"=NULL,stringsAsFactors = F)
Node <-data.frame(NULL,stringsAsFactors = F)
FOF <-data.frame(NULL,NULL,stringsAsFactors = F)
Jaccard <- data.frame(NULL,NULL,stringsAsFactors = F)
Adamic <- data.frame(NULL,NULL,stringsAsFactors = F)



for (i in 1:length(multiple100friends)){  ###For all the 40 users that multiply by 100
  Node <- rep(multiple100friends[i],times = 10)  ###We replicate the node 10 times beacause we will use it for the data frame below
  FOF <- FOF_Score(multiple100friends[i])  ###We run the function for FOF for the node
  Jaccard<-Jaccard_Score(multiple100friends[i])  ###We run the function for Jaccard for the node
  Adamic <- Adamic_Score(multiple100friends[i])  ###We run the function for Adamic for the node
  
  Recom_All_Methods <- rbind(Recom_All_Methods,cbind("Node"=Node,"FOF"=FOF[,1],"Jaccard"=Jaccard[,1],"Adamic"=Adamic[,1]))  ###We put together all the results
  
  print(i)
  
  
}

###### compute average similarities

node <- c(seq(100,4000,100))
FoF_jaccard <- c(rep(0,40))
FoF_Adamic<- c(rep(0,40))
Jaccard_Adamic<- c(rep(0,40))

similarity_percentage <- data.frame(node,FoF_jaccard ,FoF_Adamic,Jaccard_Adamic)

i<-1
j<-1

for (j in 1:40) {
  similarity_percentage[j,2] <- length(intersect(Recom_All_Methods[i:(i+9),2],Recom_All_Methods[i:(i+9),3]))
  similarity_percentage[j,3] <- length(intersect(Recom_All_Methods[i:(i+9),2],Recom_All_Methods[i:(i+9),4]))
  similarity_percentage[j,4] <- length(intersect(Recom_All_Methods[i:(i+9),3],Recom_All_Methods[i:(i+9),4]))
  i <- i+10
  j <- j+1 }

print(similarity_percentage)

average_similarity_between_the_algorithms <- colSums(similarity_percentage[,2:4])/400 
print(average_similarity_between_the_algorithms)

###As a second step to this question we have to implement an algorithm that can forecast if the recommendations are going to be accepted from the users.

###First we have to change a little the functions in order to keep all the recommendations and not only the top 10.
###Also the change is about not reading the facebook_undirected data frame which contains all the connections but a new data frame
###that does not have the friendship between F1 and F2 the 2 randomly selected users.


FOF_Score_All <- function(NodeID){
  library('dplyr')
  
  if (is.numeric(NodeID)==FALSE)
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    Friends_of_Node <- data.frame(facebook_undirected2%>% filter(Source %in% c(NodeID)))
    
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    for (i in 1:nrow(Friends_of_Node)){
      
      FOF_Node <- data.frame(facebook_undirected2 %>% filter(Source == as.numeric(Friends_of_Node[i,2])))
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node) 
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))
      
    }
    
    Recommendations_FOF <- data.frame(FOF_Node_Friends%>%dplyr::count(Target, sort=TRUE)%>%dplyr::arrange(desc(n),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2])))
    
    return(Recommendations_FOF)
  }
  
}




Jaccard_Score_All <- function(NodeID){
  library('dplyr')
  
  if (is.numeric(NodeID)==FALSE)
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    Friends_of_Node <- data.frame(facebook_undirected2%>% filter(Source %in% c(NodeID)))
    
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    for (i in 1:nrow(Friends_of_Node)){
      
      FOF_Node <- data.frame(facebook_undirected2 %>% filter(Source == as.numeric(Friends_of_Node[i,2])))
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node) 
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))
      
    }
    
    Intersection <- data.frame(FOF_Node_Friends%>%dplyr::count(Target, sort=TRUE)%>%dplyr::arrange(desc(n),Target))
    
    Union_Node <- data.frame(NULL,NULL,stringsAsFactors = F)
    
    
    for (i in 1:nrow(Intersection)){
      Fr_of_Intersection <- data.frame(Target=(facebook_undirected2[which(facebook_undirected2$Source==Intersection[i,1]),2]))
      Node_Fr <- data.frame(Target=(Friends_of_Node[which(Friends_of_Node$Source==NodeID),2]))
      Union_Node_1 <- data.frame(Target = Intersection[i,1],n = nrow(union(Fr_of_Intersection,Node_Fr)))
      Union_Node <- rbind(Union_Node,Union_Node_1)
    }
    
    
    Recommendations_Jaccard <- data.frame(cbind(Intersection,n2=Union_Node[,2])%>%group_by(Target) %>% dplyr::summarise(Score = max(n/n2))%>%dplyr::arrange(desc(Score),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2])))
    
    
    
    return(Recommendations_Jaccard)
  }
  
}



Adamic_Score_All <- function(NodeID){
  
  library('plyr')
  library('dplyr')
  
  
  if (is.numeric(NodeID)==FALSE)
  {
    print("You have to pass in a number as a parameter.Please try again.")
  }
  else
  {
    
    Friends_of_Node <- data.frame(facebook_undirected2%>% filter(Source %in% c(NodeID)))
    FOF_Node <- data.frame(NULL,NULL, stringsAsFactors = F)
    FOF_Node_Friends <- data.frame(NULL,NULL, stringsAsFactors = F)
    
    
    for (i in 1:nrow(Friends_of_Node)){
      
      FOF_Node <- data.frame(facebook_undirected2 %>% filter(Source == as.numeric(Friends_of_Node[i,2])))
      FOF_Node_Friends <-rbind(FOF_Node_Friends,FOF_Node) 
      FOF_Node_Friends <-data.frame(FOF_Node_Friends%>% filter(Target != NodeID))
      
    }
    
    Node_Friends <- data.frame(facebook_undirected2%>%dplyr::count(Source, sort = TRUE))
    Recom_Adamic <-data.frame(join(FOF_Node_Friends,Node_Friends, by="Source", type="inner" ))
    Recommendations_Adamic <- data.frame(Recom_Adamic %>% group_by(Target) %>% dplyr::summarise(Score = sum(1/log(n)))%>%dplyr::arrange(desc(Score),Target)%>%filter(!(Target %in% Friends_of_Node[which(Friends_of_Node$Source==NodeID),2])))
    
    
    
  }
  return(Recommendations_Adamic)
}




###After these minor changes to the functions we are going to proceed with what asked in this question.


F2_in_F1_list <- data.frame("Node"=NULL,"FOF"=NULL,"Jaccard"=NULL,"Adamic"=NULL,stringsAsFactors = F)
F1_in_F2_list <- data.frame("Node"=NULL,"FOF"=NULL,"Jaccard"=NULL,"Adamic"=NULL,stringsAsFactors = F)



for (i in 1:100){ 
  
  random_friends<-sample(1:nrow(facebook_undirected), 1, replace = F) ###We take a random friendship 
  
  F1 <- as.numeric(facebook_undirected[random_friends,1]) ###We have F1
  F2 <- as.numeric(facebook_undirected[random_friends,2]) ###We have F2
  
  ###With the two commands below we create a data frame that we have removed the friendship between F1 and F2 
  facebook_undirected2 <- subset(facebook_undirected, !(facebook_undirected$Source==F1 & facebook_undirected$Target==F2))
  facebook_undirected2 <- subset(facebook_undirected2, !(facebook_undirected2$Source==F2 & facebook_undirected2$Target==F1))
  
  ###We calculate the scores with all the methods for F1
  Scores <-FOF_Score_All(F1)
  FOF_Position <- which(Scores$Target==F2)
  
  Scores <-Jaccard_Score_All(F1)
  Jaccard_Position <- which(Scores$Target==F2)
  
  Scores <-Adamic_Score_All(F1)
  Adamic_Position <- which(Scores$Target==F2)
  
  ###We keep in a data frame the position that F2 appeared in F1's recommendations list 
  F2_in_F1_list <- rbind(F2_in_F1_list,cbind("F2"=F2,"FOF_F2"=FOF_Position,"Jaccard_F2"=Jaccard_Position,"Adamic_F2"=Adamic_Position))
  
  
  ###We do the same procedure for F2
  Scores <-FOF_Score_All(F2)
  FOF_Position <- which(Scores$Target==F1)
  
  Scores <-Jaccard_Score_All(F2)
  Jaccard_Position <- which(Scores$Target==F1)
  
  Scores <-Adamic_Score_All(F2)
  Adamic_Position <- which(Scores$Target==F1)
  
  
  F1_in_F2_list <- rbind(F1_in_F2_list,cbind("F1"=F1,"FOF_F1"=FOF_Position,"Jaccard_F1"=Jaccard_Position,"Adamic_F1"=Adamic_Position))
  
  ###We put back the firendship of F1 and F2 before we do the experiment again
  facebook_undirected2 <- facebook_undirected
  
  print(i)
}


###We create a data frame with all the positions for the methods for the 100 friendships
F1_F2 <- cbind(F2_in_F1_list,F1_in_F2_list)


###We calculate the average position for F1 and F2 every time for the 3 methods
F1_F2_AVG <- data.frame("F2"=F1_F2[,1],"F1"=F1_F2[,5],"FOF_AVG"=rowMeans(F1_F2[,c(2,6)], na.rm = FALSE, dims = 1),"Jaccard_AVG"=rowMeans(F1_F2[,c(3,7)], na.rm = FALSE, dims = 1),"Adamic_AVG"=rowMeans(F1_F2[,c(4,8)], na.rm = FALSE, dims = 1),stringsAsFactors = F)




