library("entropart")
# require:
# m = community matrix: species (lines) x plots (columns)
# peso = vector with the weight for each plot:
peso <- rep(1, ncol(m)) #creates a vector in which 1 is the weight of a plot. See Metacommunity help
# q = order (generally 0, 1 or 2)

#function:
beta2a2 <- function(m, peso, q){
  comb <- combn(colnames(m), 2) #creates all possible combinations
  tab <- matrix(0, 1, ncol= ncol(comb)) 
  for (i in (1:ncol(comb))){
    comb2 <- comb[,i] #select the columns
    comb4 <- m[,c(comb2[1], comb2[2])] 
    meta2a2 <- MetaCommunity(comb4, Weights = rep(1, 2)) 
    div.beta2a2 <- DivPart(q = q, meta2a2, Biased = FALSE, Correction = "None") # calculates Hill numbers. NOte the type of correction
    res <- div.beta2a2$TotalBetaDiversity #selects only beta diversity
    tab[,i] <- (res)
    tab2 <- rbind(comb[1,], comb[2,], tab) 
  }
  tab3 <- (data.frame(t(tab2)))
  return(t(as.data.frame(tapply((as.numeric(as.character(tab3[,3]))), tab3[,c(1,2)], sum)))) #sorry for the mess :)
  
}


# exemple:
#open and verify the community matrix
m <- read.table("/home/julia/Área de trabalho/database.csv", sep = ",", header = T)
m
#vector of weights
peso <- rep(1, ncol(m))
peso
#run the function:
q0 <- beta2a2(m, peso, 0) 

q1 <- beta2a2(m, peso, 1) 

q2 <- beta2a2(m, peso, 2) 

#pairwise beta. You can omit NAs
na.omit(as.matrix(q0))

#pcontact me: juliacaram@gmail.com
