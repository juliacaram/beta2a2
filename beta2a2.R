library("entropart")
# necessita de:
# m = matriz de comunidade: espécie (linha) x plots (coluna). Abra sua matriz com read.table
# peso = vetor para o peso de cada plot. Abra um vetor ou rode a função:
peso <- rep(1, ncol(m)) #essa função cria um vetor, em que cada valor (1) corresponde a uma comunidade. 
# q = ordem de diversidade: 0, 1 ou 2

# para ver como são os formatos da matriz de comunidade e o vetor de peso, veja o help das funções MetaCommunity e DivPart

#copie e cole a função:
beta2a2 <- function(m, peso, q){
  comb <- combn(colnames(m), 2) #todas as combinações possíveis
  tab <- matrix(0, 1, ncol= ncol(comb)) #matriz que vai estocar respostas
  for (i in (1:ncol(comb))){
    comb2 <- comb[,i] #seleciona as colunas
    comb4 <- m[,c(comb2[1], comb2[2])] 
    meta2a2 <- MetaCommunity(comb4, Weights = rep(1, 2)) 
    div.beta2a2 <- DivPart(q = q, meta2a2, Biased = FALSE, Correction = "None") # calcula diversidade verdadeira para as duas comunidades. Atenção para o tipo de correção.
    res <- div.beta2a2$TotalBetaDiversity #seleciona apenas a diversidade beta
    tab[,i] <- (res)
    tab2 <- rbind(comb[1,], comb[2,], tab) #cola o resultado na matriz de estoque de resultados
  }
  tab3 <- (data.frame(t(tab2)))
  return(t(as.data.frame(tapply((as.numeric(as.character(tab3[,3]))), tab3[,c(1,2)], sum)))) #transforma em um data.frame e retorna o resultado em uma matriz quadrada
  
}


# exemplo:
#abra matriz de comunidade e veja se está tudo ok. A matriz tem que ser espécie nas linhas e locais nas colunas:
m <- read.table("/home/julia/Área de trabalho/database.csv", sep = ",", header = T)
m
#crie o vetor de peso:
peso <- rep(1, ncol(m))
peso
#rode a função com o que "q" desejado: 
q0 <- beta2a2(m, peso, 0) 

q1 <- beta2a2(m, peso, 1) 

q2 <- beta2a2(m, peso, 2) 

#retorna um data.frame triangular (plots x plots) com a diversidade beta entre os plots. Para transformar em matriz, pode-se usar:
na.omit(as.matrix(q0))

#para dúvidas: juliacaram@gmail.com