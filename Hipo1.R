###Iterador para la hipotesis 1


# Values of perception for each parameter by the other sex, so their are opposite in the
#correspondent functions

#Men how perceive women
MRich = 0.70
MInte = 0.10
MApar = 0.10
MActi = 0.10

#Women how perceive men
WRich = 0.70
WInte = 0.10
WApar = 0.10
WActi = 0.10

##Definir el numero de prueba
PRUEBAS <- 10
set.seed(69)
ReservaH1 <- matrix(0, nrow = 9, ncol = PRUEBAS)
Iteracion <- 0
for (p in 1:PRUEBAS) {
  for(m in 4:6) {
    for (n in 4:6) {
      
      Personality = 8
      
      #This parameters will be 1 for time, 2 for sex, 3 for appareance
      #4 for richness, 5 for inteligence, 6 for attitude, 7 for EconValue
      
      ## Also define the number of social sparcity by a m*n dimensional matrix
      
      SpaceM = m
      SpaceN = n
      
      Population = SpaceM*SpaceN
      
      #Which implies a population of SpaceM*SpaceN
      
      ## Create the tensor with the correspondent notation for the SOCIETY
      
      #First entry for the M dimension
      #Second entry for the N dimension
      #Third entry for person characteristics
      
      
      subs <- list(c(1,1,1))
      vals <- c(1)
      dims <- c(SpaceM,SpaceN,Personality)
      Society <- sptensor(subs, vals, dims)
      
      Society
      
      
      #Referenciar funcion
      
      Temporalidad <- matrix(1, SpaceM, SpaceN)
      Sexos <- sample.dynamic.matrix(0:1, c(1/2,1/2), SpaceM, SpaceN)
      Apariencias <- matrix( runif(SpaceM*SpaceN,min=1,max=10), SpaceM, SpaceN) 
      Riquezas <- 10*normalize(matrix( rlnorm(SpaceM*SpaceN,meanlog=4,sd=2.5), SpaceM, SpaceN)) 
      Inteligencias <- 10*normalize(matrix( rnorm(SpaceM*SpaceN,mean=100,sd=15), SpaceM, SpaceN)) 
      Actitudes <- matrix( runif(SpaceM*SpaceN,min=5,max=10), SpaceM, SpaceN) 
      EconomicValue <- matrix(0, SpaceM, SpaceN)
      MenMinValue <- matrix(0, SpaceM, SpaceN)
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          
          Society[i,j,] <- c(Temporalidad[i,j], Sexos[i,j], Apariencias[i,j], 
                             Riquezas[i,j], Inteligencias[i,j], Actitudes[i,j], 
                             EconomicValue[i,j], MenMinValue[i,j])
          
          
        }
        
        
      }
      
      Society
      
      
      
      
      #Assing the values to the persons according to the sex
      
      
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          
          if(Society[i,j,2] == 1){
            Society[i,j,7] <- Society[i,j,3]*MApar + Society[i,j,4]*MRich + 
              Society[i,j,5]*MInte + Society[i,j,6]*MActi
          }else if (Society[i,j,2] == 0){
            Society[i,j,7] <- Society[i,j,3]*WApar + Society[i,j,4]*WRich + 
              Society[i,j,5]*WInte + Society[i,j,6]*WActi
          }
        }
      }
      
      #Now, Assign the min value for mens
      
      # Create gamma discount for the "patience" of every men
      
      gammadiscount <- 0.5
      
      #Now the function
      
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          
          if(Society[i,j,2] == 1){
            Society[i,j,8] <- 0
          }else if (Society[i,j,2] == 0){
            Society[i,j,7] <- gammadiscount*(Society[i,j,3]*MApar + Society[i,j,4]*MRich + 
                                               Society[i,j,5]*MInte + Society[i,j,6]*MActi)
          }
        }
      }
      
      
      ### We know the define the positive interactions by stablish the relations of this values
      
      NewGenerationCount = 0
      
      ## We will consider cycles for the interactions of every person
      ## including up to two persons in each side and one person on the diagonals
      
      #Person to the east
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          for (k in 1:2)      {
            
            if(j + k <= SpaceN) {
              if(Society[i,j,7] > Society[i,j+k,7] & (Society[i,j,2] == 1 & Society[i,j+k,2] == 0)) { 
                NewGenerationCount = NewGenerationCount + 1
              }
            } 
            
          }
        }
      }
      
      #Person to the west
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          for (k in 1:2)      {
            
            if(j - k > 0) {
              if(Society[i,j,7] > Society[i,j-k,7] & (Society[i,j,2] == 1 & Society[i,j-k,2] == 0)){ 
                NewGenerationCount = NewGenerationCount + 1
              }
            } 
            
          }
        }
      }
      
      #Person to the north
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          for (k in 1:2)      {
            
            if(i + k <= SpaceM) {
              if(Society[i,j,7] > Society[i + k,j,7] & (Society[i,j,2] == 1 & Society[i+k,j,2] == 0)){ 
                NewGenerationCount = NewGenerationCount + 1
              }
            } 
            
          }
        }
      }
      
      #Person to the south
      
      for (i in 1:SpaceM) {
        for (j in 1:SpaceN) {
          for (k in 1:2)      {
            
            if(i - k > 0) {
              if(Society[i,j,7] > Society[i - k,j,7] & (Society[i,j,2] == 1 & Society[i-k,j,2] == 0)){ 
                NewGenerationCount = NewGenerationCount + 1
              }
            } 
            
          }
        }
      }
      
      
      ReservaH1[Iteracion%%9 + 1,p] <- NewGenerationCount
      Iteracion = Iteracion + 1
    }
  }
  
}

ReservaH1


