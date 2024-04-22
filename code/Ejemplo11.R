
library(R6)

library(dplyr)



FuncionesClases <- R6Class(
  classname = "FuncionesClases"
  , public = list(
    DataContrasteCor = NULL
    , DataIndicadores = NULL
    
    , initialize = function(){
      
      self$DataContrasteCor = NA
      self$DataIndicadores = NA
      
    }
    
    # Ver las correlaciones cruzadas
    # , EvaluarCor = function(DataMatriz){ #NO SE NECESITA ESPECIFICAR OBJETO DE QUE TIPO ES!!
    #   ut <- upper.tri(DataMatriz)
    #   self$DataContrasteCor <- data.frame(i = rownames(DataMatriz)[row(DataMatriz)[ut]],
    #                                        j = rownames(DataMatriz)[col(DataMatriz)[ut]],
    #                                        cor=t(DataMatriz)[ut]
    #                                       )
    #   self$DataContrasteCor <- self$DataContrasteCor %>% 
    #     mutate(Decision = case_when(
    #       cor >= .6 ~ 1
    #       , TRUE ~ 0
    #     ))
    #   
    #   return(self$DataContrasteCor)
    # }
    
    , EvaluarCor = function(DataMatriz){ #NO SE NECESITA ESPECIFICAR OBJETO DE QUE TIPO ES!!
      if (!is.matrix(DataMatriz)) {
        stop("La data no es tipo matriz!!!")
      }else{
        ut <- upper.tri(DataMatriz)
        self$DataContrasteCor <- data.frame(i = rownames(DataMatriz)[row(DataMatriz)[ut]],
                                            j = rownames(DataMatriz)[col(DataMatriz)[ut]],
                                            cor=t(DataMatriz)[ut]
        )
        self$DataContrasteCor <- self$DataContrasteCor %>% 
          mutate(Decision = case_when(
            cor >= .6 ~ 1
            , TRUE ~ 0
          ))
        
        return(self$DataContrasteCor)
      }
    }
    
    # Ver el cálculo de indicadores
    , CalcularIndicadores = function(objeto_logit){
      objeto_logit.ks <- ks.test(x = objeto_logit$fitted.values[which(objeto_logit$y == 0)],
                                 y = objeto_logit$fitted.values[which(objeto_logit$y == 1)])
      ks <- objeto_logit.ks$statistic
      objeto_logit.roc <- roc(response=objeto_logit$y,predictor=objeto_logit$fitted.values)
      objeto_logit.gini <- 2*objeto_logit.roc$auc-1
      self$DataIndicadores <- data.frame(variable="logistico",Gini=round(100*objeto_logit.gini,2),KS=round(100*ks,2))
      row.names(self$DataIndicadores) <- NULL
      
      return(self$DataIndicadores)
    }
    
    
    
  )
)


Prueba11 = FuncionesClases$new()
Prueba11$EvaluarCor(corre1)
Prueba11$CalcularIndicadores(modelo1)






