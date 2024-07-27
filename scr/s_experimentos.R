###### Configuracion inicial e importación de librerías
#setwd("R_umbral") #Setear si es necesario en el directorio del repositorio.
source("./scr/f_generar_muestras.R")
source("./scr/f_estimaciones.R")

##### Funciones auxiliares para realizar los experimentos y
#guardar opcionalmente los resultados y la muestra

experimento <- function(size,
                        replicates,
                        p_umbral,
                        exp_lambda,
                        alpha_pen = 0.4,
                        n_pen = 1,
                        c = 0.8,
                        export_sample = FALSE,
                        export_results = FALSE,
                        name = "",
                        seed = 1,
                        ...
                        ) {
    #################################
    #Asigno a una variable los parametros del experimento
    params <- data.frame(name = name,
                         seed = seed,
                         size = size,
                         replicates = replicates,
                         p_umbral = p_umbral,
                         exp_lambda = exp_lambda,
                         alpha_pen = alpha_pen,
                         n_pen = n_pen,
                         c = c
    )
    # Me fijo si ya existe un experimento con el mismo nombre.
    #En ese caso interrumpo el experimento, sino guardo los parámetros
    file_expe <- paste("./experiments/",name,".dat",sep="")
    if (file.exists(file_expe)) stop("Ya existe el experimento)")
    
    #Si tiene nombre válido el experimento exporto sus parámetros
    if (name!="") {
      write.table(params, file = file_expe,
                 sep = "\t", row.names = FALSE)
      }
    
    #################################
    #Genero muestras
    set.seed(seed = seed)  
    muestras <- matrix(generar_muestra(size * replicates, 
                                  p_umbral = p_umbral,
                                  exp_lambda = exp_lambda,
                                  ...
                                  ), ncol = size)
    #################################
    #Exporto muestras segun corresponda
    if (export_sample) {
      #Guardo las muestras en un archivo
      muestras_name <- paste("muestras", name, sep = "_")
      muestras_path <- paste("./data/",muestras_name,".dat",sep = "")
      write.table(muestras,muestras_path,col.names = FALSE,row.names = FALSE)
      
    }
    #################################
    #Estimo valores
    est.u <- apply(muestras,1,u_est,
                   alpha_pen = alpha_pen,
                   n_pen = n_pen,
                   c = c)
    est.lamb <- mapply(lambda_emv, muestra = asplit(muestras,1), u = est.u)
    results <- cbind(est.u,est.lamb)
    #################################
    #Exporto resultados segun corresponda
    if (export_results){
      outputs_name <- paste("results",name,sep="_")
      outputs_path <- paste("./outputs/",outputs_name,".dat",sep="")
      write.table(cbind(results,params),outputs_path,row.names = FALSE)
    }
    return(results)
}

##############################

## Ejecuto los experimentos
#Defino que parámetros variar
expe_params <- expand.grid(size = c(500,1000,1500,2000,5000,10000), 
                           p_umbral = c(0.95), 
                           exp_lambda = c(19) 
                           )
#Un identificador numérico de cada experimento.
#Se utiliza el mismo identificador para las muestras y los resultados
#También para la semilla.
expe_number_ini <- 67 #El primer número natural disponible
#Los suceivos experimentos irán sumándole 1 al valor
expe_params$number_name <- 1:nrow(expe_params)-1 + expe_number_ini

time.ini <- Sys.time()
#Ejecuto sucesivamente todos los experimentos
mapply(experimento,
       size = expe_params$size,
       p_umbral = expe_params$p_umbral,
       exp_lambda = expe_params$exp_lambda,
       name = paste("experimento",expe_params$number_name,sep="_"),
       seed = expe_params$number_name,
       MoreArgs = list(replicates = 1000,
                       n_pen = 1,
                       alpha_pen = 0.4,
                       c = 0.8,
                       u_min = 0,
                       u_max = 1,
                       modelo = "unif_exp",
                       export_sample = TRUE,
                       export_results = TRUE
                      )
      )
  
time.fin <- Sys.time()
time.fin-time.ini
