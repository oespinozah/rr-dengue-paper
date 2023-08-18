#FUNCIÓN HECHA PARA CREAR LA MATRIZ DE DATOS DE CLIMA
m_clima <- function(base, temperatura, precipitacion, humedad, n){
  base <- as.data.frame(base)
  m <- nrow(base)
  covars_1 <- matrix(ncol = n, nrow = m)
  covars_2 <- matrix(ncol = n, nrow = m)
  covars_3 <- matrix(ncol = n, nrow = m)
  for (a in 1:n) {
    for (i in (a+1):m) {
      covars_1[i,a] <- base[i-a,temperatura]
    }
  }
  for (a in 1:n) {
    for (i in (a+1):m) {
      covars_2[i,a] <- base[i-a,precipitacion]
    }
  }
  for (a in 1:n) {
    for (i in (a+1):m) {
      covars_3[i,a] <- base[i-a,humedad]
    }
  }
  covars <- cbind(covars_1,covars_2,covars_3)
  covars_names <- c()
  for (i in 1:n) {
    covars_names <- rbind(covars_names,
                          cbind(paste("temp_", i, sep = ""),
                                paste("prcp_", i, sep = ""),
                                paste("hrel_", i, sep = "")) )
  }
  covars <- as.data.frame(covars)
  names(covars) <- c(covars_names[,1],
                     covars_names[,2],
                     covars_names[,3])
  return(covars)
}