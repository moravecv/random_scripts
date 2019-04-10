#' Funkce pro vypocet E pomoci sady vzorcu
#'
#' @param in_file excel s vstupnimi hodnotami (nutno )
#' @param sheetname jmeno listu v excel souboru
#' @param out_file vystupni cesta souboru
#'
#' @return data.frame s vypocitanymi hodnotami
#' @export excel s vypocitanymi hodnotami
#'
#' @examples vzorcE(in_file = "D:/sesit.xlsx", sheetname = "List1", out_file = "D:/output.xlsx")

vzorcE <- function(in_file, sheetname, out_file){
  #' Funkce pro vypocet E pomoci sady vzorcu
  #'
  #' @param in_file excel s vstupnimi hodnotami (nutno )
  #' @param sheetname jmeno listu v excel souboru
  #' @param out_file vystupni cesta souboru
  #'
  #' @return data.frame s vypocitanymi hodnotami
  #' @export excel s vypocitanymi hodnotami
  #'
  #' @examples vzorcE(in_file = "D:/sesit.xlsx", sheetname = "List1", out_file = "D:/output.xlsx")
  
  library(xlsx)
  
  dta <- read.xlsx(file = in_file, sheetName = sheetname)
  
  vypar <- which(colnames(dta) == "vypar")
  Tvzd <- which(colnames(dta) == "Tvzd")
  Tv <- which(colnames(dta) == "Tv")
  H <- which(colnames(dta) == "H")
  R <- which(colnames(dta) == "R")
  V <- which(colnames(dta) == "V")
  
  dta$vypar <- as.numeric(as.character(dta$vypar))
  dta$Tvzd <- as.numeric(as.character(dta$Tvzd))
  dta$Tv <- as.numeric(as.character(dta$Tv))
  dta$H <- as.numeric(as.character(dta$H))
  dta$R <- as.numeric(as.character(dta$R))
  dta$V <- as.numeric(as.character(dta$V))
  
  vypocet <- apply(dta, 1, function(x){
    
    ##### Vzorce ######
    res1 <- data.frame(Vzorec_1 = (0.0169 * x[R]) + 0.0451)
    res2 <- data.frame(Vzorec_2 = 0.0157 * x[R]^ 1.0148)
    res3 <- data.frame(Vzorec_3 = 0.7962 * exp(0.0071 * x[R]))
    res4 <- data.frame(Vzorec_4 = (0.7962 * exp(0.0071 * x[R])) + (0.0318 * x[Tvzd]) - 0.4229)
    res5 <- data.frame(Vzorec_5 = (0.7962 * exp(0.0071 * x[R])) + (0.0318 * x[Tvzd]) + (1.0359 * x[H]) - 1.2287) # Byla chyba
    res6 <- data.frame(Vzorec_6 = (0.7962 * exp(0.0071 * x[R])) + (0.018 * x[Tv]) - 0.2967)
    res7 <- data.frame(Vzorec_7 = (0.0169 * x[R]) + (0.0238 * x[Tv]) - 0.4005)
    res8 <- data.frame(Vzorec_8 = (0.0169 * x[R]) + (0.0238 * x[Tv]) + (0.0099 * x[Tvzd]) - 0.544)
    res9 <- data.frame(Vzorec_9 = (0.0169 * x[R]) + (0.0369 * x[Tvzd]) - 0.4953)
    res10 <- data.frame(Vzorec_10 = (0.0169 * x[R]) + (0.0369 * x[Tvzd]) - (0.002 * x[R]) - 0.1536)
    res11 <- data.frame(Vzorec_11 = (0.0169 * x[R]) - (0.9874 * x[H]) + 0.8062)
    res12 <- data.frame(Vzorec_12 = (0.0169 * x[R]) - (0.9874 * x[H]) + (0.0219 * x[Tvzd]) + 0.4889)
    res13 <- data.frame(Vzorec_13 = (0.0157 * x[R]^ 1.0148) + (0.0338 * x[Tvzd]) - 0.4598)
    res14 <- data.frame(Vzorec_14 = (0.0157 * x[R]^ 1.0148) + (0.0338 * x[Tvzd]) - (0.0021 * x[R]) - 0.1027)
    res15 <- data.frame(Vzorec_15 = (0.0157 * x[R]^ 1.0148) + (0.0209 * x[Tv]) - 0.3566)
    res16 <- data.frame(Vzorec_16 = (0.0157 * x[R]^ 1.0148) + (0.0209 * x[Tv]) - (0.0017 * x[R]) - 0.069)
    res17 <- data.frame(Vzorec_17 = (0.2517 * x[Tvzd]) - 0.687)
    res18 <- data.frame(Vzorec_18 = (0.2517 * x[Tvzd]) - (2.3629 * x[H]) + 1.1489)
    res19 <- data.frame(Vzorec_19 = (0.2517 * x[Tvzd]) - (2.3629 * x[H]) + (0.6943 * log(x[V])) + 0.8696)
    res20 <- data.frame(Vzorec_20 = (0.2517 * x[Tvzd]) + (0.4505 * x[V]) - 1.3757)
    res21 <- data.frame(Vzorec_21 = (0.2517 * x[Tvzd]) + (0.4505 * x[V]) - (2.4448 * x[H]) + 0.5243)
    res22 <- data.frame(Vzorec_22 = (0.2517 * x[Tvzd]) + (0.0036 * x[R]) - 1.3103)
    res23 <- data.frame(Vzorec_23 = (0.2517 * x[Tvzd]) + (0.0036 * x[R]) - (0.0458 * x[Tvzd]) - 0.6462)
    res24 <- data.frame(Vzorec_24 = 0.0744 * x[Tvzd]^ 1.3657)
    res25 <- data.frame(Vzorec_25 = (0.0744 * x[Tvzd]^ 1.3657) + (0.5116 * x[V]) - 0.7344)
    res26 <- data.frame(Vzorec_26 = (0.0744 * x[Tvzd]^ 1.3657) + (0.5116 * x[V]) - (1.9818 * x[H]) + 0.8058)
    res27 <- data.frame(Vzorec_27 = 0.5987 * exp(0.1046 * x[Tvzd]))
    res28 <- data.frame(Vzorec_28 = (0.5987 * exp(0.1046 * x[Tvzd])) + (0.6069 * x[V]) - 0.8857)
    res29 <- data.frame(Vzorec_29 = (0.5987 * exp(0.1046 * x[Tvzd])) + (0.6069 * x[V]) + (0.0032 * x[R]) - 1.44)
    res30 <- data.frame(Vzorec_30 = 0.2222 * x[Tv] - 1.1476)
    res31 <- data.frame(Vzorec_31 = (0.2222 * x[Tv]) - (2.4958 * x[H]) + 0.7915)
    res32 <- data.frame(Vzorec_32 = (0.2222 * x[Tv]) - (2.4958 * x[H]) + (0.8009 * log(x[V])) + 0.4693)
    res33 <- data.frame(Vzorec_33 = (0.2222 * x[Tv]) + (0.7662 * log(x[V])) - 1.4564)
    res34 <- data.frame(Vzorec_34 = (0.2222 * x[Tv]) + (0.7662 * log(x[V])) - (2.013 * log(x[H])) - 1.973)
    res35 <- data.frame(Vzorec_35 = 0.032 * x[Tv]^ 1.5401)
    res36 <- data.frame(Vzorec_36 = (0.032 * x[Tv]^ 1.5401) + (0.5879 * x[V]) - 0.862)
    res37 <- data.frame(Vzorec_37 = (0.032 * x[Tv]^ 1.5401) + (0.5879 * x[V]) - (2.3735 * x[H]) + 0.9827)
    res38 <- data.frame(Vzorec_38 = (0.032 * x[Tv]^ 1.5401) - (1.763 * log(x[H])) - 0.4163)
    res39 <- data.frame(Vzorec_39 = (0.032 * (x[Tv]^ 1.5401)) - (1.763 * log(x[H])) + (0.611 * x[V]) - 1.3498)
    res40 <- data.frame(Vzorec_40 = 0.4834 * exp(0.0936 * x[Tv]))
    res41 <- data.frame(Vzorec_41 = (0.4834 * exp(0.0936 * x[Tv])) + (0.7129 * x[V]) - 1.0594)
    res42 <- data.frame(Vzorec_42 = (0.4834 * exp(0.0936 * x[Tv])) + (0.7129 * x[V]) - (1.9686 * x[H]) + 0.4706)
    
    resA <- data.frame(Vzorec_A = (0.2287 * x[Tvzd]) - 0.6453)
    resB <- data.frame(Vzorec_B = (0.2287 * x[Tvzd]) + (0.0123 * x[Tv]) - 0.8563)
    resC <- data.frame(Vzorec_C = 0.0824 * x[Tvzd]^ 1.289)
    resD <- data.frame(Vzorec_D = (0.0824 * x[Tvzd]^ 1.289) + (0.0186 * x[Tv]) - 0.2662)
    resE <- data.frame(Vzorec_E = (0.0824 * x[Tvzd]^ 1.289) + (0.0186 * x[Tv]) - (0.0142 * x[Tvzd]) - 0.0709)
    resF <- data.frame(Vzorec_F = 0.5267 * exp(0.1073 * x[Tvzd]))
    resG <- data.frame(Vzorec_G = (0.5267 * exp(0.1073 * x[Tvzd])) - (0.0126 * x[Tvzd]) + 0.2154)
    resH <- data.frame(Vzorec_H = 0.2027 * x[Tv] - 0.9777)
    resI <- data.frame(Vzorec_I = (0.2027 * x[Tv]) - (0.003 * x[Tvzd]) - 0.9362)
    resJ <- data.frame(Vzorec_J = 0.0407 * x[Tv]^ 1.4366)
    resK <- data.frame(Vzorec_K = (0.0407 * x[Tv]^ 1.4366) + (0.0075 * x[Tv]) - 0.0872)
    resL <- data.frame(Vzorec_L = 0.4469 * exp(0.0956 * x[Tv]))
    resM <- data.frame(Vzorec_M = (0.4469 * exp(0.0956 * x[Tv])) - (0.0112 * x[Tvzd]) + 0.1869)
    resN <- data.frame(Vzorec_N = (0.2027 * x[Tv]) - (2.2623 * x[H]) + 0.7696)
    resO <- data.frame(Vzorec_O = (0.2027 * x[Tv]) - (2.2623 * x[H]) - (0.0232 * x[Tvzd]) + 1.0904)
    resP <- data.frame(Vzorec_P = (0.2027 * x[Tv]) - (2.2623 * x[H]) - (0.33 * log(x[Tvzd])) + 1.6253)
    resQ <- data.frame(Vzorec_Q = (0.0407 * x[Tv]^ 1.4366) - (2.4396 * x[H]) + 1.9261)
    resR <- data.frame(Vzorec_R = (0.0407 * x[Tv]^ 1.4366) - (1.893 * log(x[H])) - 0.455)
    resS <- data.frame(Vzorec_S = (0.0407 * x[Tv]^ 1.4366) - (1.893 * log(x[H])) - (0.239 * log(x[Tvzd])) + 0.1645)
    resT <- data.frame(Vzorec_T = (0.4469 * exp(0.0956 * x[Tv])) - (1.8338 * x[H]) + 1.4425)
    resU <- data.frame(Vzorec_U = (0.5267 * exp(0.1073 * x[Tvzd])) - (1.4277 * x[H]) + 1.1526)
    resV <- data.frame(Vzorec_V = (0.0824 * x[Tvzd]^ 1.289) - (2.3265 * x[H]) + 1.8627)
    resW <- data.frame(Vzorec_W = (0.2287 * x[Tvzd]) - 0.6453 - (2.1887 * x[H]) + 1.7027)
    
    ##### Spojeni #####
    fin <- cbind(res1,res2, res3, res4, res5, res6, res7, res8, res9, res10, res11,
                 res12, res13, res14, res15, res16, res17, res18, res19, res20, res21,
                 res22, res23, res24, res25, res26, res27, res28, res29, res30, res31,
                 res32, res33, res34, res35, res36, res37, res38, res39, res40, res41,
                 res42, 
                 
                 resA, resB, resC, resD, resE, resF, resG, resH, resI, resJ, resK, 
                 resL, resM, resN, resO, resP, resQ, resR, resS, resT, resU, resV,
                 resW)
    return(fin)}
  )
  
  
  add <- which(!colnames(dta) %in% c("vypar", "Tvzd", "Tv", "H", "R", "V"))
  
  vypocet2 <- data.frame(do.call(rbind, vypocet))
  
  fin <- cbind(dta[,add], vypocet2)
  
  write.xlsx(x = fin, file = out_file, showNA = F, row.names = F)
  return(fin)
}


