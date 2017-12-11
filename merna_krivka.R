#' Merna krivka pro lichobeznikovy profil
#' Rating curve for trapezoid profile
#'
#' @param hloubka celkova hloubka koryta / total depth
#' @param krok vypocetni krok / computing step
#' @param sirka_dna sirka dna koryta / bottom width
#' @param sklon_svahu sklon svahu koryta / slope tilt
#' @param sklon_dna podelny sklon koryta / longitudinal slope
#' @param manning1 
#' soucinitel drsnosti pro dolni cast koryta / roughness coefficient - bottom part
#' @param manning2
#' soucinitel drsnosti pro horni cast koryta / roughness coefficient - upper part 
#' @param zmena_n 
#' hloubka zmeny soucinitele drsnoti / depth of roughness coefficient change 
#'
#' @return
#' @export
#'
#' @examples
#' krivka(hloubka = 1.2, krok = 0.1, sirka_dna = 1.5, sklon_svahu = 1.5, 
#' sklon_dna = 0.005, manning1 = 0.02, manning2 = 0.03, zmena_n = 0.4)
#' 
#' 
#' 

krivka <- function(hloubka, krok, sirka_dna, sklon_svahu, sklon_dna, manning1, manning2, zmena_n){
  if(missing(manning2) & missing(zmena_n)){
    require(plotly)
    dta <- data.frame(h = 0, O = 0, S = 0, R = 0, n = manning1, C = 0, v = 0, Q = 0)
    for (i in seq(from = 0, to = hloubka, by = krok)){
      O <- sirka_dna + 2*i*sqrt(sklon_svahu*sklon_svahu+1)
      S <- i*(sirka_dna+sklon_svahu*i)
      R <- S/O
      C <- (1/manning1)*R^(1/6)
      v <- C*sqrt(R*sklon_dna)
      Q <- S*v
      k <- data.frame(h = i, O = O, S = S, R = R, n = manning1, C = C, v = v, Q = Q)
      dta <- rbind(dta,k)
    }
    x <- plot_ly(dta, x = ~Q, y = ~h, type = 'scatter', mode = 'lines+markers')
    print(x)
    return(data.frame(dta[c(-1,-2),])) 
    
  } else if (!missing(manning2) & missing(zmena_n)){print("Dodej zmenu Manninga")
  } else if (missing(manning2) & !missing(zmena_n)){print("Dodej hodnotu manning2")
  } else {
    require(plotly)
    dta <- data.frame(h = 0, O = 0, S = 0, R = 0, n = manning1, C = 0, v = 0, Q = 0)
    for (i in seq(from = 0, to = zmena_n, by = krok)){
      O <- sirka_dna + 2*i*sqrt(sklon_svahu*sklon_svahu+1)
      S <- i*(sirka_dna+sklon_svahu*i)
      R <- S/O
      C <- (1/manning1)*R^(1/6)
      v <- C*sqrt(R*sklon_dna)
      Q <- S*v
      k <- data.frame(h = i, O = O, S = S, R = R, n = manning1, C = C, v = v, Q = Q)
      dta <- rbind(dta,k)
      O_p <- dta$O[nrow(dta)]}
    for (i in seq(from = zmena_n+krok, to = hloubka, by = krok)){
      O <- sirka_dna + 2*i*sqrt(sklon_svahu*sklon_svahu+1)
      S <- i*(sirka_dna+sklon_svahu*i)
      R <- S/O
      n_v <- ((O_p*manning1)+((O-O_p)*manning2))/(O)
      C <- (1/n_v)*R^(1/6)
      v <- C*sqrt(R*sklon_dna)
      Q <- S*v
      k <- data.frame(h = i, O = O, S = S, R = R, n = n_v, C = C, v = v, Q = Q)
      dta <- rbind(dta,k)
      
    }
    x <- plot_ly(dta, x = ~Q, y = ~h, type = 'scatter', mode = 'lines+markers')
    print(x)
    return(data.frame(dta[c(-1,-2),])) 
    
  }
}
