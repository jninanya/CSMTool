#' Productivity and resilience capacity indexes
#'
#' This function calculates the productivity and resilience capacity
#' following Thiry et al. (2016).
#'
#' @param id Row name of the data frame data.
#' @param ys Column name of the yield data of drought stress condition.
#' @param yp Column name of the yield data of potential condition.
#' @param data A data frame.
#'
#' @details Use \code{inv = TRUE} in the ThiryScore function only for the SSI and TOL indices.
#'
#' @return It returns a list including:
#' \itemize{\code{indexes}} A data frame of the drought stress indexes SSI, TOL, MP, GMP, and STI.
#' \itemize{\code{scores}} A data frame of the Thiry's scores.
#' \itemize{\code{corr1.}} A matrix of Pearson correlation among the drought stress indexes and the Thiry's scores.
#' \itemize{\code{range}} A list object with the range of the Thiry's scores SSI, TOL, MP, GMP, and STI.
#' \itmeize{\code{comb1.}} A data frame of combinations of the Thiry's scores.
#' \itemize{\code{corr2.}} A matrix of Pearson correlation among Thiry's combinations and the yield data (ys and yp).
#' \itemize{\code{comb2.}} A data frame of the best Thiry's combination.
#'
#' @author Johan Ninanya, Javier Rinza
#'
#' @references
#' Thiry, A. A., Chavez Dulanto, P. N., Reynolds, M. P., & Davies, W. J.
#' (2016). How can we improve crop genotypes to increase stress
#' resilience and productivity in a future climate? A new crop screening
#'  method based on productivity and resistance to abiotic stress.
#'  Journal of experimental botany, 67(19), 5593-5603.
#'
#' @seealso \code{ThiryScore}
#'
#' @examples
#' n <- 20
#' id <- 1:n
#' ys <- runif(n)*5
#' yp <- runif(n)*10
#' data <- data.frame(id, ys, yp)
#' ts <- Thiry("id","ys","yp",data)
#'
#' @importFrom stats cor
#' @export

#===================================================#
# Function to calculate productivity and resilience #
# capacity index according to Thiry's metodology    #
#===================================================#
Thiry <- function(id, ys, yp, data){
  ys=data$ys
  yp=data$yp

  ts=data.frame(ys,yp)
  rownames(ts)=data$id

  #Drought stress idexes calculation
  SI=(1-mean(ts$ys)/mean(ts$yp))
  ts$ssi=(1-ts$ys/ts$yp)/SI                 #Stress susceptibility index (SSI)
  ts$tol=ts$yp-ts$ys                        #Stress tolerance index (TOL)

  ts$mp=(ts$ys+ts$yp)/2                     #Mean production index (MP)
  ts$gmp=sqrt(ts$ys*ts$yp)                  #Geometric mean productivity index (GMP)
  ts$sti=(ts$ys*ts$yp)/(mean(ts$yp)^2)      #Stress tolerance index (STI)

  #Thiry's scores calculation
  ssi=ThiryScore(ts$ssi,inv = TRUE)
  tol=ThiryScore(ts$tol,inv = TRUE)
  mp=ThiryScore(ts$mp)
  gmp=ThiryScore(ts$gmp)
  sti=ThiryScore(ts$sti)


  ts$s_ssi=ssi$score$score
  ts$s_tol=tol$score$score
  ts$s_mp=mp$score$score
  ts$s_gmp=gmp$score$score
  ts$s_sti=sti$score$score

  #Thiry's combinations: Addition
  ts$c1=(ts$s_ssi+ts$s_mp)/2
  ts$c2=(ts$s_ssi+ts$s_gmp)/2
  ts$c3=(ts$s_ssi+ts$s_sti)/2
  ts$c4=(ts$s_tol+ts$s_mp)/2
  ts$c5=(ts$s_tol+ts$s_gmp)/2
  ts$c6=(ts$s_tol+ts$s_sti)/2

  #Thiry's combinations: Substraction
  ts$c7=(ts$s_mp-ts$s_ssi)/2
  ts$c8=(ts$s_gmp-ts$s_ssi)/2
  ts$c9=(ts$s_sti-ts$s_ssi)/2
  ts$c10=(ts$s_mp-ts$s_tol)/2
  ts$c11=(ts$s_gmp-ts$s_tol)/2
  ts$c12=(ts$s_sti-ts$s_tol)/2

  #Thiry's combinations: By group
  ts$c13=(ts$s_ssi+ts$s_tol)/2+(ts$s_mp+ts$s_gmp)/2
  ts$c14=(ts$s_ssi+ts$s_tol)/2+(ts$s_mp+ts$s_sti)/2
  ts$c15=(ts$s_ssi+ts$s_tol)/2+(ts$s_gmp+ts$s_sti)/2
  ts$c16=(ts$s_mp+ts$s_gmp)/2-(ts$s_ssi+ts$s_tol)/2
  ts$c17=(ts$s_mp+ts$s_sti)/2-(ts$s_ssi+ts$s_tol)/2
  ts$c18=(ts$s_gmp+ts$s_sti)/2-(ts$s_ssi+ts$s_tol)/2


  df1=round(ts[,3:7],2)                                              #Drought indexes output
  df2=ts[,8:12]                                                      #Thiry's scores output
  colnames(df2)=c("ssi","tol","mp","gmp","sti")
  df3=cor(ts[,3:7],ts[,8:12])                                        #Output of Pearson correlation between indexes vs. scores
  df4=list("ssi"=ssi$range,"tol"=tol$range,                          #Output of the range of Thiry's scores
           "mp"=mp$range,"gmp"=gmp$range,"sti"=sti$range)
  df5=ts[,13:30]                                                     #Thiry's combinations output
  df6=t(cor(ts[,1:2],ts[,13:30]))                                    #Output of Pearson correlation between yiels (ys & yp) vs. Thiry's combinations
  df6=as.data.frame(df6)

  xp=which.max(df6$yp)
  xs=which.max(df6$ys)

  df7=data.frame("r2"=c(df6$ys[xs],df6$yp[xp]),                      #Output of the best Thiry's combinatios for ys and yp
                 "Comb."=c(rownames(df6)[xs],rownames(df6)[xp]))
  rownames(df7)=c("YSSI","YPSI")

  output <- list("indexes"=df1,"scores"=df2,"corr1."=df3,                #General output
             "range"=df4,"comb1."=df5,"corr2."=df6,"comb2."=df7)
}
