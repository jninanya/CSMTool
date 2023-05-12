#' Thiry scores computation
#'
#' This function calculates the Thiry scores (Thiry et al. 2016)
#' from yield and tolerance indices data (stress susceptibility index
#' (SSI), tolerance index (TOL), mean productivity index (MP),
#' geometric mean productivity index (GMP), and stress tolerance
#' index (STI)).
#'
#' @param x A vector of values of some of the yield and tolerance
#' indeces suggested by Thiry et al. (2016).
#' @param inv A logical value. If \code{TRUE} data values will be
#' inverted, i.e., higher values will turn into lower values,
#' and vice versa. Defaul to \code{FALSE}.
#'
#' @details Use \code{inv = TRUE} only for the SSI and TOL indices.
#'
#' @return It returns a list including:
#' \itemize{\code{score}} A data frame object with the original value
#' (\code{x}) and its respective Thiry score (\code{score}).
#' \itemize{\code{order}} A data frame object.
#' \itemize{\code{range}} A data frame with the amount of data per score.
#' \itemize{\code{corr}} Correlation between the index and its score.
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
#' @seealso \code{Thiry}
#'
#' @examples
#' x <- runif(50)
#' ts <- ThiryScore(x)
#' ts$range
#' ts <- ThiryScore(x, inv = TRUE)
#' ts$range
#'
#' @importFrom stats cor
#' @export

ThiryScore <- function(x, inv = FALSE){

  xrange = range(x)

  # scores
  df1 = data.frame("x" = x)
  df1$score = NA

  # data range
  df3 = data.frame("score" = 1:10)
  df3$range = NA
  df3$count = NA

  # Score calculation for SSI and TOL
  if(inv == TRUE){
    r = seq(xrange[2], xrange[1], length = 11)

    for(i in 1:length(x)){
      ix = x[i]
      if(ix <= r[1] & ix > r[2]){
        df1$score[i] = 1
      }else if(ix <= r[2] & ix > r[3]){
        df1$score[i] = 2
      }else if(ix <= r[3] & ix > r[4]){
        df1$score[i] = 3
      }else if(ix <= r[4] & ix > r[5]){
        df1$score[i] = 4
      }else if(ix <= r[5] & ix > r[6]){
        df1$score[i] = 5
      }else if(ix <= r[6] & ix > r[7]){
        df1$score[i] = 6
      }else if(ix <= r[7] & ix > r[8]){
        df1$score[i] = 7
      }else if(ix <= r[8] & ix > r[9]){
        df1$score[i] = 8
      }else if(ix <= r[9] & ix > r[10]){
        df1$score[i] = 9
      }else if(ix <= r[10] & ix >= r[11]){
        df1$score[i] = 10
      }
    }
    df2 <- df1[order(df1$x, decreasing = TRUE),]

    # Score calculation for MP, GMP and DTI
  }else if(inv == FALSE){
    r = seq(xrange[1], xrange[2], length = 11)

    for(i in 1:length(x)){
      ix = x[i]
      if(ix >= r[1] & ix < r[2]){
        df1$score[i] = 1
      }else if(ix >= r[2] & ix < r[3]){
        df1$score[i] = 2
      }else if(ix >= r[3] & ix < r[4]){
        df1$score[i] = 3
      }else if(ix >= r[4] & ix < r[5]){
        df1$score[i] = 4
      }else if(ix >= r[5] & ix < r[6]){
        df1$score[i] = 5
      }else if(ix >= r[6] & ix < r[7]){
        df1$score[i] = 6
      }else if(ix >= r[7] & ix < r[8]){
        df1$score[i] = 7
      }else if(ix >= r[8] & ix < r[9]){
        df1$score[i] = 8
      }else if(ix >= r[9] & ix < r[10]){
        df1$score[i] = 9
      }else if(ix >= r[10] & ix <= r[11]){
        df1$score[i] = 10
      }
    }

    df2 <- df1[order(df1$x),]

  }

  # range output
  rng = paste0("[", round(r[1:9],2), " - ", round(r[2:10],2), ">")
  rng[10] = paste0("[", round(r[10],2), " - ", round(r[11],2), "]")

  df3$range <- rng

  #Count output
  for(k in 1:10){
    df3$count[k] = sum(df1$score == k)
  }

  # Correlation between the score and index
  df4 <- cor(df1$score, df1$x)

  #Output
  output <- list("score" = df1,
                 "order" = df2,
                 "range" = df3,
                 "corr" = df4)

}
