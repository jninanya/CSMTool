#' Plotting PCA and Cluster results
#'
#' This function plot the results of the PCA and clustering analysis.
#'
#' @param res.pca Output of the PCA function.
#' @param res.hcpc Output of the HCPC function.
#' @param show_pca Choose to plot \code{var}, \code{ind} or \code{both}.
#' @param plot A logical value.
#'
#' @author Johan Ninanya, Javier Rinza
#'
#'
#'
#' @examples
#' plot_pca(res.pca, res.hcpc, show_pca = "var")
#' plot_pca(res.pca, res.hcpc, show_pca = "ind")
#' plot_pca(res.pca, res.hcpc, show_pca = "both")
#'
#' @importFrom FactoMiner PCA HCPC
#' @importFrom basicPlotteR addTextLabels
#' @export

plot_pca <- function(res.pca,
                     res.hcpc,
                     show_pca = c("var", "ind", "both"),
                     plot = TRUE){


  show_pca <- match.arg(show_pca)

  nc <- res.hcpc$call$t$nb.clust
  color_list <- c("black", "blue", "green", "red", "deepskyblue",
                  "pink", "cyan", "gray70", "chocolate", "darckorchid")
  xcolor <- vector()
  for(i in 1:nrow(res.hcpc$data.clust)){
    xcolor[i] <- color_list[res.hcpc$data.clust$clust[i]]
  }

  leg_color <- color_list[1:nc]

  comp1 <- round(res.pca$eig[1,2], 1)
  comp2 <- round(res.pca$eig[2,2], 1)

  var <- res.pca$var$coord
  ind <- res.pca$ind$coord

  if(show_pca == "var"){

    par(mar=c(5,5,1,1))
    plot(var[,1],var[,2], las = 1, col = "black",
         xlim = c(-1.2,1.2), ylim = c(-1.2,1.2), pch = "",
         xlab = paste0("Comp. 1 (", comp1, " %)"),
         ylab = paste0("Comp. 2 (", comp2, " %)"))

    arrows(0, 0, var[,1], var[,2], col = "red", lwd = 1.5)
    addTextLabels(var[,1], var[,2], rownames(var), col.label = "red", cex.label = 1.2)
    #text(var[,1], var[,2], rownames(var), pos = 3, cex = 1.2)
    abline(v=0,h=0,col = "gray50", lty = 2)

  }

  if(show_pca == "ind"){

    r1 <- floor(range(ind[,1:2])[1])
    r2 <- ceiling(range(ind[,1:2])[2])
    rr <- r2-r1

    par(mar=c(5,5,1,6))
    plot(ind[,1], ind[,2], las=1, pch = 20, col = xcolor,
         xlim = c(r1,r2), ylim = c(r1, r2),
         xlab = paste0("Comp. 1 (", comp1, " %)"),
         ylab = paste0("Comp. 2 (", comp2, " %)"))

    addTextLabels(ind[,1], ind[,2], labels = rownames(ind), col.label = xcolor, cex.label = 0.7, col.line = "gray80", lwd = 1, lty = 2)
    text(ind[,1], ind[,2], rownames(ind), cex = 0.7, col = xcolor)

    abline(v=0,h=0,col = "gray50", lty = 2)

    par(xpd=TRUE)
    legend(x = r2+0.06*rr,
           y = r2-0.05*rr,
           legend = paste0("Cluster ", 1:nc),
           col = leg_color,
           pch = 20, cex = 0.8)
    par(xpd=FALSE)

  }


  if(show_pca == "both"){

    r1 <- floor(range(ind[,1:2])[1])
    r2 <- ceiling(range(ind[,1:2])[2])
    rr <- r2-r1

    par(mar=c(5,5,1,6))
    plot(ind[,1], ind[,2], las=1, pch = 20, col = xcolor,
         xlim = c(r1,r2), ylim = c(r1, r2),
         xlab = paste0("Comp. 1 (", comp1, " %)"),
         ylab = paste0("Comp. 2 (", comp2, " %)"))

    addTextLabels(ind[,1], ind[,2], labels = rownames(ind), col.label = xcolor, cex.label = 0.7, col.line = "gray80", lwd = 1, lty = 2)
    #text(ind[,1], ind[,2], rownames(ind), cex = 0.7, col = xcolor)

    abline(v=0,h=0,col = "gray50", lty = 2)

    par(xpd=TRUE)
    legend(x = r2+0.06*rr,
           y = r2-0.05*rr,
           legend = paste0("Cluster ", 1:nc),
           col = leg_color,
           pch = 20, cex = 0.8)
    par(xpd=FALSE)

    par(new = TRUE)
    plot(x = NULL, y = NULL, axes = FALSE,
         xlim = c(-1.2, 1.2), ylim = c(-1.2, 1.2),
         xlab = "", ylab = "")

    arrows(0, 0, var[,1], var[,2], col = "red", lwd = 1.5)
    addTextLabels(var[,1], var[,2], rownames(var), col.label = "red", cex.label = 1.2)
    #text(var[,1], var[,2], rownames(var), pos = 3, cex = 1.2, col = "red")


  }


}





