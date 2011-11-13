ggbigraph <- 
# Args: 
#   g: a igraph graph object or matrix (object)
#   ptsize: point size of nodes (numeric)
#   linesize: point size of nodes (numeric)
#   textsize: point size of nodes (numeric)
#   plot_: plot data as bipartite network (logical)
#   weighted: weight lines width by number of interactions (logical) 
  
function(g, ptsize = 6, linesize = 2, textsize = 6, plot_ = TRUE, 
  weighted = FALSE) {
  
  if(!require(ggplot2)) stop("must first install 'ggplot2' package.")
  if(!require(igraph)) stop("must first install 'igraph' package.")
  if(!require(reshape2)) stop("must first install 'reshape2' package.")
  
  if(class(g)=="matrix"){ 
    g <- as.data.frame(g)
	  temp <- data.frame(expand.grid(dimnames(g))[1:2], as.vector(as.matrix(g)))
	  temp <- temp[(temp[, 3] > 0) & !is.na(temp[, 3]), ]
	  temp <- temp[sort.list(temp[, 1]), ]
	  g_df <- data.frame(rows = temp[, 1], cols = temp[, 2], freqint = temp[, 3])

  	g_df$id <- 1:length(g_df[,1])
	  g_df <- data.frame(id=g_df[,4], rows=g_df[,1], cols=g_df[,2], freqint=g_df[,3])
	  g_df_ <- melt(g_df, id=c(1,4))
	  
    xy_s <- data.frame(
      value = unique(g_df_$value), 
        x = c(rep(1, length(unique(g_df_[g_df_[,3]=="rows",]$value))), 
          rep(2, length(unique(g_df_[g_df_[,3]=="cols",]$value)))),
        y = c(seq(1, length(unique(g_df_[g_df_[,3]=="rows",]$value)), 1), 
          seq(1, length(unique(g_df_[g_df_[,3]=="cols",]$value)), 1)))
	  g_df_2 <- merge(g_df_, xy_s, by = "value")
  } else
  
  {
  g_ <- get.edgelist(g)
  g_df <- as.data.frame(g_)
  g_df$id <- 1:length(g_df[,1])
  g_df <- melt(g_df, id=3)
  xy_s <- data.frame(value = unique(g_df$value), 
    x = c(rep(1, length(unique(g_df$value))/2), rep(4, length(unique(g_df$value))/2)),
    y = rep(seq(1, length(unique(g_df$value))/2, 1), 2))
  g_df_2 <- merge(g_df, xy_s, by = "value") 
  }
  
  if(plot_ == FALSE) {g_df_2} else 
    {  
      if(weighted == FALSE) {
  ggplot(g_df_2, aes(x, y)) +
    geom_point(size = ptsize) +
    geom_line(size = linesize, aes(group = id)) +
    geom_text(size = textsize, hjust = 1.5, aes(label = value)) +
    theme_bw() + 
    opts(panel.grid.major=theme_blank(), 
      panel.grid.minor=theme_blank(), 
      axis.text.x=theme_blank(),
      axis.text.y=theme_blank(),
      axis.title.x=theme_blank(),
      axis.title.y=theme_blank(),
      axis.ticks=theme_blank(),
      panel.border=theme_blank(),
      legend.position="none")
    } else
    {
    ggplot(g_df_2, aes(x, y)) +
    geom_point(size = ptsize) +
    geom_line(aes(group = id, size = freqint)) +
    geom_text(size = textsize, hjust = 1.5, aes(label = value)) +
    theme_bw() + 
    opts(panel.grid.major=theme_blank(), 
      panel.grid.minor=theme_blank(), 
      axis.text.x=theme_blank(),
      axis.text.y=theme_blank(),
      axis.title.x=theme_blank(),
      axis.title.y=theme_blank(),
      axis.ticks=theme_blank(),
      panel.border=theme_blank(),
      legend.position="none")
    }
  }
}

# From an igraph graph object
require(igraph)
g <- grg.game(20, 0.45, torus=FALSE)
a <- ggbigraph(g)

# From a matrix 
setwd("/Mac/R_stuff/Blog_etc/ggbigraph")
mat1 <- read.csv("samplematrix.csv")
g <- as.matrix(mat1[,-1])
dimnames(g)
dimnames(g)[[1]] <- mat1[,1]
g

# different options
ggbigraph(g, linesize = 3, plot_ = FALSE)
ggbigraph(g, linesize = 3, plot_ = TRUE, weighted = TRUE)
ggbigraph(g, plot_ = TRUE, weighted = FALSE)


# layout many 
require(gridExtra)
a <- ggbigraph(g, linesize = 3, textsize = 0, plot_ = TRUE, weighted = TRUE)

  # you can uncomment and use jpeg and dev.off to save to your computer
# jpeg("effsizeplots_bestfit.jpeg", width=680, height=680)
grid.newpage()
vpa_ <- viewport(width = 0.35, height = 0.95, x = 0.17, y = 0.5)
vpb_ <- viewport(width = 0.35, height = 0.95, x = 0.5, y = 0.5)
vpc_ <- viewport(width = 0.35, height = 0.95, x = 0.82, y = 0.5)
print(a, vp = vpa_)
print(a, vp = vpb_)
print(a, vp = vpc_)
# dev.off()
