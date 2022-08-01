# plotting functions ####
makePlot <- function(out_matrix, title = "abundance of species by time", obj = "species", y.label = "x.t"){
  df <- as.data.frame(out_matrix)
  dft <-  melt(df, id="time")
  names(dft)[2] = obj
  names(dft)[3] = y.label
  lgd = ncol(df)<= 20
  ggplot(dft, aes_string(names(dft)[1], names(dft)[3], col = names(dft)[2])) +
    geom_line(show.legend = lgd, lwd=0.5) +
    ggtitle(title) +
    theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

makePlotRes <- function(out_matrix, title = "quantity of compounds by time"){
  df <- as.data.frame(out_matrix)
  dft <-  melt(df, id="time")
  names(dft)[2] = "resources"
  names(dft)[3] = "S.t"
  lgd = ncol(df)<= 20
  ggplot(dft, aes(time, S.t, col = resources)) +
    geom_line(show.legend = lgd, lwd=0.5) +
    ggtitle(title) +
    theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14))
}

makePiePlot <- function(multinomdist, label = 'Meta\ncommunity', title = "Metacommunity \nspecies abundance\n"){
  df <- data.frame(group = seq(length(multinomdist)), probability = multinomdist)
  fig <- ggplot(df, aes(x=group,y=1,fill=probability, )) +
    geom_tile(colour="#edfaf9",size=0.005) +
    theme(axis.title = element_blank()) +
    scale_fill_gradient2(label, low = "white", high = "magenta3", midpoint = max(multinomdist)/8) +
    theme_void() +
    coord_fixed(ratio = length(multinomdist)/4) +
    ggtitle(title) # + theme_linedraw()
  fig
}

makeHeatmap <-function(matrix.A,
                       title = "Consumption/production matrix",
                       y.label = 'resources',
                       x.label = 'species',
                       midpoint_color = NULL,
                       lowColor = "red",
                       midColor = "white",
                       highColor = "blue"){
  df <- melt(t(matrix.A))
  if (is.null(midpoint_color)) {
    midpoint_color <- 0
  }
  names(df)<- c("x", "y", "strength")
  df$y <- factor(df$y, levels=rev(unique(sort(df$y))))
  fig <- ggplot(df, aes(x,y,fill=strength)) + geom_tile() + coord_equal() +
    theme(axis.title = element_blank()) +
    scale_fill_gradient2('strength', low = lowColor, mid = midColor, high = highColor, midpoint = midpoint_color)+
    theme_void() + ggtitle(title)

  if (ncol(matrix.A)<=10 & nrow(matrix.A)<=10){
    fig <- fig + geom_text(aes(label = round(strength, 2)))
  } else if (ncol(matrix.A)<=15 & nrow(matrix.A)<=15){
    fig <- fig + geom_text(aes(label = round(strength, 1)))
  } else {
    fig <- fig
  }

  fig <- fig + labs(x = x.label, y = y.label)+
    theme_linedraw() +
    theme(plot.title = element_text(hjust = 0.5, size = 14), axis.text.x = element_text(
      angle = 90))

  if (nrow(matrix.A) >= 20){
    # too many species
    fig <- fig + theme(
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
    )
  }
  if (ncol(matrix.A) >= 20){
    # too many resources
    fig <- fig + theme(
      axis.title.x=element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank()
    )
  }
  fig
}

makeRegression <- function (fit) {

  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 2),
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Slope =",signif(fit$coef[[2]], 2),
                       " P =",signif(summary(fit)$coef[2,4], 2)),)
}

#' @importFrom umap umap umap.defaults
makeUMAP <- function(matrix, n_neighbors=10, min_dist=0.1, gradient=NULL, gradient_title = 'gradient', group=NULL, group2=NULL){
  custom.config = umap.defaults
  custom.config$n_neighbors = n_neighbors
  custom.config$min_dist = min_dist

  df <- as.data.frame(umap(matrix,config = custom.config)$layout)
  df$gradient <- gradient

  if (is.null(gradient)){
    df$gradient <- 1

  }
  colnames(df) = c('UMAP_2', 'UMAP_1', gradient_title)
  if (is.null(group)){
    ggplot(df, aes_string('UMAP_2', 'UMAP_1', color=gradient_title)) +
      geom_point() +
      scale_color_gradient(low="blue", high="red")
  } else {
    if (is.null(group2)){
      ggplot(df, aes_string('UMAP_2', 'UMAP_1', color=gradient_title)) +
        geom_point(aes(color = group)) + theme_bw()
    } else {
      ggplot(df, aes_string('UMAP_2', 'UMAP_1', color=gradient_title)) +
        geom_point(aes(color = group, shape = group2)) + theme_bw()
    }
  }
}

# converting functions ####
text2char <- function(text){
  # split words or numbers by separators(',' and ';')
  if(trimws(text) == "") {
    return(NULL)
  } else {
    return(strsplit(x = trimws(text), split = "\\,+\\s+|\\s+\\,+|\\,+|\\;+\\s+|\\s+\\;+|\\;+")[[1]])
  }
}

text2chars <- function(text, len, prefix = NULL, expr = NULL){
  # split words or numbers by separators(',' and ';')
  # if length not enough, generate new values using expr.
  # used for automatic names of species/compounds(resources) with prefix
  # used for random numbers of initial abundances / growth rates with expr
  text <- text2char(text)
  if (length(text) < len){
    if (is.null(prefix) & is.null(expr)){
      stop("'prefix' or 'expr' not provided to function 'text2chars'")
    } else if (is.null(expr)){
      return(c(text, paste0(prefix, seq_len(len))[(length(text)+1):len]))
    } else if (is.null(prefix)){
      return(c(text, eval(parse(text = expr)))[1:len])
    }
  } else if (length(text) > len){
    warning("length of text provided to 'textchars' more than needed")
    return(text[1:len])
  } else {
    return(text)
  }
}


