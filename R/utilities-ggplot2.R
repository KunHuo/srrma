theme_sci <- function(font.size = 12,
                      font.family = "serif",
                      line.size = 0.25,
                      legend.key.size = 1,
                      face.bold = FALSE,
                      panel.grid.major = FALSE,
                      panel.grid.minor = FALSE,
                      panel.border = FALSE,
                      panel.spacing = 0.6,
                      strip.background = "gray90",
                      aspect.ratio = NULL, ...) {

  face <- ifelse(face.bold, "bold", "plain")

  if(panel.grid.major){
    pg.major = ggplot2::element_line(color = "gray90", size = line.size)
  }else{
    pg.major = ggplot2::element_blank()
  }

  if(panel.grid.minor){
    pg.minor = ggplot2::element_line(color = "gray90", size = line.size, linetype = "dashed")
  }else{
    pg.minor = ggplot2::element_blank()
  }


  if(panel.border){
    pborder = ggplot2::element_rect(color = "black", size = line.size)
  }else{
    pborder = ggplot2::element_rect(color = "NA")
  }

  ggplot2::theme_bw(
    base_size = font.size,
    base_family = font.family,
    base_line_size = line.size,
    base_rect_size = line.size) +

    ggplot2::theme(
      panel.background = ggplot2::element_rect(fill = NA),
      panel.grid = ggplot2::element_blank(),
      panel.border = pborder,
      panel.grid.major = pg.major,
      panel.grid.minor = pg.minor,
      panel.spacing = ggplot2::unit(panel.spacing, "cm"),

      strip.background = ggplot2::element_rect(fill = strip.background, size = line.size),

      axis.line = ggplot2::element_line(size = line.size, color = "black",lineend = "square"),
      axis.ticks.length = ggplot2::unit(0.12, "cm"),
      axis.ticks = ggplot2::element_line(color = "black", size = line.size),
      axis.text = ggplot2::element_text(color = "black", size = font.size),
      axis.title = ggplot2::element_text(color = "black", size = font.size, face = face),

      legend.background = ggplot2::element_rect(fill = "NA"),
      legend.text = ggplot2::element_text(color = "black", size = font.size),
      legend.title = ggplot2::element_text(face = face),
      legend.key.size = ggplot2::unit(legend.key.size, "lines"),

      plot.title = ggplot2::element_text(size = font.size + 2, face = face),
      plot.title.position = "plot",
      plot.margin = ggplot2::unit(c(0.4, 1.0, 0.4, 0.4), "cm"), # top, right, bottom, left

      strip.text = ggplot2::element_text(color = "black", size = font.size, face = face),
      aspect.ratio = aspect.ratio,
      complete = FALSE,
      ...
    )
}


.is_waiver <- function(value){
  class(value) == "waiver"
}


legend_title <- function(value = NULL){
  if(length(value) == 0L){
    ggplot2::theme(
      legend.title = ggplot2::element_blank()
    )
  }else{
    ggplot2::labs(color = value, fill = value, alpha = value, size = value, linetype = value)
  }
}


legend_position <- function(position) {
  if(length(position) == 0L){
    ggplot2::theme(
      legend.position = "none"
    )
  }else{
    if(is.character(position)){
      ggplot2::theme(
        legend.position = position
      )
    }else{
      ggplot2::theme(
        legend.position = position,
        legend.justification = position
      )
    }
  }
}


rotate_x_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle > 5)
    hjust <- 1
  if (missing(vjust) & angle == 90)
    vjust <- 0.5
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


rotate_y_text <- function (angle = 45, hjust = NULL, vjust = NULL, ...) {
  if (missing(hjust) & angle == 90)
    hjust <- 0.5
  else if (missing(hjust) & angle > 5)
    hjust <- 1
  ggplot2::theme(axis.text.y = ggplot2::element_text(angle = angle, hjust = hjust, vjust = vjust, ...))
}


.pretty_xbreaks <- function(plot, x.breaks = NULL, x.breaks.n = 5, zero = FALSE, facet = NULL){
  if(is.null(x.breaks)){
    if(!is.null(facet)){
      plot <- plot + ggplot2::facet_wrap(facet)
    }
    gdata <- ggplot2::ggplot_build(plot)$data[[1]]
    x.breaks <- gdata[["x"]]
    if(zero){
      x.breaks <- c(0, x.breaks)
    }
    x.breaks <- pretty(x.breaks, x.breaks.n)
  }
  x.breaks
}


.pretty_ybreaks <- function(plot, y.breaks = NULL, y.breaks.n = 5, zero = FALSE, facet = NULL){
  if(is.null(y.breaks)){
    if(!is.null(facet)){
      plot <- plot + ggplot2::facet_wrap(facet)
    }
    gdata <- ggplot2::ggplot_build(plot)$data[[1]]
    y.breaks <- gdata[["y"]]

    if(zero){
      y.breaks <- c(0, y.breaks)
    }

    y.breaks <- pretty(y.breaks, y.breaks.n)
  }
  y.breaks
}


.set_legend_title <- function(plot, title, data, group){
  if(.is_waiver(title)){
    label <- attr(data[[group]], "label")
    if(is.null(label)){
      plot
    }else{
      plot + legend_title(label)
    }
  }else{
    plot + legend_title(title)
  }
}


.set_legend_position <- function(plot, position){
  if(.is_waiver(position)){
    plot + legend_position("right")
  }else{
    plot + legend_position(position)
  }
}
