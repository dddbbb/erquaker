GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                           required_aes = c("x"),
                           default_aes = ggplot2::aes(shape = 19, size=1, color = "black", alpha = 0.5, stroke = 1, fill=NA),
                           draw_key = ggplot2::draw_key_point,

                           draw_group = function(data, panel_params, coord) {
                             coords <- coord$transform(data, panel_params)
                             coords$size[is.na(coords$size)]<-1
                             coords$colour[is.na(coords$colour)]<-1
                             lines<-grid::linesGrob(
                               unit(c(.05, .95), "npc"),
                               coords$y,
                               gp = grid::gpar(col = "grey")
                             )

                             points<-grid::pointsGrob(
                               coords$x, coords$y,
                               pch = coords$shape,
                               gp = grid::gpar(
                                 col = coords$colour,
                                 alpha = coords$alpha,
                                 fontsize = coords$size * .pt + coords$stroke * .stroke / 2)


                             )
                             grid::gList(lines, points)


                           }
)

#' Geom Timeline
#'
#' Function plots timeline with circle point of earthquakes
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#' @param ... ...
#'
#' @return ggplot layer with timeline aes draw_key_point
#' @export
#' @importFrom ggplot2 ggproto layer
#' @importFrom grid gpar gList pointsGrob linesGrob gpar
#' @examples
#'library(magrittr)
#'library(dplyr)
#'d%>%subset(COUNTRY=="USA" | COUNTRY=="CHINA")%>%subset(DATE>=as.Date("2000-01-01"))%>%
#' ggplot(aes(DATE, COUNTRY, colour = DEATHS, size = EQ_PRIMARY, label = LOCATION_NAME)) +
#'  geom_timeline()+ geom_timeline_label(n_max=5)+
#'   guides(size = guide_legend("Richter scale value")) +
#'   guides(colour = guide_colorbar("# Deaths", label.theme = element_text(angle = 45, size = 8 ))) +
#'   ylab("") +
#'   theme_classic() +
#'   theme(legend.position="bottom", axis.line.y = element_blank())

geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA,
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}


