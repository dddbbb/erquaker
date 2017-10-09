GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                             required_aes = c("x", "label"),
                             default_aes = ggplot2::aes(n_max = 5),
                             draw_key = ggplot2::draw_key_text,

                             draw_group = function(data, panel_params, coord) {
                               coords <- coord$transform(data, panel_params)
                               subdata <- dplyr::arrange(coords, dplyr::desc(size))
                               subdata <- subdata[1:data$n_max[1],]

                               labels <- grid::textGrob(
                                 subdata$label,
                                 subdata$x,
                                 subdata$y+0.08,
                                 rot = 45,
                                 just="left"
                               )
                               forlines<-data.frame(id = rep(1:data$n_max[1],2),x = subdata$x, y = c(subdata$y, subdata$y+0.08))
                               lines<-grid::polylineGrob(
                                 x = forlines$x,
                                 y = forlines$y,
                                 id = forlines$id,
                                 gp = grid::gpar(col = "grey")
                               )
                               grid::gList(lines, labels)

                             }
)

#' Geom Timeline Label
#'
#' Function plots Labels for earthquakes.
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
#' @return ggplot layer with labels
#' @export
#' @importFrom ggplot2 ggproto layer draw_key_text aes
#' @importFrom grid gpar gList polylineGrob textGrob gpar
#' @examples
#' # d%>%subset(COUNTRY=="USA" | COUNTRY=="CHINA")%>%subset(DATE>=as.Date("2000-01-01"))%>%
#' # ggplot(aes(DATE, COUNTRY, colour = DEATHS, size = EQ_PRIMARY, label = LOCATION_NAME)) +
#' #  geom_timeline()+ geom_timeline_label(n_max=5)+
#' #   guides(size = guide_legend("Richter scale value")) +
#' #   guides(colour = guide_colorbar("# Deaths", label.theme = element_text(angle = 45, size = 8 ))) +
#' #   ylab("") +
#' #   theme_classic() +
#' #   theme(legend.position="bottom", axis.line.y = element_blank())
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE, show.legend = NA,
                                inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,  data = data, stat = stat,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )

}
