library(ggtext)

theme_mt <- function(base_size = 17,
                     base_family = "Roboto Condensed",
                     plot_title_family = base_family,
                     subtitle_family = base_family,
                     strip_text_family = base_family,
                     axis_title_family = base_family,
                     axis_text_family = base_family,
                     caption_family = base_family,
                     plot_title_size = base_size + 8,
                     axis_text_size = base_size + 2,
                     strip_text_size = base_size,
                     subtitle_size = base_size + 3,
                     caption_size = base_size - 3,
                     axis_title_size = base_size + 2,
                     grid_color = "gray80",
                     axis_color = FALSE, # "gray60"
                     axis_text_color = "gray30") {
    ggplot2::theme_minimal(
        base_family = base_family,
        base_size = base_size
    ) %+replace%
        theme(
            axis.line = element_line(
                color = ifelse(axis_color, "gray60", "transparent"),
                size = 0.5
            ),
            axis.text = element_markdown(
                size = axis_text_size,
                family = axis_text_family,
                margin = margin(t = 0, r = 0)
            ),
            axis.text.x = element_markdown(
                size = axis_text_size,
                color = axis_text_color,
                family = axis_text_family,
                margin = margin(t = 0)
            ),
            axis.text.y = element_markdown(
                size = axis_text_size,
                color = axis_text_color,
                family = axis_text_family,
                margin = margin(r = 0)
            ),
            axis.ticks = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title = element_text(
                size = axis_title_size,
                family = axis_title_family
            ),
            axis.title.x = element_markdown(
                hjust = 1,
                size = axis_title_size,
                margin = margin(10, 10, 10, 10),
                family = axis_title_family,
                face = "plain"
            ),
            axis.title.y = element_markdown(
                hjust = 1,
                vjust = 1.5,
                size = axis_title_size,
                margin = margin(10, 10, 10, 10),
                angle = 90,
                family = axis_title_family,
                face = "plain"
            ),
            axis.title.y.right = element_markdown(
                hjust = 1,
                size = axis_title_size,
                angle = 90,
                family = axis_title_family,
                face = "plain"
            ),
            legend.background = element_blank(),
            legend.direction = "horizontal",
            legend.key = element_blank(),
            legend.key.size = unit(0.7, "cm"),
            legend.margin = margin(-base_size, 0, 0, 0, "pt"),
            legend.position = "bottom",
            legend.text = element_text(size = base_size),
            legend.title = element_text(size = base_size + 2),
            plot.caption = element_markdown(
                hjust = 1,
                size = caption_size,
                margin = margin(t = 10),
                family = caption_family,
                face = "plain"
            ),
            panel.grid = element_blank(),
            panel.grid.major = element_line(color = grid_color, size = 0.2),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_line(color = grid_color, size = 0.1),
            panel.grid.minor.x = element_blank(),
            panel.spacing = unit(3, "lines"),
            plot.margin = margin(1.5, 1.5, 1.5, 1.5, "lines"),
            plot.subtitle = element_text(
                hjust = 0,
                size = subtitle_size,
                margin = margin(b = 10),
                family = subtitle_family,
                face = "plain"
            ),
            plot.title = element_text(
                hjust = 0,
                size = plot_title_size,
                margin = margin(b = 5),
                family = plot_title_family,
                face = "bold"
            ),
            strip.background = element_blank(),
            strip.placement = "outside",
            strip.text = element_markdown(
                margin = margin(),
                size = strip_text_size,
                face = "plain",
                family = strip_text_family
            )
        )
}

theme_set(theme_mt())
