# settings.R

# options(hrbrthemes.loadfonts = TRUE,
#         fig.retina = 3
# )
# theme_set(theme_ipsum_ps() + #base_size = 10
#             theme(panel.grid.major = element_blank(),
#                   panel.grid.minor = element_blank(),
#                   plot.title.position = "plot",
#                   plot.caption.position = "plot"
#             )
# )

theme_set(theme_light() + #base_size = 10
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  #axis.line.x = element_line(color = "black", size = 0.15),
                  axis.ticks = element_blank(),
                  plot.title.position = "plot",
                  plot.caption.position = "plot"
            )
)
