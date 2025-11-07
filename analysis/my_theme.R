my_theme <- function() {
  theme(
    # all lines
    line = element_line(linewidth=1),
    # axes
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14.5),
    axis.ticks=element_line(linewidth=1),
    axis.ticks.length=unit(.125, "cm"),
    # font
    text = element_text(family='sans', 
                        face='plain',
                        color='black',
                        size=14),
    # legend
    legend.key=element_rect(fill="transparent"),
    legend.position='none',
    legend.text=element_text(size=12.5),
    legend.title=element_text(size=14),
    # panel
    panel.background=element_rect(fill='white', 
                                  colour='white'),
    # facet stips
    strip.background=element_blank(),
    strip.text = element_text(size = 14.5)
  )
}
