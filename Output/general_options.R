## These lines set several options
#options(scipen = 999) # Do not print scientific notation
options(stringsAsFactors = FALSE) ## Do not load strings as factors
options("lfe.threads"=12)
getOption("lfe.threads")

gg_save_width <- 8
gg_save_height <- 5

gg_width_Tex_third <- 6.3
gg_height_Tex_third <- 2

gg_width_Tex_half <- 6.3
gg_height_Tex_half <- 3.3


library(tidyverse)
font_plot = "Arial Narrow" #"CMU Serif"
theme_jo <-   theme(plot.title = element_text(hjust=0, size=30, margin=margin(b=10), family=font_plot, face="bold"),
                    plot.subtitle = element_text(hjust=0, size=26, margin=margin(b=15), family=font_plot, face="plain"),
                    plot.caption=element_text(hjust=1, size=22, margin=margin(t=10), family=font_plot, face="italic"),
                    text=element_text(family=font_plot),
                    axis.text.x=element_text(size=24, margin=margin(t=0)),
                    axis.text.y=element_text(size=24, margin=margin(r=0)),
                    axis.title=element_text(size=24, family=font_plot),
                    axis.title.x=element_text(size=24, family=font_plot, face="plain"), #hjust=xj,
                    axis.title.y=element_text(size=24, family=font_plot, face="plain"), #hjust=yj, 
                    axis.title.y.right=element_text(size=24, angle=90, family=font_plot, face="plain"), #hjust=yj,
                    strip.text=element_text(hjust=.5, size=20, face="plain", family=font_plot),
                    plot.margin=margin(30, 30, 30, 30),
                    #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                    legend.title=element_text(size=22),
                    legend.text=element_text(size=22),
                    legend.position="bottom",
                    legend.box = "horizontal",
                    panel.background = element_rect(fill = "white"),
                    panel.grid=element_line(color="#cccccc", size=0.2),
                    panel.grid.major=element_line(color="#cccccc", size=0.2),
                    panel.grid.minor=element_line(color="#cccccc", size=0.15))  

fips_num_to_char <- function(x) str_pad(x, width=2, side = "left", pad = "0")
if(FALSE) {
  fips_num_to_char(c(1, 10, 100))
}



theme_jo77 <-   theme(plot.title = element_text(hjust=0, size=20, margin=margin(b=10), family=font_plot, face="bold"),
                      plot.subtitle = element_text(hjust=0, size=16, margin=margin(b=15), family=font_plot, face="plain"),
                      plot.caption=element_text(hjust=1, size=12,margin=margin(t=10), family=font_plot, face="italic"),
                      text=element_text(family=font_plot),
                      axis.text.x=element_text(size=12, margin=margin(t=0)),
                      axis.text.y=element_text(size=12, margin=margin(r=0)),
                      axis.title=element_text(size=12, family=font_plot),
                      axis.title.x=element_text(size=12, family=font_plot, face="plain"), #hjust=xj,
                      axis.title.y=element_text(size=12, family=font_plot, face="plain"), #hjust=yj, 
                      axis.title.y.right=element_text(size=12, angle=90, family=font_plot, face="plain"), #hjust=yj,
                      strip.text=element_text(hjust=.5, size=12, face="plain", family=font_plot),
                      plot.margin=margin(30, 30, 30, 30),
                      #plot.margin = unit(c(0.05,0.05,0.05,0.05), "inches"),
                      legend.title=element_text(size=12),
                      legend.text=element_text(size=12),
                      legend.position="bottom",
                      legend.box = "horizontal",
                      panel.background = element_rect(fill = "white"),
                      panel.grid=element_line(color="#cccccc", size=0.2),
                      panel.grid.major=element_line(color="#cccccc", size=0.2),
                      panel.grid.minor=element_line(color="#cccccc", size=0.15))

#guides(color = guide_legend(override.aes = list(fill = "white"))) 
  