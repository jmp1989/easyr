



theme_Publication = function(legend_position = 'right',legend.direction='vertical', base_size=14, base_family="", ...){

    theme_Publication <- function( ...) {

        library(grid)
        library(ggthemes)


        # if(hide_legend==FALSE){
        #     legend_position = 'right'
        # }
        # if(hide_legend==TRUE){
        #     legend_position='none'
        # }

        (theme_foundation(base_size=base_size, base_family=base_family)
            + theme(plot.title = element_text(face = "bold",
                                              size = rel(1.25)),
                    text = element_text(),
                    panel.background = element_rect(colour = NA),
                    plot.background = element_rect(colour = NA),
                    panel.border = element_rect(colour = NA),
                    axis.title = element_text(face = "bold",size = rel(1)),
                    axis.title.y = element_text(angle=90,vjust =2),
                    axis.title.x = element_text(vjust = -0.2),
                    axis.text.x = element_text(angle=90),
                    axis.text = element_text(),
                    axis.line = element_line(colour="black"),
                    axis.ticks = element_line(),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    legend.key = element_rect(colour = NA),
                    legend.position = legend_position,
                    legend.direction = legend.direction,
                    legend.key.size=  unit(0.2, "cm"),
                    #legend.margin = unit(0, "cm"),
                    legend.title=element_blank(),
                    #legend.title = element_text(face="italic"),
                    plot.margin=unit(c(10,5,5,5),"mm"),
                    strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
                    strip.text = element_text(face="bold")
            ))  #+ list(guides(color = guide_legend(override.aes = list(size = 12))) )







    }


    list(guides(color = guide_legend(override.aes = list(size = 11))),

    theme_Publication(),

    theme(...))
}



scale_fill_Publication <- function(...){
  library(scales)
  discrete_scale("fill","Publication",manual_pal(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")), ...)

}

scale_colour_Publication <- function(...){
  library(scales)
  discrete_scale("colour","Publication",manual_pal(values = c("#1f77b4", "#ff7f0e", "#2ca02c", "#d62728", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")), ...)
}




