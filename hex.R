plt_hex <- plot_mean +


  geom_errorbar(aes(ymin = Cred.lb,
                    ymax = Cred.ub),
                width = 0,
                size = 2,
                alpha = 0.85) +
  geom_point(size = 8,
             color = "#CC79A7", alpha = 0.15) +
  geom_point(size = 6,
             color = "#CC79A7") +
  theme_minimal() +
  ylab("") + xlab("") +
  theme( axis.ticks  = element_line(size = 2, colour = "grey"), axis.text = element_blank(),
         axis.ticks.length = unit(0.2, "cm"), panel.grid = element_line(linetype = "dashed"),
         panel.grid.minor.x= element_blank(), panel.grid.major.x = element_blank() )

library(hexSticker)
library(showtext)
## Loading Google fonts (http://www.google.com/fonts)
font_add_google("Special Elite", "gochi")

s <- sticker(plt_hex, p_family = "gochi",
             package="vICC", p_size=25, s_x=.935, s_y=.75, p_y = 1.5,p_x = .95, p_color = "grey",
             s_width=1.4, s_height=1.2,h_fill = "white",h_size = 2, h_color = "black",
             filename="man/figures/hex_sticker.png")
s
