require(ggplot2)
require(hexSticker)
require(svglite)
require(pkgdown)

R_blue <- rgb(22, 92, 170, maxColorValue = 255)
R_blue2 <- rgb(39, 109, 195, maxColorValue = 255)
R_grey <- rgb(203, 206, 208, maxColorValue = 255)
R_grey2 <- rgb(132, 131, 139, maxColorValue = 255)

hex_back <- R_grey
hex_border <- R_blue

plotdat <- data.frame(
  variable = factor(c(LETTERS[1:10], LETTERS[1:10]), levels = LETTERS[10:1]),
  value = c(-10, -8,  5, -4,  2, -1.5, -1, -1, -0.5, 0,
             10,  9, -6,  4, -2, 1, 0.5, 0, 0.5, 1),
  Level = rep(c("low", "high"), each = 10)
)

g1 <- ggplot(plotdat, aes_string(x = "variable", y = "value", fill = "Level")) +
  geom_bar(position = "identity", stat = "identity") +
  coord_flip() +
  ylab("") +
  xlab("") +
  scale_fill_manual(values = c(R_blue, R_grey2)) +
  theme(axis.text = element_blank(),
        axis.line = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(), #element_rect(fill = "transparent"),
        plot.background = element_blank(), #element_rect(fill = "transparent"),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

g2 <- ggplot() +
  geom_hexagon(size = 3, fill = hex_back, color = hex_border) +
  theme_sticker(3) +
  geom_subview(subview = g1, x = 1, y = 1, width = 1.5, height = 1.3) +
  geom_pkgname("tornado", x = 1, y = 0.35, size = 10,
               color = "black", family = "sans")

plot(g2)

# ggsave(g2, width = 48.9, height = 50.8, filename = file.path("etc", "tornado_hex.svg"),
#        bg = "transparent", units = "mm")

sticker(g1, package="tornado", s_x=1, s_y=1, s_width=1.5, s_height=1.3,
        p_x=1, p_y=0.35, p_color="black", p_size=18, p_family = "sans",
        h_fill = hex_back, h_color = hex_border,
        filename=file.path("logo.png"))

# sticker(g1, package="tornado", s_x=1, s_y=1, s_width=1.5, s_height=1.3,
#         p_x=1, p_y=0.35, p_color="black", p_size=10, p_family = "sans",
#         h_fill = hex_back, h_color = hex_border,
#         filename=file.path("etc", "tornado_hex.svg"))

### Build the favicon for pkgdown

pkgdown::build_favicons()

