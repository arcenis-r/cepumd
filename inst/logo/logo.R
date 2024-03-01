library(hexSticker)
library(ggplot2)
library(dplyr)

bls_blue <- "#10287A"
bls_red <- "#990000"

font_data <- sysfonts::font_files()
font_name <- "Arial-BoldMT"
font_file <- font_data |> filter(ps_name %in% font_name) |> pull(file)
sysfonts::font_add(font_name, font_file)

blue_back <- ggplot() +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)
  )

sticker(
  blue_back,
  package = "cepumd",
  h_fill = bls_blue,
  h_color = bls_red,
  h_size = 1.5,
  p_x = 1,
  p_y = 1.05,
  p_size = 28,
  p_family = font_name,
  filename = "./man/figures/logo.png"
)
