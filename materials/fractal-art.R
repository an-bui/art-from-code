library(dplyr)
library(purrr)
library(tibble)
library(ggplot2)
library(ggthemes)
library(ambient)
library(here)

sample_canva <- function(seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  sample(ggthemes::canva_palettes, 1)[[1]]
}

blank_canvas <- long_grid(
  x = seq(from = 0, to = 1, length.out = 2000),
  y = seq(from = 0, to = 1, length.out = 2000)
) 

plot_painted_canvas <- function(canvas, palette = NULL) {
  if(is.null(palette)) {
    palette <- c("#e5ddc8","#01949a","#004369","#db1f48")
  }
  canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
}

fractal_art <- function(fractal, generator, palette = NULL, ...) {
  blank_canvas |>
    mutate(
      paint = fracture(
        noise = generator,
        fractal = fractal,
        x = x, 
        y = y, 
        ...
      )
    ) |>
    plot_painted_canvas(palette = palette)
}

gf <- function(x) x * .8
new_gain <- function(x) x * 0.1
new_frequency <- function(x) x * 0.5
fractal_art(fractal = ridged, 
            generator = gen_waves, 
            seed = 10, 
            octaves = 100, 
            gain = gf, 
            frequency = new_frequency)

# ambient artwork
x_coords <- seq(from = 0, to = 1, length.out = 800)
y_coords <- seq(from = 0, to = 1, length.out = 800)

canvas <- long_grid(x = x_coords, y = y_coords) 

gen_perlin(x = 1:5, y = 1, frequency = .001, seed = 1)
gen_perlin(x = 1:5, y = 1, frequency = .5, seed = 1)

canvas <- canvas |> 
  mutate(paint = gen_perlin(x, y, frequency = 10, seed = 1234))
canvas

art <- ggplot(canvas, aes(x, y, fill = paint)) + 
  geom_raster(show.legend = FALSE) 

art + 
  theme_void() +
  coord_equal() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_gradientn(colours = sample_canva())


make_noise_art <- function(
    generator = gen_perlin, 
    frequency = 10, 
    seed = 1234,
    pixels = 2000,
    palette = c("#e5ddc8", "#01949a", "#004369", "#db1f48"), 
    ...
) {
  
  # define the grid
  canvas <- long_grid(
    x = seq(from = 0, to = 1, length.out = pixels),
    y = seq(from = 0, to = 1, length.out = pixels)
  ) 
  
  # use the generator to add paint
  canvas <- canvas |>
    mutate(
      paint = generator(
        x, y, 
        frequency = frequency, 
        seed = seed, 
        ...
      )
    )
  
  # use ggplot2 to draw the picture
  art <- canvas |> 
    ggplot(aes(x, y, fill = paint)) + 
    geom_raster(show.legend = FALSE) +
    theme_void() +
    coord_equal() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_fill_gradientn(colours = palette)
  
  return(art)
}


blank_canvas |> 
  mutate(
    lf_noise = gen_simplex(x, y, frequency = 1),
    mf_noise = gen_simplex(x, y, frequency = 20),
    hf_noise = gen_simplex(x, y, frequency = 99),
    gate = gen_spheres(x, y, frequency = 10) |> normalise(),
    paint = lf_noise +
      (1 + mf_noise) * (gate >= .1 & gate < .6) +
      (1 + hf_noise) * (gate >= .05)
  ) |>
  plot_painted_canvas(palette = sample_canva(seed = 2))



