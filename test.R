#This line of code installs the pacman page if you do not have it installed -
#if you do, it simply loads the package
if(!require(pacman))install.packages("pacman")

pacman::p_load('ddplot', 'dplyr', 'tidyr', 'gapminder',
               'ggplot2',  'ggalt',
               'forcats', 'R.utils', 'png',
               'grid', 'ggpubr', 'scales',
               'eurostat', 'countrycode', 'ggalt', 'tidyr')


gasGdpCapita <- read.csv('dataframe.csv', row.names='X', header=TRUE, stringsAsFactors = FALSE)

windowsFonts("Helvetica" = windowsFont("Helvetica Light"))

plot.label.sources <-"Eurostat (2019) Real GDP per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/sdg_08_10 \r\n Greenhouse gas emissions per capita [Data file] Retrieved from: https://ec.europa.eu/eurostat/web/products-datasets/-/t2020_rd300"
klassen <- 3


dumbbell_top <- gasGdpCapita %>%
    select(geo, jaar, gas) %>%
    filter(geo != 'RO', jaar %in% c(2000,2016)) %>%
    spread(jaar, gas) %>%
    mutate(gap = `2016` - `2000`) %>%
    arrange(desc(gap)) %>%
    head(5)

dumbbell_bottom <- gasGdpCapita %>%
    mutate(klasse = cut_to_classes(gdp, n=klassen, style="quantile", decimals=0)) %>%
    select(geo, jaar, gas) %>%
    filter(geo != 'RO', jaar %in% c(2000,2016)) %>%
    spread(jaar, gas) %>%
    mutate(gap = `2016` - `2000`) %>%
    arrange(gap) %>%
    head(5)
dumbbell <- rbind(dumbbell_top, dumbbell_bottom) %>% arrange(gap)


#Make plot
plot.dumbbell <- ggplot(dumbbell, aes(x = `2000`, xend = `2016`, y = reorder(geo, gap), group = geo)) +
  geom_dumbbell(colour = "#dddddd",
                size = 3,
                colour_x = "#FAAB18",
                colour_xend = "#1380A1") +
  dd_style() +
  labs(title="derp", subtitle="darp")

dd_plot(plot_name = plot.dumbbell,
              source = plot.label.sources,
              save_filepath = "dumbell2.png",
              width_pixels = 640,
              height_pixels = 500,
              logo_image_path = "placeholder.png")
