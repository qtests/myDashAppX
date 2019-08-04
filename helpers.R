
library(ggplot2)
library(treemapify)

categories <- list("income", "savings", "expenses")

prettifyValueBox <- function(val, subtitle, color) {
  val <- prettyNum(val, big.mark = ",")
  valueBox(value = glue("{val} \u20AC"), subtitle = subtitle, color = color)
}


#--- Map plot ----------------------------

treemapified_dat <- function(dat) {
  treemapify(dat, 
             area = "total" #, subgroup = "category", subgroup2 = "subcategory"
  )
}

basePlot <- function(dat) {
  ggplot(dat, aes(
    area = total, fill = category, label = subcategory))
}


renderLandingPagePlot <- function(basePlot) {
  colors <- c(
    income = "#975c72", 
    expenses = "#724678", 
    savings = "#545294"
  )
  
  basePlot + 
    geom_treemap() + 
    geom_treemap_text(color = "white", fontface = 2) +
    scale_fill_manual(values = colors) +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.key.size = unit(1, "cm"),
      legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold", 
        inherit.blank = TRUE)
    )
}

getClickedPoint <- function(treeDat, click) {
  treeDat %>%
    filter(xmin <= click$x) %>% filter(xmax >= click$x) %>%
    filter(ymin <= click$y) %>% filter(ymax >= click$y)
}

#--- Map plot ---------------------------- END


