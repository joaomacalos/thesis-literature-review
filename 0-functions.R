library(tidyverse)
library(imfr)
library(rlang)

## Multi-join

multi_join <- function(dataset) {
  df <- NULL
  for (data in dataset) {
    if (is.null(df)) {
      df = data
    } else {
      df = full_join(df, data)
    }
  }
  return(df)
}


## Plotting diagrams function

sdprep <- function(...) {
  names = map(rlang::enexprs(...), ~rlang::as_string(.x))
  args = rlang::list2(...)
  
  tbl <- imap_dfr(args, ~mutate(.x, label = names[[.y]]))
  
  tbl <- tbl %>% group_by(label) %>% mutate(type = as_factor(if_else(first(y) - last(y) > 0, "d", "s"))) %>% ungroup
  
  lbl <- imap_dfr(args, ~mutate(slice(.x, 0.9 * length(.x$x)), label = names[[.y]], type = substr(label, 1, 1)))
  
  return(list(tbl = tbl, lbl = lbl))
}


sdplot <- function(..., labels = NULL, x.axis = NULL, y.axis = NULL, intersections = NULL) {
  .sd1 <- sdprep(...)
  
  .sd1$tbl %>%
    ggplot(aes(x = x, y = y)) +
    geom_line(aes(group = label, color = type), size = 1) +
    geom_label(data = .sd1$lbl, aes(x = x, y = y, fill = type), label = labels, color = "white") +
    geom_segment(data = intersections, aes(x = 0, y = y, xend = x, yend = y), lty = "dotted") +
    geom_segment(data = intersections, aes(x = x, y = 0, xend = x, yend = y), lty = "dotted") +
    scale_color_manual(values = c("dodgerblue3", "firebrick3")) +
    scale_fill_manual(values = c("dodgerblue3", "firebrick3")) +
    scale_x_continuous(expand = c(0, 0), breaks = intersections$x,
                       labels = x.axis) +
    scale_y_continuous(expand = c(0, 0), breaks = intersections$y,
                       labels = y.axis) +
    labs(x = '', y = '') +
    theme_classic() +
    theme(legend.position = 'none',
          #axis.text.x = element_blank(),
          #axis.text.y = element_blank(),
          axis.ticks = element_blank())
}