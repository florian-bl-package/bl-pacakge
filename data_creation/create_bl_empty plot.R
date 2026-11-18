# create bl empty plot
# run once before re-installing package



bl_empty_plot <- bl_species_data %>%
    ggplot(mapping = aes (x = grasshopper , y = butterflies )) +
    bl_theme() +
    ylab (NULL) +
    xlab (NULL) +
    theme (line = element_line(color = "white"),
    axis.text = element_text(color = "white"),
    axis.ticks = element_line(color = "white"),
    #panel.grid = element_line(color = "white"),
    panel.border = element_rect(colour = "white", fill = NA))


usethis::use_data(bl_empty_plot)
