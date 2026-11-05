
#  FLOWCHART
#  Lab group project 2026 - Are species distribution models equally reliable
#  across land-cover classes?

library(ggplot2)

project_dir <- "/Users/mak/Documents/GitHub/lab_group_project_2026"
setwd(project_dir)
out_dir <- file.path("outputs", "figures")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------------------------------------------
# 1. Boxes
# -----------------------------------------------------------------------------
# One row per box: centre (x, y), width (w), height (h)
box <- function(id, x, y, w, h, title, detail, type) data.frame(id, x, y, w, h, title, detail, type)

boxes <- rbind(
  # Inputs
  box("gbif", 3,  30, 5.0, 1.6, "GBIF bird records",
      "Turkey Vulture, House Sparrow\n(human observations, Mexico)", "Input"),
  box("wc",   9,  30, 5.0, 1.6, "WorldClim 2.1",
      "19 bioclimatic variables\n (~1 km)", "Input"),
  box("lc",  15,  30, 5.0, 1.6, "Esri land cover",
      "9 classes, 1 km\n(Sentinel-2, 10 m source)", "Input"),
  # Shared preparation
  box("s3",   3,  26.6, 5.0, 2.0, "Step 3. Clean records",
      "present, Mexico\nlocation error <= 1 km\nduplicates removed", "Data preparation"),
  box("s1",   9,  26.6, 5.0, 2.0, "Step 1. Climate data",
      "", "Data preparation"),
  box("s2",  15,  26.6, 5.0, 2.0, "Step 2. Land cover",
      "class of each 1 km cell\n% cover of each class", "Data preparation"),
  # Per species
  box("s4",   9,  22.6, 9.0, 1.6, "Step 4. Study area",
      "", "Data preparation"),
  box("s5",   9,  20.4, 9.0, 1.8, "Step 5. Presence and background points",
      "presence = 1 km cell with records (one per cell)\nbackground = cells visited by birders (target group)", "Data preparation"),
  box("s6",   9,  18.0, 9.0, 1.8, "Step 6. Predictors and class labels",
      "climate variables with |r| <= 0.7 (same set for both models)", "Data preparation"),
  box("s8",   9,  15.6, 9.0, 1.8, "Steps 7-8. Spatial test folds",
      "5 sets of ~100 km blocks; each set is held out once\nso every point is tested exactly once", "Modelling"),
  box("s9a",  5.6, 12.6, 6.2, 2.2, "Step 9a. Tuning",
      "", "Modelling"),
  box("s9b", 12.4, 12.6, 6.2, 2.2, "Step 9b. Fit and test",
      "fit on training folds", "Modelling"),
  box("s10",  9,   9.4, 9.0, 1.8, "Step 10. Results split by land-cover class",
      "AUC and Boyce index (ranking)  |  O/E ratio (amount)\nclasses with >= 30 test records", "Evaluation"),
  box("s11",  3.4, 6.4, 5.4, 2.2, "Step 11. Paired bootstrap",
      "95% confidence intervals\nModel 2 - Model 1 per class\nspread across classes", "Evaluation"),
  box("s12",  9,   6.4, 5.4, 2.2, "Step 12. Fair comparison",
      "999 random point sets of the\nsame size and climate\n(is a class worse than chance?)", "Evaluation"),
  box("s13", 14.6, 6.4, 5.4, 2.2, "Step 13. Leave one class out",
      "train without a class, test in it\n(transfer to unseen habitat)", "Evaluation"),
  box("s14",  9,   3.3, 9.0, 1.6, "Output- Maps, tables and figures",
      "", "Output"),
  box("sE",  21.0, 15.6, 4.6, 2.0, "Optional check",
      "virtual species with a known\nanswer run through the\nsame steps 4-14", "Check")
)
boxes$xmin <- boxes$x - boxes$w / 2; boxes$xmax <- boxes$x + boxes$w / 2
boxes$ymin <- boxes$y - boxes$h / 2; boxes$ymax <- boxes$y + boxes$h / 2

# --------
# 2. Arrows
# --------
link <- function(from, to, side = "down") {
  a <- boxes[boxes$id == from, ]; b <- boxes[boxes$id == to, ]
  if (side == "down")  return(data.frame(x = a$x, y = a$ymin, xend = b$x, yend = b$ymax))
  if (side == "right") return(data.frame(x = a$xmax, y = a$y, xend = b$xmin, yend = b$y))
  if (side == "left")  return(data.frame(x = a$xmin, y = a$y, xend = b$xmax, yend = b$y))
}
arrows <- rbind(
  link("gbif", "s3"), link("wc", "s1"), link("lc", "s2"),
  link("s3", "s4"), link("s1", "s4"), link("s2", "s4"),
  link("s4", "s5"), link("s5", "s6"), link("s6", "s8"),
  link("s8", "s9a"), link("s9a", "s9b", "right"), link("s9b", "s10"),
  link("s10", "s11"), link("s10", "s12"), link("s10", "s13"),
  link("s11", "s14"), link("s12", "s14"), link("s13", "s14"),
  link("sE", "s8", "left")
)

# Dashed frame around the steps that run once for every species
species_frame <- data.frame(xmin = 0.2, xmax = 17.8, ymin = 2.2, ymax = 24.4)

# ---------
# 3. Draw
# ---------
fill_cols <- c("Input" = "#E0E0E0", "Data preparation" = "#C6DBEF", "Modelling" = "#FDD0A2",
               "Evaluation" = "#C7E9C0", "Output" = "#DADAEB", "Check" = "#FFF7BC")

p <- ggplot() +
  geom_rect(data = species_frame, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = NA, colour = "grey45", linetype = 2) +
  annotate("text", x = 0.45, y = 24.05, label = "Repeated for each species", hjust = 0,
           fontface = "italic", colour = "grey35", size = 3.6) +
  geom_segment(data = arrows, aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.18, "cm"), type = "closed"), colour = "grey30", linewidth = 0.45) +
  geom_rect(data = boxes, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = type),
            colour = "grey25", linewidth = 0.4) +
  geom_text(data = boxes, aes(x = x, y = ymax - 0.32, label = title), fontface = "bold", size = 3.5) +
  geom_text(data = boxes, aes(x = x, y = ymax - 0.62, label = detail), vjust = 1, size = 2.9,
            lineheight = 0.95, colour = "grey15") +
  scale_fill_manual(values = fill_cols, breaks = names(fill_cols), name = NULL) +
  coord_equal(xlim = c(0, 23.6), ylim = c(-0.2, 31.2), expand = FALSE) +
  labs(title = "Are species distribution models equally reliable across land-cover classes?",
       subtitle = "") +
  theme_void() +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "grey30"),
        plot.margin = margin(10, 10, 10, 10), plot.background = element_rect(fill = "white", colour = NA))

ggsave(file.path(out_dir, "workflow_flowchart.png"), p, width = 11, height = 14.5, dpi = 200)
ggsave(file.path(out_dir, "workflow_flowchart.pdf"), p, width = 11, height = 14.5)
print(p)
cat("Flowchart saved in", file.path(project_dir, out_dir), "\n")
