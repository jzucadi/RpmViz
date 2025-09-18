The script draws a dial with multiple concentric tracks, each representing a set of speeds (RPMs), using different colors for each layer.
It visually marks the center of the dial and highlights boundaries like the dial’s edge and any mounting holes.
Each speed layer (track) displays the corresponding RPM values as text, facing outward for readability.

Parameter Setup
R
font_size <- 15 #pt
scale_height <- (dial_outer_edge_dia_in - dial_diameter_in) / length(spindle_pulley_diameters)
gap_after = stop_angle - start_angle
cell_padding <- c(0.00, 1.00, 0.00, 1.00)
font_size: Sets text size for speed labels.
scale_height: Calculates the height of the circular tracks based on dial dimensions and the number of pulleys.
gap_after: Determines the angular gap between tracks.
cell_padding: Sets padding between circular cells (tracks).
Outermost Track (Highest Speeds)
R
circos.par(clock.wise = TRUE, start.degree = start_angle, gap.after = gap_after, cell.padding = cell_padding)
circos.initialize(factors = "speeds1", xlim = c(min(speeds1), max(speeds1)))
circos.track(
  ylim = c(0, 1),
  bg.border = "light blue",
  track.height = convert_length(scale_height, "in")
)
circos.text(speeds1, rep(0.5, length(speeds1)), speeds1, facing = "outside", niceFacing = TRUE, cex = fontsize(font_size), font = 2, family = "serif")
circos.clear()
Initializes the circular plot with the outermost speed layer (speeds1).
Draws a track with a blue border and places speed values outward around the ring.
Center Mark and Boundaries
R
lines(x = c(-0.1, 0.1), y = c(0, 0), fg = "lightgray")
lines(x = c(0, 0), y = c(-0.1, 0.1), fg = "lightgray")
symbols(c(0, 0), c(0, 0), circles = c(dial_diameter_in / 2, mounting_hole_dia_in / 2), fg = "lightgray", add = TRUE)
Draws faint cross lines to mark the dial center.
Draws circles for the dial edge and the mounting hole, helping visually anchor the dial.
Additional Tracks (Lower Speeds)
For each subsequent speed layer (speeds2, speeds3, speeds4), the code:
Uses par(new = TRUE) to overlay new tracks on the same plot.
Expands the canvas limits (xy_canvas_lim) to fit the next ring.
Initializes and draws colored tracks using circos.track.
Places the speed values around each ring.
Example for Second Layer:
R
xy_canvas_lim <- 1.16
circos.par(canvas.xlim = c(-xy_canvas_lim, xy_canvas_lim), ...)
circos.initialize(factors = "speeds2", xlim = c(min(speeds2), max(speeds2)))
circos.track(
  ylim = c(0, 1),
  bg.border = "yellow",
  ...,
  panel.fun = function(x, y) {
    circos.text(speeds2, rep(0.5, length(speeds2)), speeds2, facing = "outside", ...)
  }
)
circos.clear()
Yellow, red, and purple borders are used for layers 2–4, respectively.
Key Features & Techniques
Multi-layered Dial: Each speed set is rendered as a distinct ring.
Text Facing Outward: Ensures RPM values are readable from outside the dial.
Custom Track Heights: Dynamically computed for proportional visuals.
Layer Overlay: Uses par(new = TRUE) and increasing canvas limits to stack rings.
