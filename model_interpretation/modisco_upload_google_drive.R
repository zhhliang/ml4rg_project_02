# Install and load the readxl package if not already installed
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}
library(readxl)

# Define the path to the Excel file
file_path <- "merged_single_qvalue_report.xlsx"

# Read the diff_qvalues sheet
diff_qvalues <- as.data.frame(read_excel(file_path, sheet = "diff_qvalues"))
colnames(diff_qvalues)[1] <- "motif_tf" 
rownames(diff_qvalues) <- diff_qvalues$motif_tf
diff_qvalues <- diff_qvalues[, 2:6]

# Read the global_qvalues sheet
global_qvalues <- as.data.frame(read_excel(file_path, sheet = "global_qvalues"))
colnames(global_qvalues)[1] <- "motif_tf"
rownames(global_qvalues) <- global_qvalues$motif_tf
global_qvalues <- global_qvalues[, 2:6]

# Print the first few rows of each data frame
print(head(diff_qvalues))
print(head(global_qvalues))

diff_qvalues_filter <- diff_qvalues
# diff_qvalues_filter[diff_qvalues_filter>0.1] <-NA
diff_qvalues_filter <- diff_qvalues_filter[rowSums(is.na(diff_qvalues_filter)) != 4, ]
diff_qvalues_filter$class <- gsub("\\[|\\]", "", diff_qvalues_filter$class)
diff_qvalues_filter$class <- gsub("\\'", "", diff_qvalues_filter$class)

# keep the rows with at least one qvalue < 0.5
# diff_qvalues_filter <- diff_qvalues_filter[apply(diff_qvalues_filter[, 1:4], 1, function(x) any(x < 0.5, na.rm = TRUE)), ]


global_qvalues_filter <- global_qvalues
# global_qvalues_filter[global_qvalues_filter>0.1] <-NA
global_qvalues_filter <- global_qvalues_filter[rowSums(is.na(global_qvalues_filter)) != 4, ]
global_qvalues_filter$class <- gsub("\\[|\\]", "", global_qvalues_filter$class)
global_qvalues_filter$class <- gsub("\\'", "", global_qvalues_filter$class)

# keep the rows with at least one qvalue < 0.5
# global_qvalues_filter <- global_qvalues_filter[apply(global_qvalues_filter[, 1:4], 1, function(x) any(x < 0.5, na.rm = TRUE)), ]


diff_class <- diff_qvalues_filter$class
table(diff_class)

diff_class_new <- ifelse(diff_class %in% c("C2H2 zinc finger factors", "Nuclear receptors with C4 zinc fingers", "Other C4 zinc finger-type factors"), "Zinc Finger Factors", 
                           ifelse(diff_class %in% c("Basic helix-loop-helix factors (bHLH)", "Basic leucine zipper factors (bZIP)"), "Helix-Loop-Helix and\nLeucine Zipper Factors",
                                  ifelse(diff_class %in% c("Homeo domain factors", "High-mobility group (HMG) domain factors", "Paired box factors"), "Homeobox and\nHMG Domain Factors", "GCM domain factors")))

diff_class_new <- factor(diff_class_new, levels = c("Zinc Finger Factors", 
                                                        "Helix-Loop-Helix and\nLeucine Zipper Factors",
                                                        "Homeobox and\nHMG Domain Factors",
                                                        "GCM domain factors"))

table(diff_class_new)
# diff_class_new
# Zinc Finger Factors Helix-Loop-Helix and\nLeucine Zipper Factors             Homeobox and\nHMG Domain Factors 
# 9                                            5                                            5 
# GCM domain factors 
# 3 


global_class <- global_qvalues_filter$class
table(global_class)

global_class_new <- ifelse(global_class %in% c("C2H2 zinc finger factors", "Other C4 zinc finger-type factors", "Nuclear receptors with C4 zinc fingers"), "Zinc Finger Factors", 
                           ifelse(global_class %in% c("Basic helix-loop-helix factors (bHLH)", "Heat shock factors", "Rel homology region (RHR) factors"), "Helix-Loop-Helix, Heat Shock\nand Rel Homology Region Factors",
                                  ifelse(global_class %in% c("Homeo domain factors", "High-mobility group (HMG) domain factors", "Paired box factors"), "Homeobox and High\nMobility Group Factors", "Other Specific Factors")))

global_class_new <- factor(global_class_new, levels = c("Zinc Finger Factors", 
                                                        "Helix-Loop-Helix, Heat Shock\nand Rel Homology Region Factors",
                                                        "Homeobox and High\nMobility Group Factors",
                                                        "Other Specific Factors"))

table(global_class_new)
# global_class_new
# Zinc Finger Factors Helix-Loop-Helix, Heat Shock\nand Rel Homology Region Factors 
# 18                                                             9 
# Homeobox and High\nMobility Group Factors                                        Other Specific Factors 
# 7                                                             3 

# heatmap using -log10(qvalues)
df.diff <- -log10(diff_qvalues_filter[, 1:4])
# df.diff[is.na(df.diff)] <- 0

df.global <- -log10(global_qvalues_filter[, 1:4])
# df.global[is.na(df.global)] <- 0

### heatmaps
library(ComplexHeatmap)
library(circlize)
library(colorspace)
library(RColorBrewer)

# colors
summary(unlist(df.diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.1924  0.0000  3.3778 

summary(unlist(df.global))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.0000  0.2717  1.8045  1.8147  2.7649  6.7383      47 


### diff
ht_opt$DIMNAME_PADDING = unit(5, "mm")
ht_opt$TITLE_PADDING = unit(5, "mm")

column_ha = HeatmapAnnotation(Class = diff_class_new,
                              col = list(Class = c("Zinc Finger Factors" = "#003366", "Helix-Loop-Helix and\nLeucine Zipper Factors" = "darkseagreen4",
                                                   "Homeobox and\nHMG Domain Factors" = "orchid4", "GCM domain factors" = "gold1")
                              ),
                              # annotation_label = c("ai score", "cohort", "age", "ABCA13 alteration"),
                              annotation_label = c("Class"),
                              annotation_name_gp = gpar(fontsize = 16),
                              simple_anno_size = unit(0.7, "cm"),
                              # annotation_name_offset = unit(3, "mm"),
                              annotation_legend_param = list(title_gp = gpar(fontsize = 15), labels_gp = gpar(fontsize = 15),
                                                             # nrow = 1, direction = "horizontal",
                                                             Treatment_Respond = list(title = "Class", direction = "horizontal", nrow=1)
                              ),
                              annotation_name_side = "left"  
)

df.diff.plot <- t(df.diff[order(df.diff$mesoderm, decreasing = T), ])

heatmap.diff <- Heatmap(as.matrix(df.diff.plot),
                        na_col = "grey80",
                          column_split = diff_class_new,
                          # column_title_rot = 0, column_title_gp =  gpar(fontsize = 18), 
                          # column_gap = unit(4, "mm"),
                          column_title = NULL,
                          name = "negative log10 P-adjust",
                          col = colorRamp2(c(0, 1, 4), c("gray100", "#FEE092","#A50026")),
                          rect_gp = gpar(col = "gray50", lwd = 1),
                          # row_title = c("Motifs and TFs using contribution scores (differential analysis)"),
                          # row_title_gp = gpar(fontsize = 22),
                          cluster_rows = FALSE, show_row_dend = FALSE, cluster_row_slices = FALSE,
                          show_row_names = TRUE, row_names_gp = gpar(fontsize = 18), row_names_side = "left", 
                          cluster_columns = FALSE, show_column_dend = FALSE, column_names_rot = 45, cluster_column_slices = FALSE,
                          show_column_names = TRUE, column_names_gp = gpar(fontsize = 18),
                          show_heatmap_legend = FALSE,
                          top_annotation = column_ha,
                          ### fdr < 0.05 and fdr < 0.1, and NA
                          cell_fun = function(j, i, x, y, width, height, fill) {
                            if(is.na(df.diff.plot[i, j])) {
                              grid.points(x, y, pch = 4, size = unit(8, "mm"))
                            }
                            if(!is.na(df.diff.plot[i, j]) & (abs(df.diff.plot[i, j]) > abs(log10(0.05)))) {
                              grid.points(x, y, pch = 16, size = unit(6, "mm"))
                            }
                            if(!is.na(df.diff.plot[i, j]) & (abs(df.diff.plot[i, j]) <= abs(log10(0.05))) & (abs(df.diff.plot[i, j]) > abs(log10(0.1)))) {
                              grid.points(x, y, pch = 17, size = unit(6, "mm"))
                            }
                          }
)

lgd.main =  Legend(nrow =1, col_fun = colorRamp2(c(0, 1, 4), c("gray100", "#FEE092","#A50026")),
                   title = "negative log10 qvalue", grid_height = unit(5, "mm"), direction = "horizontal",
                   legend_width = unit(5, "cm"),
                   title_gp = gpar(fontsize = 20),  labels_gp = gpar(fontsize = 18))

lgd.shape = Legend(title = "q-value", labels = c("< 0.05", "< 0.1", "NA"), 
                   type = "points", pch = c(16, 17, 4), legend_gp = gpar(col = c("black", "black", "black")),
                   size = unit(0.4, "snpc"), direction = "horizontal", nrow = 1,
                   title_gp = gpar(fontsize = 20),  labels_gp = gpar(fontsize = 18))

pd = packLegend(lgd.main, lgd.shape,  direction = "horizontal")


ht_opt$HEATMAP_LEGEND_PADDING = unit(8, "mm")


jpeg("diff_motifs_tfs_annotation_with_title.jpg", height = 8, width = 15, res = 600, units = "in")

draw(heatmap.diff, heatmap_legend_list = pd, ht_gap = unit(8, "mm"),  heatmap_legend_side = "bottom", merge_legend = TRUE,
     padding = unit(c(5, 5, 20, 10), "mm"))

decorate_heatmap_body("negative log10 P-adjust", {
  grid.text("Motifs and TFs using contribution scores (differential analysis)",unit(5.2, "cm"), unit(95, "mm"), just = "left", gp = gpar(fontsize = 20))

  })

dev.off()

jpeg("diff_motifs_tfs_annotation.jpg", height = 8, width = 15, res = 600, units = "in")

draw(heatmap.diff, heatmap_legend_list = pd, ht_gap = unit(8, "mm"),  heatmap_legend_side = "bottom", merge_legend = TRUE,
     padding = unit(c(5, 0, 5, 5), "mm"))

dev.off()

ht_opt(RESET = TRUE)





### global
ht_opt$DIMNAME_PADDING = unit(5, "mm")
ht_opt$TITLE_PADDING = unit(5, "mm")

df.global.plot <- t(df.global[order(df.global$mesoderm, decreasing = T), ])

column_ha = HeatmapAnnotation(Class = global_class_new,
                              col = list(Class = c("Zinc Finger Factors" = "#003366", "Helix-Loop-Helix, Heat Shock\nand Rel Homology Region Factors" = "darkseagreen4",
                                                   "Homeobox and High\nMobility Group Factors" = "orchid4", "Other Specific Factors" = "gold1")
                              ),
                              # annotation_label = c("ai score", "cohort", "age", "ABCA13 alteration"),
                              annotation_label = c("Class"),
                              annotation_name_gp = gpar(fontsize = 16),
                              simple_anno_size = unit(0.7, "cm"),
                              # annotation_name_offset = unit(3, "mm"),
                              annotation_legend_param = list(title_gp = gpar(fontsize = 15), labels_gp = gpar(fontsize = 15),
                                                             # nrow = 1, direction = "horizontal",
                                                             Treatment_Respond = list(title = "Class", direction = "horizontal", nrow=1)
                              ),
                              annotation_name_side = "left"  
)

heatmap.global <- Heatmap(as.matrix(df.global.plot),
                          na_col = "grey80",
                          column_split = global_class_new,
                          # column_title_rot = 0, column_title_gp =  gpar(fontsize = 15), 
                          # column_gap = unit(4, "mm"), 
                          column_title = NULL,
                          name = "negative log10 P-adjust",
                          col = colorRamp2(c(0, 1, 3, 5), c("gray100", "seashell2", "#FEE092","#A50026")),
                          rect_gp = gpar(col = "gray50", lwd = 1),
                          # column_title = c("Motifs and TFs using contribution scores (global analysis)"),
                          # column_title_gp = gpar(fontsize = 22),
                          cluster_rows = FALSE, show_row_dend = FALSE, cluster_row_slices = FALSE,
                          show_row_names = TRUE, row_names_gp = gpar(fontsize = 18), row_names_side = "left", 
                          cluster_columns = FALSE, show_column_dend = FALSE, column_names_rot = 45, cluster_column_slices = FALSE,
                          show_column_names = TRUE, column_names_gp = gpar(fontsize = 13),
                          show_heatmap_legend = FALSE,
                          top_annotation = column_ha,
                          ### fdr < 0.05 and fdr < 0.1, NA
                          cell_fun = function(j, i, x, y, width, height, fill) {
                            if(is.na(df.global.plot[i, j])) {
                              grid.points(x, y, pch = 4, size = unit(8, "mm"))
                            }
                            if(!is.na(df.global.plot[i, j]) & (abs(df.global.plot[i, j]) > abs(log10(0.05)))) {
                              grid.points(x, y, pch = 16, size = unit(6, "mm"))
                            }
                            if(!is.na(df.global.plot[i, j]) & (abs(df.global.plot[i, j]) <= abs(log10(0.05))) & (abs(df.global.plot[i, j]) > abs(log10(0.1)))) {
                              grid.points(x, y, pch = 17, size = unit(6, "mm"))
                            }
                          }
)

lgd.main =  Legend(nrow =1, col_fun = colorRamp2(c(0, 1, 3, 5), c("gray100", "seashell2", "#FEE092","#A50026")),
                   title = "negative log10 qvalue", grid_height = unit(5, "mm"), direction = "horizontal",
                   legend_width = unit(5, "cm"),
                   title_gp = gpar(fontsize = 20),  labels_gp = gpar(fontsize = 18))

lgd.shape = Legend(title = "q-value", labels = c("< 0.05", "< 0.1", "NA"), 
                   type = "points", pch = c(16, 17, 4), legend_gp = gpar(col = c("black", "black", "black")),
                   size = unit(0.4, "snpc"), direction = "horizontal", nrow = 1,
                   title_gp = gpar(fontsize = 20),  labels_gp = gpar(fontsize = 18))


pd = packLegend(lgd.main, lgd.shape,  direction = "horizontal")


ht_opt$HEATMAP_LEGEND_PADDING = unit(8, "mm")


jpeg("global_motifs_tfs_annotation_with_title.jpg", height = 7, width = 15, res = 400, units = "in")
draw(heatmap.global, heatmap_legend_list = pd, ht_gap = unit(8, "mm"),  heatmap_legend_side = "bottom", merge_legend = TRUE,
     padding = unit(c(5, 5, 10, 5), "mm"))

decorate_heatmap_body("negative log10 P-adjust", {
  grid.text("Motifs and TFs using contribution scores (global analysis)",unit(8.2, "cm"), unit(78, "mm"), just = "left", gp = gpar(fontsize = 20))
})  
  
dev.off()


jpeg("global_motifs_tfs_annotation.jpg", height = 6, width = 15, res = 400, units = "in")
draw(heatmap.global, heatmap_legend_list = pd, ht_gap = unit(8, "mm"),  heatmap_legend_side = "bottom", merge_legend = TRUE,
     padding = unit(c(5, 0, 5, 5), "mm"))
dev.off()


ht_opt(RESET = TRUE)

