#Composition barplot for core community based on class level

# Load the global variable 
mae <- readRDS("mae.rds")
tse = mae[[1]]
tse_bacteria = mae[[2]]
tse_active = mae[[3]]
tse_pathway = mae[[4]]
tse_enzyme = mae[[5]]
tse_gene = mae[[6]]


#######################################################################
# Getting top taxa on a species level - active from samsa2 
tse_active_species <- agglomerateByRank(tse_active, rank ="Species")

top_taxa_active <- getTopFeatures(tse_active_species, top = 19, assay.type = "relabundance")

# Renaming the "Phylum" rank to keep only top taxa and the rest to "Other"
species_renamed <- lapply(rowData(tse_active_species)$Species, function(x){
  if (x %in% top_taxa_active) {x} else {"Other"}
})
rowData(tse_active_species)$Species <- as.character(species_renamed)


plots_species <- plotAbundance(tse_active_species, rank = "Species", 
                              assay.type = "relabundance", features = "Date")

# Modify the legend of the first plot to be smaller
plots_species[[1]] <- plots_species[[1]] +
  theme(
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10))

# Modify the legend of the second plot to be smaller
plots_species[[2]] <- plots_species[[2]] +
  theme(
    legend.key.height = unit(0.3, 'cm'),
    legend.key.width = unit(0.3, 'cm'),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")

# Combine legends
legend_species <- wrap_plots(
  as_ggplot(get_legend(plots_species[[1]])),
  as_ggplot(get_legend(plots_species[[2]])),
  ncol = 1)

# Removes legends from the plots
plots_species[[1]] <- plots_species[[1]] + theme(legend.position = "none")
plots_species[[2]] <- plots_species[[2]] +
  theme(legend.position = "none", axis.title.x=element_blank())

# Combine plots
plot_species_relabundance <- wrap_plots(plots_species[[2]], plots_species[[1]], ncol = 1, heights = c(2, 10))
# Combine the plot with the legend

#png(filename="figures/barplot_species.png" ,units = 'in',width=9, height=6, res=1000)
wrap_plots(plot_species_relabundance, legend_species, nrow = 1, widths = c(2, 1))
#dev.off()


################################################################################
################################################################################
# Active Community 
tse_active_barplot <- agglomerateByRank(tse_active, rank ="Species")

##################################
##Core community 
core_active_species = getPrevalence(
  tse_active, detection = 1, sort = TRUE, assay.type = "counts",
  as.relative = FALSE) %>% head(19)

#cat("There are 19 core active species in the microbiome")
#print(core_active_species)

core_active_species = names(core_active_species)
##################################


# Renaming the "Phylum" rank to keep only top taxa and the rest to "Other"
species_renamed <- lapply(rowData(tse_active_barplot)$Species, function(x){
  if (x %in% core_active_species) {x} else {"Other"}
})
rowData(tse_active_barplot)$Species <- as.character(species_renamed)

plots <- plotAbundance(tse_active_barplot, rank = "Species", 
                       assay.type = "relabundance", features = "Date")

# Modify the legend of the first plot to be smaller
plots[[1]] <- plots[[1]] +
  theme(
    legend.key.size = unit(0.3, 'cm'),
    legend.text = element_text(size = 8),
    legend.title = element_text(size = 10))

# Modify the legend of the second plot to be smaller
plots[[2]] <- plots[[2]] +
  theme(
    legend.key.height = unit(0.3, 'cm'),
    legend.key.width = unit(0.3, 'cm'),
    legend.text = element_text(size = 6),
    legend.title = element_text(size = 8),
    legend.direction = "vertical")


# Combine legends
legend <- wrap_plots(
  as_ggplot(get_legend(plots[[1]])),
  as_ggplot(get_legend(plots[[2]])),
  ncol = 1)

# Removes legends from the plots
plots[[1]] <- plots[[1]] + theme(legend.position = "none")
plots[[2]] <- plots[[2]] +
  theme(legend.position = "none", axis.title.x=element_blank())

# Combine plots
plot_active_species_relabundance <- wrap_plots(plots[[2]], plots[[1]], ncol = 1, heights = c(2, 10))
# Combine the plot with the legend

barplot_active_species = wrap_plots(plot_active_species_relabundance, legend, nrow = 1, widths = c(2, 1))
png(filename="figures/barplot_active_species.png" ,units = 'in',width=9, height=6, res=1000)
print(barplot_active_species )
dev.off()

################################################################################
################################################################################
