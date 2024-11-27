suppressMessages({
  suppressWarnings({
    source(file = "scripts/01_install_load_packages.R")
    source(file = "scripts/02_import_clean_data.R")
    source(file = "scripts/06_exploration.R")
    source(file = "scripts/07_community_composition.R")
    source(file = "scripts/08_tree.R")
    source(file = "scripts/09_combined_barplot_func_cat_stress.R")
    source(file = "scripts/10_density_plots_stress_genes.R")
    source(file = "scripts/11_metaomics_stress_core_community.R")
    source(file = "scripts/12_feature_importance_analysis.R")
    source(file = "scripts/13_sulfates_time_series.R")
  })
})

################################################################################
#ls("package:mia")



