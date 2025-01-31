# plot_influence()

    Code
      rdchem_dt <- data.table::as.data.table(wooldridge::rdchem)[, .(rdintens, sales,
        profmarg)]
      formula_obj <- rdintens ~ sales + profmarg
      rdchem_influence_lst <- RregressPkg::plot_influence(df = rdchem_dt,
        formula_obj = formula_obj, influence_meas = "cook", label_threshold = 3,
        title = "Cook's Distance for Data Point Influence", subtitle = "Source: Wooldridge::rdchem",
        rot_y_tic_label = TRUE)
      a_plot <- rdchem_influence_lst$plot

