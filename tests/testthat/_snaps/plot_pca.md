# plot_pca()

    Code
      measurements <- colnames(RregressPkg::pilots)[2:7]
      pilots_pca_lst <- RregressPkg::plot_pca(df = RregressPkg::pilots, measures = measurements,
      center = TRUE, scale. = TRUE, rank. = 4, aes_fill = "Group", pts_size = 2,
      x_limits = c(-4, 2), x_major_breaks = seq(-4, 2, 1), title = "Principal Components of Pilots and Apprentices",
      subtitle = "6 tested attributes from 20 pilots and 20 apprentices")
      pca <- pilots_pca_lst$pca
      pca_percent <- pilots_pca_lst$percent_var
      figure_plot <- pilots_pca_lst$figure_plot
      pca
    Output
      Standard deviations (1, .., p=6):
      [1] 1.3323392 1.1637937 1.0356884 0.9026604 0.7284317 0.6726049
      
      Rotation (n x k) = (6 x 4):
                             PC1        PC2        PC3        PC4
      Intelligence    0.40239072  0.3964661 -0.4617841 -0.3928149
      Form Relations -0.09715877  0.7472294  0.1752970 -0.1315611
      Dynamometer     0.38541311 -0.2181560  0.4329575 -0.7177525
      Dotting         0.54333623 -0.3144601  0.1065065  0.2453920
      Coordination   -0.31188931 -0.3559400 -0.6268314 -0.3992852
      Perservation    0.53629229  0.1062657 -0.4053555  0.3058981
    Code
      summary(pca)
    Output
      Importance of first k=4 (out of 6) components:
                                PC1    PC2    PC3    PC4
      Standard deviation     1.3323 1.1638 1.0357 0.9027
      Proportion of Variance 0.2959 0.2257 0.1788 0.1358
      Cumulative Proportion  0.2959 0.5216 0.7004 0.8362

