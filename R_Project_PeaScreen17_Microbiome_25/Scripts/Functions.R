# Function to tidy PERMANOVA table and create label to print PERMANOVA in ggplot as text.
create_permanova_label <- function(per, var_name_old = "asdf", var_name = ""){
  t.permanova <- per %>% data.frame()%>% 
    rownames_to_column(var = "Variable") %>% 
    mutate(p_new = if_else(Pr..F. <= 0.001, paste0("*P* < 0.001 "), 
                           paste0("*P* = ", signif(Pr..F., 2))),
           "R^2" = paste0(signif(R2 * 100, 2), "%"),
           across(where(is.numeric), ~ as.character(signif(., 2))),
           across(everything(), ~ replace_na(., "")),
           Variable = str_replace_all(Variable, var_name_old, var_name))
  
  
  # Prepare label
  lab_permanova <- paste0("**PERMANOVA** <Br>", t.permanova$Variable[1],
                          ": R<sup>*2*</sup> = ", t.permanova$`R^2`[1],
                          ", ", t.permanova$p_new[1])
  
  return(lab_permanova)
}



# Function for geom_richtext to add permanova to ordination
geom_add_permanova_ <- function(label = lab_per, hjust = 0, vjust = 1,
                                x = -Inf, y = Inf, stat = "unique", 
                                fill = "white", label.color = "white", size = 3,
                                label.padding = grid::unit(c(0.5, 0.5, 0.5, 0.75), 
                                                           "lines"),
                                label.r = unit(0, "lines"), color = "black", ...)  {
  geom_richtext(aes(x = x, y = y, label = label), hjust = hjust, vjust = vjust,
                stat = stat, fill = fill, label.color = label.color, size = size,
                label.padding = label.padding, label.r = label.r, color = color) 
} 


# Combined function for geom_add_permanova_() and create_permanova_label()
geom_add_permanova <- function(per = d.permanova, var_name_old = "asdf",
                               var_name = "", hjust = 0, vjust = 1,
                               x = -Inf, y = Inf, stat = "unique", 
                               fill = "white", label.color = "white", size = 3,
                               label.padding = grid::unit(c(0.5, 0.5, 0.5, 0.75), 
                                                          "lines"),
                               label.r = unit(0, "lines"), ...)  {
  geom_add_permanova_(label = create_permanova_label(per = d.permanova, 
                                                     var_name_old = var_name_old, 
                                                     var_name = var_name))
} 

# Function to plot model assumptions
plot.mod.vg <- function(mod){
  par(mfrow = c(1,2))
  plot1 <- plot(fitted(mod), resid(mod), xlab = "Fitted values", ylab = "Residuals", main = "Tukey-Anscombe plot")
  plot2 <- car::qqPlot(resid(mod), dist = "norm", mean = mean(resid(mod)), sd = sd(resid(mod)),xlab = "Theoretical quantiles", ylab = "Empirical quantiles", main = "Q-Q plot of residuals")
}

# Functions for visualizaiton with ggplot
gg_col_emerg <- function() {
  list(
    scale_color_manual(values = c("1" = "#440154", 
                                  "2" = "#3b528b",
                                  "3" = "#21918c",
                                  "4" = "#5ec962"),
                       name = c("emergence" = "Emergence")))}

gg_fill_emerg <- function() {
  list(
    scale_fill_manual(values = c("1" = "#440154",
                                 "2" = "#3b528b",
                                 "3" = "#21918c",
                                 "4" = "#5ec962"),
                       name = c("emergence" = "Emergence")))}


gg_col_RRI <- function() {
  list(
    scale_color_manual(values = c("3" = "#5ec962", 
                                  "4" = "#3b528b",
                                  "5" = "#440154"),
                       labels = c("3" = "low", 
                                  "4" = "medium",
                                  "5" = "high")))}

gg_col_RRI_g <- function() {
  list(
    scale_color_manual(name   = c("RRI_g" = "RRI"),
                       values = c("3" = "#5ec962", 
                                  "4" = "#3b528b",
                                  "5" = "#440154"),
                       labels = c("3" = "low", 
                                  "4" = "medium",
                                  "5" = "high")))}


# RsquareAdj works only (and is only valid) when the term can be regarded as the only term in the model
get_adjusted_r2 <- function(adonis_object, data) {
  adonis_object <- as.data.frame(adonis_object)
  n_observations <- nrow(data)
  d_freedom <- adonis_object %>% .[1,1]
  r2 <- adonis_object %>% .[1,3]
  adjusted_r2 <- RsquareAdj(r2, n_observations, d_freedom)
  adjusted_r2
}