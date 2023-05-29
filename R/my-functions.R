resumo <- function(df,col){
  df %>% filter(depth == "0.0-0.1m") %>%
    group_by(treatment, date) %>%
    summarise({{col}} := mean({{col}}, na.rm=TRUE)) %>%
    ggplot(aes(x=date, y = {{col}}, color=treatment)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    scale_color_manual(values = c("red","blue","orange"))
}
