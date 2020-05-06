
Economy2 <- read_excel("Economy2.xlsx")

cormat <- Economy2 %>% 
  select(-Year) %>% 
  cor()

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}


upper_tri <- get_upper_tri(cormat)
upper_tri


# Melt the correlation matrix
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Heatmap
library(ggplot2)
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_classic()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()


# Durable
t <- time(Economy2$Durable, quarters = 12, title = "Durable")

t$Forecast
ggsave("Durable_Forcast_changes.jpeg")
t$CV
ggsave("Durable_CV_changes.jpeg")

# Non-durable
t <- time(Economy2$Nondurable_goods, quarters = 12, title = "Non-durable")

t$Forecast
ggsave("NonDurable_Forcast_changes.jpeg")
t$CV
ggsave("NonDurable_CV_changes.jpeg")

## GDP
t <- time(Economy2$GDP, quarters = 12, title = "GDP")

t$Forecast
ggsave("GDP_Forcast_changes.jpeg")
t$CV
ggsave("GDP_CV_changes.jpeg")

