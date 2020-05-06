pacman::p_load(tidyverse, factoextra)
arrests_national <- read_csv("arrests_national.csv")

arrests_national_drug_1_ <- read_csv("arrests_national_drug (1).csv")

pe_1960_2018 <- read_csv("pe_1960_2018.csv")

dat <- arrests_national

  
dat %>% 
  filter(year != 1994) %>% 
  select(-c("id", "population",  "total_arrests")) %>% 
  pivot_longer(homicide:curfew_loitering, names_to = "Crime", values_to = "Reported") %>% 
  
  ggplot(aes(x = year, y = Reported, color = Crime))+
  geom_line()+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2001),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2011), lty = "dashed", size = 1)
  geom_text(aes(color = Crime, label = Crime))
  
dat %>% 
    filter(year != 1994) %>% 
    select(-c("id")) %>% 
  cor()

PCA.dat <- dat %>% 
  filter(year != 1994) %>% 
  select(-c("id", "population", "year", "total_arrests")) %>% 
  scale(scale = T)
  

pr.out <- prcomp(PCA.dat, scale = F)

pr.var <- (pr.out$sdev)^2

pve <- pr.var/sum(pr.var)

fviz_eig(pr.out)

fviz_pca_ind(pr.out,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pr.out,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pr.out, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
res.ind <- get_pca_ind(pr.out)

res.var <- get_pca_var(pr.out)
res.var$coord          # Coordinates

# Contributions to the PCs
res.var$contrib[,c(0:4)]       


## Create a PCA dataset

year <- dat$year[-23]

pca <- res.ind$cos2[,c(1:4)] %>% 
  data.frame() %>% 
  cbind(year)

pca %>% 
  Merritt::scatter_matrix()

pca %>% 
  pivot_longer(Dim.1:Dim.4, names_to = "Dimension", values_to = "Value") %>% 
  ggplot(aes(x = year, y = Value))+
  geom_line()+
  facet_wrap(~Dimension)+
  theme_classic()+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2001),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2011), lty = "dashed", size = 1)+
  geom_rect(aes(xmin=2000, xmax=2001, ymin=-Inf, ymax=Inf), alpha = .01)+
  geom_rect(aes(xmin=2008, xmax=2011, ymin=-Inf, ymax=Inf), alpha = .01)

ggsave("PCA_graph.jpeg",  width = 8, height = 5)

### PCA further

new <- dat %>% 
  filter(year != 1994) %>% 
  dplyr::select(liquor_laws, drunkenness, disorderly_conduct, dui, curfew_loitering, other, property_crime, other_assault, violent_crime, larceny, aggravated_assault, drug_abuse, motor_vehicle_theft, stolen_property, weapons) %>% 
  scale(scale = T)

pr.out <- prcomp(new, scale = F)

fviz_eig(pr.out)

fviz_pca_ind(pr.out,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(pr.out,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(pr.out, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)
res.ind <- get_pca_ind(pr.out)

res.var <- get_pca_var(pr.out)
res.var$coord          # Coordinates

# Contributions to the PCs
res.var$contrib[,c(0:4)]

pca <- res.ind$cos2[,c(1:4)] %>% 
  data.frame() %>% 
  cbind(year)

pca %>% 
  pivot_longer(Dim.1:Dim.4, names_to = "Dimension", values_to = "Value") %>% 
  ggplot(aes(x = year, y = Value))+
  geom_line()+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  facet_wrap(~Dimension)+
  geom_text(aes(label = "Resessions", x = 2005, y = .8))+
  theme_classic()

ggsave("Crime.jpeg", width = 8, height = 5)


## Lets just do mass counts

dat %>% 
  ggplot(aes(x = year, y = total_arrests))+
  geom_line()+ 
  theme_classic()+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2001),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2011), lty = "dashed", size = 1)+
  geom_rect(aes(xmin=2000, xmax=2001, ymin=-Inf, ymax=Inf), alpha = .01)+
  geom_rect(aes(xmin=2008, xmax=2011, ymin=-Inf, ymax=Inf), alpha = .01)

##
Housing_Index <- read_csv("Housing Index.csv")

Housing_Index %>% 
  rename(year = "yr") %>% 
  group_by(year) %>% 
  summarize(Index = mean(index_nsa)) %>% 
  filter(year > 1994) %>% 
  left_join(dat, by = "year") %>% 
  dplyr::select(year, total_arrests, Index) %>% 
  mutate(Arrests = scale(total_arrests, scale = T),
         `Housing Index` = scale(Index, scale = T)) %>% 
  pivot_longer(`Housing Index`:Arrests, names_to = "Variable", values_to = "values") %>% 
  ggplot(aes(x = year, y = values))+
  geom_line(aes(color = Variable))+
  theme_classic()+
  geom_vline(aes(xintercept = 2000),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2008), lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2001),  lty = "dashed", size = 1)+
  geom_vline(aes(xintercept = 2011), lty = "dashed", size = 1)+
  geom_rect(aes(xmin=2000, xmax=2001, ymin=-Inf, ymax=Inf), alpha = .01)+
  geom_rect(aes(xmin=2008, xmax=2011, ymin=-Inf, ymax=Inf), alpha = .01)+
  theme(legend.position = "bottom")+
  labs(color = "")
ggsave("ArrestsAndHousing.jpeg", width = 8, height = 6)

## Clustering
dat.scaled <- dat %>%
  filter(year != 1994) %>% 
  select(-c("id", "population", "year", "total_arrests")) %>% 
  scale()

fviz_nbclust(dat.scaled, kmeans, method = "gap_stat")

km.out <- kmeans(dat, 2, nstart = 20)

km.out$cluster

fviz_cluster(km.out, data = dat.scaled,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

res.hc <- dat.scaled %>%
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering

# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 2, # cut
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)
