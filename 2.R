Unemployment2008_2009 <- read_excel("Unemployment2008-2009.xlsx")

dat <- Unemployment2008_2009

split <- strsplit(dat$`Education/Year`, "20")
dat$year <- sapply(split, "[",2)
dat$Education <- sapply(split, "[",1)

dat %>% 
  select(`Black, Unemployment rate`, `White, Unemployment rate`, `Asian, Unemployment rate`, `Hispanic, Unemployment rate`, year, Education) %>% 
  rename(Black = "Black, Unemployment rate",
         White = "White, Unemployment rate", 
         Asian = "Asian, Unemployment rate",
         Hispanic = "Hispanic, Unemployment rate") %>% 
  pivot_longer(Black:Hispanic, names_to = "Race", values_to = "Unemployment")
