library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2); theme_set(theme_bw(base_size=12, base_family = "Times"))
library(egg)
install.packages(c("ggplot2", "ggthemes", "tidyverse", "extrafont",
                   "cowplot", "grid", "gridExtra", "ggrepel",
                   "reshape2", "ggforce", "ggridges", "shiny"))
getwd()
source("../R/bee_to_ts.R")

read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

social_raw <- read_excel_allsheets("all social bee pairs_events.xlsx")
solitary_raw <- read_excel_allsheets("all solitary bee pairs_events.xlsx") 

social <- social_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="social"
  )

solitary <- solitary_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="solitary"
  )

social_occ <- lapply(split(social, social$video), function(v){
  lapply(split(v, v$subject), function(x) {
    xt <- x %>%
      select(behavior, time, status) %>%
      spread(behavior, status)
    
    rl <- apply(xt[,-1], 2, function(y) {
      sum(rle(y)[[2]])
    })
    
    data.frame(
      behavior=names(rl),
      occ=unname(rl),
      subject=x$subject[1],
      video=x$video[1]
    )
  }) %>%
    bind_rows  
}) %>%
  bind_rows %>%
  merge(behavior_type) %>%
  mutate(
    type="social"
  )

solitary_occ <- lapply(split(solitary, solitary$video), function(v){
  lapply(split(v, v$subject), function(x) {
    xt <- x %>%
      select(behavior, time, status) %>%
      spread(behavior, status)
    
    rl <- apply(xt[,-1], 2, function(y) {
      sum(rle(y)[[2]])
    })
    
    data.frame(
      behavior=names(rl),
      occ=unname(rl),
      subject=x$subject[1],
      video=x$video[1]
    )
  }) %>%
    bind_rows  
}) %>%
  bind_rows %>%
  merge(behavior_type) %>%
  mutate(
    type="solitary"
  )

allbees_occ <- bind_rows(social_occ, solitary_occ)

allbees_occ_order <- allbees_occ %>%
  filter(type=="social") %>%
  group_by(behavior, category) %>%
  summarize(
    median=median(occ)
  ) %>%
  arrange(category, -median)

allbees_occ2 <- allbees_occ %>%
  ungroup %>%
  mutate(
    behavior=factor(behavior, level=allbees_occ_order$behavior)
  )

g1 <-  ggplot(allbees_occ2) +
     geom_boxplot(aes(behavior, occ, col=type, fill=type), alpha=0.2) +
    scale_x_discrete("Behavior") +
     scale_y_continuous("Occurrences") +
     scale_color_viridis_d(end=0.8) +
     scale_fill_viridis_d(end=0.8) +
     facet_grid(~category, scale="free_x", space='free') +
     theme(
         panel.grid = element_blank(),
         panel.spacing = unit(0, "cm"),
         axis.text.x = element_text(angle=45, hjust=1),
         axis.title.x = element_blank(),
         legend.title = element_blank(),
         legend.position = c(0.9, 0.7)
       )

g1
allbees <- bind_rows(social, solitary)

allbees_prop <- allbees %>%
  group_by(video, behavior, subject, category, type) %>%
  summarize(
    mean=mean(status)*1800
  ) %>%
  ungroup %>%
  mutate(
    behavior=factor(behavior, level=allbees_occ_order$behavior)
  )

g2 <- ggplot(allbees_prop) +
  geom_boxplot(aes(behavior, mean, col=type, fill=type), alpha=0.2) +
  scale_x_discrete("Behavior") +
  scale_y_sqrt("Total duration (seconds)", limits=c(0, 1800), breaks=c(0, 100, 500, 1000, 1500)) +
  scale_color_viridis_d(end=0.8) +
  scale_fill_viridis_d(end=0.8) +
  facet_grid(~category, scale="free_x", space='free') +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g2

allbees_avg <- allbees_prop %>%
  merge(allbees_occ) %>%
  mutate(
    avg=mean/occ,
    avg=ifelse(is.finite(avg), avg, 0)
  ) %>%
  ungroup %>%
  mutate(
    behavior=factor(behavior, level=allbees_occ_order$behavior)
  )


g3 <- ggplot(allbees_avg) +
  geom_boxplot(aes(behavior, avg, col=type, fill=type), alpha=0.2) +
  scale_x_discrete("Behavior") +
  scale_y_sqrt("Average duration (seconds)", limits=c(0, 1800), breaks=c(0, 100, 500, 1000, 1500)) +
  scale_color_viridis_d(end=0.8) +
  scale_fill_viridis_d(end=0.8) +
  facet_grid(~category, scale="free_x", space='free') +
  theme(
    panel.grid = element_blank(),
    panel.spacing = unit(0, "cm"),
    axis.text.x = element_text(angle=45, hjust=1),
    axis.title.x = element_blank(),
    legend.title = element_blank(),
    legend.position = "none"
  )

g3

gtot <- ggarrange(g1, g2, g3, nrow=3)

ggsave("figure_order.pdf", gtot, width=8, height=8)

allbees_occ_spread <- allbees_occ2 %>%
  select(-category) %>%
  spread(behavior, occ) 

pca_occ <- prcomp(allbees_occ_spread[,-c(1:3)], center=TRUE, scale = TRUE)

plot(pca_occ$x[,1], pca_occ$x[,2], col=as.numeric(factor(allbees_occ_spread$type)))

allbees_prop_spread <- allbees_prop %>%
  select(-category) %>%
  spread(behavior, mean)

pca_prop <- prcomp(allbees_prop_spread[,-c(1:3)], center=TRUE, scale = TRUE)

plot(pca_prop$x[,1], pca_prop$x[,2], col=as.numeric(factor(allbees_prop_spread$type)))

allbees_avg_spread <- allbees_avg %>%
  dplyr::select(-mean, -occ) %>%
  select(-category) %>%
  spread(behavior, avg)

pca_avg <- prcomp(allbees_avg_spread[,-c(1:3)], center=TRUE, scale = TRUE)

plot(pca_avg$x[,1], pca_avg$x[,2], col=as.numeric(factor(allbees_avg_spread$type)))

#CORRELATIONS
social2 <- social_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="social"
  )

solitary2 <- solitary_raw %>%
  lapply(bee_to_ts) %>%
  bind_rows(.id="video") %>%
  mutate(
    type="solitary"
  )

comb <- bind_rows(social2, solitary2) 

comb2 <- comb %>%
  group_by(video, time, subject, category, type) %>%
  summarize(
    status=any(status)
  ) %>%
  mutate(
    minute=floor(time/60)
  ) %>%
  group_by(video, minute, subject, category, type) %>%
  summarize(
    prop=mean(status)
  ) %>%
  group_by(video) %>%
  mutate(
    subject=factor(as.numeric(as.factor(subject)))
  ) 

time_prop<-ggplot(comb2) +
  geom_line(aes(minute, prop, col=subject)) +
  scale_x_continuous("Time (minutes)") +
  scale_y_continuous("Proportion observed within each minute") +
  facet_grid(category~type+video)

time_prop

comb3 <- comb2 %>%
  spread(subject, prop) %>%
  group_by(video, category, type) %>%
  summarize(
    corr=cor(`1`, `2`, use="pairwise.complete.obs", method="spearman")
  )

ggplot(comb3) + 
  geom_boxplot(aes(category, corr, fill=type))


#EXTRA
cor.test(~ category + type,
         data=comb,
         method= "spearman",
         continuity=FALSE,
         conf.level= 0.95) 

cor(comb[sapply(comb, is.numeric)]) 

comb$category<-as.numeric(as.factor(comb$category))
comb$type<-as.numeric(as.factor(comb$type))
