# Barplot of gene ontology classifications with ggplot 
library(tidyverse)
theme_set(theme_bw())

#https://cmdlinetips.com/2019/10/barplots-with-ggplot2-in-r/ 
# phd_field %>% 
#   group_by(broad_field,field) %>%
#   summarise(n=sum(n_phds, na.rm=TRUE)) %>%
#   arrange(desc(n))%>%
#   head(30)%>%
#   ggplot(aes(x=reorder(field,n),y=n, fill=broad_field)) +
#   xlab("field")+
#   geom_col() + coord_flip()

Data_Genes_Ontology <- read.delim("Data/Data_Genes_Ontology.txt")

dim(Data_Genes_Ontology)
head(Data_Genes_Ontology,5)

Data_Genes_Ontology %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology")+
  geom_col() + coord_flip() 

# Add labels # https://datavizpyr.com/how-to-add-labels-over-each-bar-in-barplot-in-r/ 
Data_Genes_Ontology %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology") +
  geom_col() + coord_flip() +
  geom_text(aes(label = signif(Percentage, digits = 3)), nudge_y = 4) +
  labs(title="Barplot with labels on bars",
       x="Genes_ontology", y= "Percentage (%)")
  
  
  
Data_Genes_Ontology %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology") +
  geom_col() + coord_flip() +
  geom_text(aes(label = round(Percentage, 2)), nudge_y = 4) +
  labs(title="Barplot with labels on bars",
       x="Genes_ontology", y= "Percentage (%)")

# The three QTLs classes https://drsimonj.svbtle.com/ordering-categories-within-ggplot2-facets 

Data_Genes_Ontology_all <- read.delim("Data/Data_Genes_Ontology_all.txt")
head(Data_Genes_Ontology_all,5)
colnames(Data_Genes_Ontology_all)[2] <- "Genes_Ontology"


Data_Genes_Ontology_all %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology") +
  geom_col() + coord_flip() +
  geom_text(aes(label = round(Percentage, 2)), nudge_y = 3, size = 3 ) +
  labs(title="Barplot with labels on bars",
       x="Genes_ontology", y= "Percentage (%)") +
  facet_grid(QTL.Class~., scales = "free")

# In order
Data_Genes_Ontology_all2 <- read.delim("Data/Data_Genes_Ontology_all2.txt")
head(Data_Genes_Ontology_all2,5)
colnames(Data_Genes_Ontology_all2)[2] <- "Genes_Ontology"


Data_Genes_Ontology_all2 %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology") +
  geom_col() + coord_flip() +
  geom_text(aes(label = round(Percentage, 2)), size = 3, position = position_stack(vjust = 0.5)) +
  labs(title="Barplot with labels on bars",
       x="Genes_ontology", y= "Percentage (%)") +
  facet_grid(QTL.Class~., scales = "free")


Data_Genes_Ontology_all2 %>% arrange(desc(Percentage)) %>% 
  ggplot(aes(x=reorder(Genes_Ontology,Percentage),y = Percentage, fill = Genes_Ontology)) +
  xlab("Genes_Ontology") +
  geom_col() + coord_flip() +
  #geom_text(aes(label = round(Percentage, 2)), size = 3, position = position_stack(vjust = 1)) +
  labs(title="Barplot with labels on bars",
       x="Genes_ontology", y= "Percentage (%)") +
  facet_grid(QTL.Class~., scales = "free")
