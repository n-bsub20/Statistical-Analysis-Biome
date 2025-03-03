

```r
# Script name: Analysis for HelloBiome Internship application
# Created on: Jan 2024
# Author: Neeraja Balasubrahmaniam
#
# 

# install packages
install.packages("readxl")
```

```
## Warning in install.packages :
##   package 'readxl' is in use and will not be installed
```

```r
install.packages("tidyverse")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("vegan")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("writexl")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("ecolTest")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("RColorBrewer")
```

```
## Error in install.packages : Updating loaded packages
```

```r
install.packages("glue")
```

```
## Error in install.packages : Updating loaded packages
```

```r
# load packages
library(readxl)
library(tidyverse)
library(vegan)
library(writexl)
library(ecolTest)
library(RColorBrewer)
library(glue)

# package version
packageVersion("readxl")
```

```
## [1] '1.4.3'
```

```r
packageVersion("tidyverse")
```

```
## [1] '2.0.0'
```

```r
packageVersion("vegan")
```

```
## [1] '2.6.4'
```

```r
packageVersion("writexl")
```

```
## [1] '1.4.2'
```

```r
setwd("C:/Users/neerj/OneDrive/Desktop/HelloBiome/N_study")
```

```r
# import metadata

metadata = read_xlsx("N_metadata.xlsx", sheet = "eTBjHroY")
```

```
## New names:
## • `Other` -> `Other...6`
## • `Other` -> `Other...13`
## • `Other` -> `Other...23`
## • `Other` -> `Other...26`
## • `Other` -> `Other...39`
## • `Other` -> `Other...42`
## • `Other` -> `Other...44`
## • `Other` -> `Other...47`
## • `Itchiness` -> `Itchiness...55`
## • `Dryness` -> `Dryness...56`
## • `Oiliness` -> `Oiliness...58`
## • `None` -> `None...61`
## • `Other` -> `Other...62`
## • `Dandruff` -> `Dandruff...63`
## • `Scalp acne` -> `Scalp acne...73`
## • `Other` -> `Other...74`
## • `None` -> `None...88`
## • `Other` -> `Other...89`
## • `None` -> `None...100`
## • `Other` -> `Other...101`
## • `Itchiness` -> `Itchiness...102`
## • `Dryness` -> `Dryness...103`
## • `Dandruff` -> `Dandruff...104`
## • `Oiliness` -> `Oiliness...106`
## • `Scalp acne` -> `Scalp acne...108`
## • `None` -> `None...111`
## • `Other` -> `Other...112`
## • `None` -> `None...117`
## • `Other` -> `Other...118`
## • `Other` -> `Other...123`
## • `None` -> `None...130`
## • `Other` -> `Other...131`
## • `None` -> `None...136`
## • `Other` -> `Other...137`
## • `Other` -> `Other...140`
```

```r
names(metadata)<-make.names(names(metadata),unique = TRUE)


colnames(metadata) = colnames(metadata) %>%                                    
  str_replace("What.gender.do.you.identify.with.", "Gender") %>%
  str_replace("Group", "Age") %>%
  str_replace(colnames(metadata)[24],"Skin.type") %>%
  str_replace(colnames(metadata)[25],"Skin.tone") %>%
  str_replace(colnames(metadata)[27],"Skin.reaction") %>%
  str_replace(colnames(metadata)[40],"Stress.level") %>%
  str_replace(colnames(metadata)[50],"Hair.loss") %>%
  str_replace(colnames(metadata)[54],"Scalp.type") %>%
  str_replace(colnames(metadata)[55],"Itchiness") %>%
  str_replace(colnames(metadata)[57],"Flaking")
  
names(metadata)<-make.names(names(metadata),unique = TRUE) #make unique colnames again


#Reactive skin 1/0 replace with str
metadata$Skin.reaction[metadata$Skin.reaction == 1] = 'Reactive'
metadata$Skin.reaction[metadata$Skin.reaction == 0] = 'Not.Reactive'

#Flaking replace with str "yes/no"
metadata$Flaking[metadata$Flaking == "Flaking"] = 'Yes'
metadata$Flaking[is.na(metadata$Flaking)==TRUE] = 'No'

#Itchiness replace with str "yes/no"
metadata$Itchiness[metadata$Itchiness == "Itchiness"] = 'Yes'
metadata$Itchiness[is.na(metadata$Itchiness)==TRUE] = 'No'


df = metadata

df = df %>%                                          # Combine Gender
  dplyr::mutate(Gender = invoke(coalesce, across(all_of(colnames(df)[5:6])))) %>%
  dplyr::select(Gender, colnames(df)[! colnames(df) %in% colnames(df)[5:6]])

df = df %>%                                          # Combine Ethnicity
  dplyr::mutate(Ethnicity = invoke(coalesce, across(all_of(colnames(df)[6:12])))) %>%
  dplyr::select(Ethnicity, colnames(df)[! colnames(df) %in% colnames(df)[6:12]])

df = df %>%                                          # Combine Hormonal status
  dplyr::mutate(Hormonal.status = invoke(coalesce, across(all_of(colnames(df)[7:16])))) %>%
  dplyr::select(Hormonal.status, colnames(df)[! colnames(df) %in% colnames(df)[7:16]])

df = df %>%                                          # Combine Skin tone
  dplyr::mutate(Skin.tone = invoke(coalesce, across(all_of(colnames(df)[9:10])))) %>%
  dplyr::select(Skin.tone, colnames(df)[! colnames(df) %in% colnames(df)[9:10]])

df = df %>%                                          # Combine Diet
  dplyr::mutate(Diet = invoke(coalesce, across(all_of(colnames(df)[11:22])))) %>%
  dplyr::select(Diet, colnames(df)[! colnames(df) %in% colnames(df)[11:22]])

df <- df[, c(colnames(df)[6:9], colnames(df)[5:3], colnames(df)[22], colnames(df)[26:27],colnames(df)[29])]

df[is.na(df)] <- "N/A"

#Pie of Gender
df %>%
  count(Gender) %>% 
  mutate(Gender = factor(Gender), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Gender)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Gender")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-1.png)

```r
#Pie of Hormonal Status
df[df=="I have been pregnant within the last year and half, still breastfeeding"] <- "Still breastfeeding"
df[df=="I have been pregnant within the past year"] <- "Pregnant within past year"
df[df=="I have been through menopause"] <- "Been through menopause"


df %>%
  count(Hormonal.status) %>% 
  mutate(Hormonal.status = factor(Hormonal.status), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Hormonal.status)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Hormonal status")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-2.png)

```r
#Pie of Ethnicity
df %>%
  count(Ethnicity) %>% 
  mutate(Ethnicity = factor(Ethnicity), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Ethnicity)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Ethnicity")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-3.png)

```r
#Pie of Age
df %>%
  count(Age) %>% 
  mutate(Age = factor(Age), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Age)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Age group")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-4.png)

```r
#Pie of Scalp type
df %>%
  count(Scalp.type) %>% 
  mutate(Scalp.type = factor(Scalp.type), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Scalp.type)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Scalp type")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-5.png)

```r
#Pie of Hair loss
df %>%
  count(Hair.loss) %>% 
  mutate(Hair.loss = factor(Hair.loss), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Hair.loss)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Hair loss")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-6.png)

```r
#Pie of Flaking
df %>%
  count(Flaking) %>% 
  mutate(Flaking = factor(Flaking), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Flaking)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Flaking")) +
  theme_void()+
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-7.png)

```r
#Pie of Itchiness
df %>%
  count(Itchiness) %>% 
  mutate(Itchiness = factor(Itchiness), percentage = n / sum(n)) %>% 
  ggplot(aes(x = "", percentage, fill = Itchiness)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label(aes(label = scales::percent(percentage)),
             color = "white",fontface = "bold", size = 9,
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  guides(fill = guide_legend(title = "Itchiness")) +
  theme_void() +
  theme(legend.text=element_text(size=14), legend.title=element_text(size=14))
```

![plot of chunk Import data](figure/Import data-8.png)



```r
library(ecolTest)

#Find Kit ID numbers corresponding to front and back

df_front.back = df[,2:3]


df_front.back_longer = df_front.back %>% 
  pivot_longer(
    cols = c(Kit.ID..FRONT., Kit.ID..BACK.),
    names_to = "Sampling.site",
    values_to = "Kit.ID"
  ) 

df_front.back_longer[df_front.back_longer=="Kit.ID..FRONT."] = "Front"
df_front.back_longer[df_front.back_longer=="Kit.ID..BACK."] = "Back"

###################################################
########Fungal Analysis##########################
#######################################################

##Part 2 Fungal Analysis

#Load fungal datasets using lapply as a list of dataframes

file.list <- list.files(pattern=c(".*fungal.*?\\.csv"), full.names = TRUE)
df.list <- lapply(file.list, read.csv, header=TRUE)
names(df.list) <- file.list # assign names to list items

#Combine all dataframes from within list by using merge (or outer_join) to get a combine fungal load df

list_df = list(df.list[[1]], df.list[[2]], df.list[[3]], 
               df.list[[4]], df.list[[5]], df.list[[6]]) # make a list of dataframes
df_taxa_fungal <- list_df %>% reduce(merge, by=c("species_name","id"), all = TRUE) #all = TRUE == outer_join

#Make NAs to 0s, remove long_taxa column, remove X from kitID names, convert KitID to char
df_taxa_fungal[is.na(df_taxa_fungal)] <- 0
df_taxa_fungal = as.data.frame(t(df_taxa_fungal[,-2]))
colnames(df_taxa_fungal)=df_taxa_fungal[1,]
df_taxa_fungal = df_taxa_fungal[-1,]
df_taxa_fungal <- tibble::rownames_to_column(df_taxa_fungal, "Kit.ID")
df_taxa_fungal$Kit.ID<-gsub("X","",as.character(df_taxa_fungal$Kit.ID))
df_taxa_fungal$Kit.ID<-as.character(df_taxa_fungal$Kit.ID)

#Get Sampling site column in df_taxa_fungal dataframe

df_fungal_taxa_site <- merge(df_front.back_longer, df_taxa_fungal, by="Kit.ID", all = TRUE)
df_fungal_taxa_site = na.omit(df_fungal_taxa_site) #Remove the extra columns not having taxa info (NA rows)

#Make long format OTU/Species dataframe
df_fungal_taxa_site_longer = df_fungal_taxa_site %>% 
  pivot_longer(
    cols = colnames(df_fungal_taxa_site)[3:126],
    names_to = "Species",
    values_to = "Count"
  ) 

#Make Count column numeric
df_fungal_taxa_site_longer$Count<-as.numeric(df_fungal_taxa_site_longer$Count)
df_fungal_count_longer = df_fungal_taxa_site_longer

#Find fungal load for each Sample ID/Kit.ID

df_fungal_load = df_fungal_count_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), load_n = sum(Count), std = sd(Count))

  
#Plot log10(fungal load) in each Sample ID bar plot

ggplot(df_fungal_load, aes(x=Kit.ID, y = load_n, fill=Sampling.site))+ 
  geom_bar(stat="identity", position="dodge", width = 0.8)+
  geom_errorbar(aes(x=Kit.ID, ymin=load_n-std, ymax=load_n+std), 
                width=0.4, colour="black", alpha=0.9, size=0.8) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18))+
  theme(axis.text.y   = element_text(size=18, color = "black", family = "sans"),
        axis.text.x   = element_text(size=10, color = "black", family = "sans"),
        axis.title.y  = element_text(size=18, color = "black", family = "sans"),
        axis.title.x  = element_text(size=18, color = "black", family = "sans"),
        legend.text = element_text(size=18, color = "black", family = "sans"),
        legend.title = element_text(size=18, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  labs(x="Sampling ID", y="log10(Fungal load)", fill=NULL)
```

![plot of chunk Load libraries](figure/Load libraries-1.png)

```r
#Find fungal Shannon diversity to compare between Sampling site

df_fungal_shannon = df_fungal_count_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), shannon = diversity(Count, index = "shannon"))
            
#Group and aggregate/summarize by Sampling.site for Hutcheson's t-test between Sampling site groups
df_fungal_site_agg = df_fungal_shannon %>%
  group_by(Sampling.site) %>%
  pivot_wider(names_from = Sampling.site, values_from = shannon, values_fill = 0) %>%
  group_by(Kit.ID) %>%
  summarise(across(c(Front, Back), sum))

#Hutcheson's t-test between Sampling site groups for Shannon diversity indices: two-sided test
Hutcheson_t_test(df_fungal_site_agg$Front, df_fungal_site_agg$Back, shannon.base = 10)
```

```
## 
## 	Hutcheson t-test for two communities
## 
## data:  df_fungal_site_agg$Front ,  df_fungal_site_agg$Back
## Hutcheson t-statistic = -0.2304, df = 17.265, p-value = 0.8205
## alternative hypothesis: true difference in H' is not equal to 0
## sample estimates:
##        x        y 
## 1.148330 1.175177
```

```r
#Analysis of Variance to compare Shannon diversity between Sampling site groups
sppdiv_aov <- aov(shannon ~ Sampling.site, data = df_fungal_shannon)
summary(sppdiv_aov)
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)
## Sampling.site  1  0.097 0.09669   0.833  0.367
## Residuals     36  4.178 0.11605
```

```r
print("The communities are not significantly different based on Shannon diversity indices")
```

```
## [1] "The communities are not significantly different based on Shannon diversity indices"
```

```r
#Plot Shannon dversity box plot

df_fungal_shannon %>%
  ggplot(mapping = aes(x = Sampling.site, y = shannon, fill = `Sampling.site`)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(colour = "black", linewidth = 0.8) +
  labs(x = "Sampling site", y = "Shannon diversity index") +
  theme(axis.title.y = element_text(size = 14, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(size = 14, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(size = 12, hjust = 0, color = "black")) +
  theme(legend.text = element_text(size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank())
```

![plot of chunk Load libraries](figure/Load libraries-2.png)

```r
#Transforming data for Bray-Curtis

df_fungal_taxa_site_meta = df_fungal_taxa_site
rownames(df_fungal_taxa_site_meta) = df_fungal_taxa_site_meta[,1]
df_fungal_taxa_site_meta = df_fungal_taxa_site_meta[,-1] #with Sampling site info column
df_fungal_taxa_site_bray = df_fungal_taxa_site_meta[,-1]
df_fungal_taxa_site_bray <- df_fungal_taxa_site_bray %>% mutate_if(is.character, as.numeric)

bray_fungal <- vegdist(df_fungal_taxa_site_bray, method = "bray")
bray_fungal_matrix = as.matrix(bray_fungal)
```

```r
pcoa <- wcmdscale(bray_fungal, eig=TRUE, k=2)

positions <- pcoa$points
positions=as.data.frame(positions)
colnames(positions) <- c("PCoA1", "PCoA2")

positions2=cbind(df_fungal_taxa_site_meta[,1], positions)
colnames(positions2)[1] <- "Sampling.site"

percent_explained <- 100 * pcoa$eig / sum(pcoa$eig)

pretty_pe <- format(round(percent_explained, digits =1), nsmall=1, trim=TRUE)

labels <- c(glue("PC1 ({pretty_pe[1]}%)"),
            glue("PC2 ({pretty_pe[2]}%)"))


ggplot(positions2, aes(x = PCoA1, y = PCoA2, color = Sampling.site, shape = Sampling.site)) +
  geom_point(size=5) +
  scale_shape_manual(values=c(15, 17, 19)) +
  # scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
  stat_ellipse(geom = "polygon",
               aes(fill = Sampling.site), 
               alpha = 0, linewidth = 1) +
  xlab(labels[1]) +   #----------get the variance% value from pc_pct_variance[1]*100 for PC1
  ylab(labels[2]) +    #----------get the variance% value from pc_pct_variance[2]*100 for PC2
  #xlim(-0.55,0.45) +    #changing plot X axis limits
  #ylim(-0.5,0.45) +    #changing plot Y axis limits
  theme(axis.text.y   = element_text(size=20, color = "black", family = "sans"),
        axis.text.x   = element_text(size=20, color = "black", family = "sans"),
        axis.title.y  = element_text(size=20, color = "black", family = "sans"),
        axis.title.x  = element_text(size=20, color = "black", family = "sans"),
        legend.text = element_text(size=20, color = "black", family = "sans"),
        legend.title = element_text(size=20, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  coord_fixed(ratio = 0.9)
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-1.png)

```r
#Are groups different based on Sampling.site?: Using Permanova testing

PERMANOVA_Site <- adonis2(bray_fungal_matrix ~ Sampling.site, data = df_fungal_taxa_site_meta, 
                        permutations = 10000)
print(PERMANOVA_Site)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = bray_fungal_matrix ~ Sampling.site, data = df_fungal_taxa_site_meta, permutations = 10000)
##               Df SumOfSqs      R2     F Pr(>F)
## Sampling.site  1   0.2369 0.02179 0.802 0.5665
## Residual      36  10.6361 0.97821             
## Total         37  10.8730 1.00000
```

```r
print("No significant differences in groups based on Sampling Site")
```

```
## [1] "No significant differences in groups based on Sampling Site"
```

```r
#Group and aggregate/summarize by Species and get top n species within each Sampling site group
df_fungal_species_top = df_fungal_count_longer %>%
  group_by(Sampling.site, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
#Statistical differences groups based on top 10 species: using PERMANOVA from Adonis2

df_fungal_taxa_site_top10 = df_fungal_taxa_site_bray%>%
  select(df_fungal_species_top$Species)

PERMANOVA_Site_top10 <- adonis2(df_fungal_taxa_site_top10 ~ Sampling.site, data = df_fungal_taxa_site_meta,
                                permutations = 10000)

print(PERMANOVA_Site_top10)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = df_fungal_taxa_site_top10 ~ Sampling.site, data = df_fungal_taxa_site_meta, permutations = 10000)
##               Df SumOfSqs      R2     F Pr(>F)
## Sampling.site  1   0.2396 0.02214 0.815 0.5598
## Residual      36  10.5854 0.97786             
## Total         37  10.8250 1.00000
```

```r
print("No significant differences in groups based on Sampling Site by top10 species")
```

```
## [1] "No significant differences in groups based on Sampling Site by top10 species"
```

```r
#Plot top 10 within each Sampling group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

ggplot(data = df_fungal_species_top) +
  geom_col(mapping = aes(x = Sampling.site, y = Count, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Sampling site", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = col_combined)
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-2.png)

```r
  #facet_wrap(vars(Sampling.site))

#Find Kit ID numbers corresponding to front and back

df_front.back2 = df[,c(2:4,7:11)]


df_front.back_longer2 = df_front.back2 %>% 
  pivot_longer(
    cols = c(Kit.ID..FRONT., Kit.ID..BACK.),
    names_to = "Sampling.site",
    values_to = "Kit.ID"
  ) 

df_front.back_longer2[df_front.back_longer2=="Kit.ID..FRONT."] = "Front"
df_front.back_longer2[df_front.back_longer2=="Kit.ID..BACK."] = "Back"
df_meta_all = df_front.back_longer2

#Get Scalp type column in df_taxa_fungal dataframe

df_fungal_taxa_scalp <- merge(df_meta_all, df_taxa_fungal, by="Kit.ID", all = TRUE)
df_fungal_taxa_scalp = na.omit(df_fungal_taxa_scalp) #Remove the extra columns not having taxa info (NA rows)

#Make long format OTU/Species dataframe
df_fungal_taxa_scalp_longer = df_fungal_taxa_scalp %>% 
  pivot_longer(
    cols = colnames(df_fungal_taxa_scalp)[9:132],
    names_to = "Species",
    values_to = "Count"
  ) 

#Make Count column numeric
df_fungal_taxa_scalp_longer$Count<-as.numeric(df_fungal_taxa_scalp_longer$Count)
df_fungal_count_scalp_longer = df_fungal_taxa_scalp_longer

#Group and aggregate/summarize by Scalp.type+Species and get top n species within each such group
df_fungal_species_scalp_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Scalp.type, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Scalp.type'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_scalp = list(df_fungal_species_scalp_top, df_fungal_species_top)
df_fungal_species_scalp_top <- list_df_scalp %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+scalp.type group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_scalp_top) +
  geom_col(mapping = aes(x = Scalp.type, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Scalp type", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", angle = 90, hjust = 1,
                                   size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-3.png)

```r
#Group and aggregate/summarize by Hair.loss+Species and get top n species within each such group
df_fungal_species_hair_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Hair.loss, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Hair.loss'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_hair = list(df_fungal_species_hair_top, df_fungal_species_top)
df_fungal_species_hair_top <- list_df_hair %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+hair.loss group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_hair_top) +
  geom_col(mapping = aes(x = Hair.loss, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Hair loss", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", angle = 90, hjust = 1,
                                   size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-4.png)

```r
#Group and aggregate/summarize by Flaking+Species and get top n species within each such group
df_fungal_species_flaking_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Flaking, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Flaking'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
list_df_flaking = list(df_fungal_species_flaking_top, df_fungal_species_top)
df_fungal_species_flaking_top <- list_df_flaking %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+Flaking group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_flaking_top) +
  geom_col(mapping = aes(x = Flaking, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Flaking", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-5.png)

```r
#Group and aggregate/summarize by Itchiness+Species and get top n species within each such group
df_fungal_species_itchiness_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Itchiness, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Itchiness'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_itchiness = list(df_fungal_species_itchiness_top, df_fungal_species_top)
df_fungal_species_itchiness_top <- list_df_itchiness %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+Itchiness group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_itchiness_top) +
  geom_col(mapping = aes(x = Itchiness, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Itchiness", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-6.png)

```r
##===End of Part 2 Fungal Analysis===========================

##======Part 3 Fungal Analysis-----------------

##Shannon diversity
#Find fungal Shannon diversity to compare between Sampling site

df_fungal_shannon2 = df_fungal_count_scalp_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), Age = first(Age), shannon = diversity(Count, index = "shannon"))

#Group and aggregate/summarize by Sampling.site for Hutcheson's t-test between Sampling site+Age groups
df_fungal_site_agg2 = df_fungal_shannon2 %>%
  group_by(Sampling.site, Age) %>%
  pivot_wider(names_from = c(Sampling.site, Age), values_from = shannon, values_fill = 0) 

#Hutcheson's t-test between Sampling site groups for Shannon diversity indices: two-sided test
multiple_Hutcheson_t_test(x = df_fungal_site_agg2[,2:5], shannon.base = 10)
```

```
## $p.values
##                   Back_30 H = 0.97 Front_40 H = 0.76 Front_30 H = 0.93 Back_40 H = 0.76
## Back_30 H = 0.97                NA           0.09921           0.41079          0.10657
## Front_40 H = 0.76          0.09921                NA           0.17945          0.49300
## Front_30 H = 0.93          0.41079           0.17945                NA          0.18308
## Back_40 H = 0.76           0.10657           0.49300           0.18308               NA
```

```r
#Analysis of Variance to compare Shannon diversity between Sampling site+Age groups
sppdiv_aov2 <- aov(shannon ~ Sampling.site * Age, data = df_fungal_shannon2)
summary(sppdiv_aov2)
```

```
##                   Df Sum Sq Mean Sq F value Pr(>F)
## Sampling.site      1  0.097 0.09669   0.827  0.370
## Age                1  0.136 0.13606   1.163  0.288
## Sampling.site:Age  1  0.065 0.06454   0.552  0.463
## Residuals         34  3.977 0.11698
```

```r
print("The communities are not significantly different based on Shannon diversity indices")
```

```
## [1] "The communities are not significantly different based on Shannon diversity indices"
```

```r
#Plot Shannon diversity box plot

df_fungal_shannon2 %>%
  ggplot(mapping = aes(x = Age, y = shannon, fill = `Sampling.site`)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(colour = "black", linewidth = 0.8) +
  labs(x = "Sampling site", y = "Shannon diversity index") +
  theme(axis.title.y = element_text(size = 14, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(size = 14, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(size = 12, hjust = 0, color = "black")) +
  theme(legend.text = element_text(size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank())+
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting using Bray-Curtis distances](figure/PCoA plotting using Bray-Curtis distances-7.png)

```r
##Bray-Curtis groups by Sampling.site + Age

#Transforming data for Bray-Curtis

df_fungal_taxa_scalp_meta = df_fungal_taxa_scalp
rownames(df_fungal_taxa_scalp_meta) = df_fungal_taxa_scalp_meta[,1]
df_fungal_taxa_scalp_meta = df_fungal_taxa_scalp_meta[,-1] #with Sampling site info column
df_fungal_taxa_scalp_meta = df_fungal_taxa_scalp_meta[,-c(2:6)]
df_fungal_taxa_scalp_meta$Site_age = paste(df_fungal_taxa_scalp_meta$Sampling.site, 
                                           df_fungal_taxa_scalp_meta$Age, sep="_")
df_fungal_taxa_scalp_meta = df_fungal_taxa_scalp_meta %>% relocate(Site_age, .after = Sampling.site)

df_fungal_taxa_scalp_bray = df_fungal_taxa_scalp_meta[,-c(1:3)] #remove meta columns for bray input matrix
df_fungal_taxa_scalp_bray <- df_fungal_taxa_scalp_bray %>% mutate_if(is.character, as.numeric)

bray_fungal2 <- vegdist(df_fungal_taxa_scalp_bray, method = "bray")
bray_fungal_matrix2 = as.matrix(bray_fungal2)
```

```r
pcoa2 <- wcmdscale(bray_fungal2, eig=TRUE, k=2)

positions2 <- pcoa2$points
positions2=as.data.frame(positions2)
colnames(positions2) <- c("PCoA1", "PCoA2")

positions2.2=cbind(df_fungal_taxa_scalp_meta[,c(1:3)], positions)
colnames(positions2.2)[1:3] <- c("Age", "Sampling.site","Sampling.site&Age")

percent_explained2 <- 100 * pcoa2$eig / sum(pcoa2$eig)

pretty_pe2 <- format(round(percent_explained2, digits =1), nsmall=1, trim=TRUE)

labels2 <- c(glue("PC1 ({pretty_pe2[1]}%)"),
            glue("PC2 ({pretty_pe2[2]}%)"))


ggplot(positions2.2, aes(x = PCoA1, y = PCoA2, color = Age, shape = Age)) +
  geom_point(size=5) +
  scale_shape_manual(values=c(15, 16, 17, 18)) +
  # scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
  stat_ellipse(geom = "polygon",
               aes(fill = Age), 
               alpha = 0, linewidth = 1, linetype = 2) +
  xlab(labels2[1]) +   #----------get the variance% value from pc_pct_variance[1]*100 for PC1
  ylab(labels2[2]) +    #----------get the variance% value from pc_pct_variance[2]*100 for PC2
  #xlim(-0.55,0.45) +    #changing plot X axis limits
  #ylim(-0.5,0.45) +    #changing plot Y axis limits
  theme(axis.text.y   = element_text(size=20, color = "black", family = "sans"),
        axis.text.x   = element_text(size=20, color = "black", family = "sans"),
        axis.title.y  = element_text(size=20, color = "black", family = "sans"),
        axis.title.x  = element_text(size=20, color = "black", family = "sans"),
        legend.text = element_text(size=20, color = "black", family = "sans"),
        legend.title = element_text(size=20, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  coord_fixed(ratio = 0.9)
```

![plot of chunk PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)](figure/PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)-1.png)

```r
#Are groups different based on Sampling.site and Age?: Using Permanova testing


#Group and aggregate/summarize by Age+Species and get top n species within each such group
#Groups: Front+30, Front+40, Back+30, Back+40
df_fungal_species_age_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Age, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Age'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
#Statistical differences groups based on top 10 species: using PERMANOVA from Adonis2

df_fungal_taxa_age_top10 = df_fungal_taxa_scalp_bray%>%
  select(df_fungal_species_age_top$Species)

PERMANOVA_Age_top10 <- adonis2(df_fungal_taxa_age_top10 ~ Sampling.site*Age, data =
                                 df_fungal_taxa_scalp_meta, permutations = 10000)

print(PERMANOVA_Age_top10)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = df_fungal_taxa_age_top10 ~ Sampling.site * Age, data = df_fungal_taxa_scalp_meta, permutations = 10000)
##                   Df SumOfSqs      R2      F Pr(>F)  
## Sampling.site      1   0.2371 0.02186 0.8441 0.5408  
## Age                1   0.7334 0.06762 2.6111 0.0204 *
## Sampling.site:Age  1   0.3257 0.03003 1.1595 0.3044  
## Residual          34   9.5499 0.88049                
## Total             37  10.8460 1.00000                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
print("Significant differences in groups based on Age by top10 species, not by Sampling.site")
```

```
## [1] "Significant differences in groups based on Age by top10 species, not by Sampling.site"
```

```r
### Note: I used top 10 species within each of these 4 groups. However, the analysis below, uses
### only the top10 species within each Sampling.site like before and compared across Age as there were
### too many species in the legend. Question was unclear on this.

##################
#Combine (To Use only previously used top10 species by Sampling.site)
list_df_age = list(df_fungal_species_age_top, df_fungal_species_top)
df_fungal_species_age_top <- list_df_age %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join

##Plot top 10 within each Sampling.site+Age group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_age_top) +
  geom_col(mapping = aes(x = Age, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Age", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)](figure/PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)-2.png)

```r
#Compare across pre-menopause groups

#Group and aggregate/summarize by Itchiness+Species and get top n species within each such group
df_fungal_species_hormone_top = df_fungal_count_scalp_longer %>%
  group_by(Sampling.site, Age, Hormonal.status, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Age', 'Hormonal.status'. You can override using the
## `.groups` argument.
## Selecting by Count
```

```r
list_df_hormone = list(df_fungal_species_hormone_top, df_fungal_species_top)
df_fungal_species_hormone_top <- list_df_hormone %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join

#subset by hormonal status:"None of these" for premenopause
df_fungal_species_premenopause_top = subset(df_fungal_species_hormone_top, Hormonal.status == 'None of these')

##Plot top 10 within each Sampling.site+Age+premenopausal group
#Define color scales
col_Clado = colorRampPalette(c("green", "darkgreen"))(2)
col_Mal = colorRampPalette(c("lightblue", "darkblue"))(7)
col_combined = c("red", "yellow", col_Clado, col_Mal, "purple", "black")

#Plot
ggplot(data = df_fungal_species_premenopause_top) +
  geom_col(mapping = aes(x = Age, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Age", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)](figure/PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)-3.png)

```r
###################################################
########Bacterial Analysis##########################
#######################################################

##Part 2 Bacterial Analysis

#Load bacterial datasets using lapply as a list of dataframes

file.list2 <- list.files(pattern=c(".*bac.*?\\.csv"), full.names = TRUE)
df.list2 <- lapply(file.list2, read.csv, header=TRUE)
names(df.list2) <- file.list2 # assign names to list items

#Combine all dataframes from within list by using merge (or outer_join) to get a combine fungal load df

list_df2 = list(df.list2[[1]], df.list2[[2]], df.list2[[3]], 
               df.list2[[4]], df.list2[[5]], df.list2[[6]]) # make a list of dataframes
df_taxa_bac <- list_df2 %>% reduce(merge, by=c("species_name","id"), all = TRUE) #all = TRUE == outer_join

#Make NAs to 0s, remove long_taxa column, remove X from kitID names, convert KitID to char
df_taxa_bac[is.na(df_taxa_bac)] <- 0
df_taxa_bac = df_taxa_bac[,-2]
df_taxa_bac = aggregate(df_taxa_bac[,-1], df_taxa_bac["species_name"] ,FUN=sum) #sums duplicated Species columns
df_taxa_bac = as.data.frame(t(df_taxa_bac))
colnames(df_taxa_bac)=df_taxa_bac[1,]
df_taxa_bac = df_taxa_bac[-1,]
df_taxa_bac <- tibble::rownames_to_column(df_taxa_bac, "Kit.ID")
df_taxa_bac$Kit.ID<-gsub("X","",as.character(df_taxa_bac$Kit.ID))
df_taxa_bac$Kit.ID<-as.character(df_taxa_bac$Kit.ID)

#Get Sampling site column in df_taxa_bac dataframe

df_bac_taxa_site <- merge(df_front.back_longer, df_taxa_bac, by="Kit.ID", all = TRUE)
df_bac_taxa_site = na.omit(df_bac_taxa_site) #Remove the extra columns not having taxa info (NA rows)

#Make long format OTU/Species dataframe
df_bac_taxa_site_longer = df_bac_taxa_site %>% 
  pivot_longer(
    cols = colnames(df_bac_taxa_site)[3:202],
    names_to = "Species",
    values_to = "Count"
  ) 

#Make Count column numeric
df_bac_taxa_site_longer$Count<-as.numeric(df_bac_taxa_site_longer$Count)
df_bac_count_longer = df_bac_taxa_site_longer

#Find bacterial load for each Sample ID/Kit.ID

df_bac_load = df_bac_count_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), load_n = sum(Count), std = sd(Count))


#Plot log10(fungal load) in each Sample ID bar plot

ggplot(df_bac_load, aes(x=Kit.ID, y = load_n, fill=Sampling.site))+ 
  geom_bar(stat="identity", position="dodge", width = 0.8)+
  geom_errorbar(aes(x=Kit.ID, ymin=load_n-std, ymax=load_n+std), 
                width=0.4, colour="black", alpha=0.9, size=0.8) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 18))+
  theme(axis.text.y   = element_text(size=18, color = "black", family = "sans"),
        axis.text.x   = element_text(size=10, color = "black", family = "sans"),
        axis.title.y  = element_text(size=18, color = "black", family = "sans"),
        axis.title.x  = element_text(size=18, color = "black", family = "sans"),
        legend.text = element_text(size=18, color = "black", family = "sans"),
        legend.title = element_text(size=18, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  labs(x="Sampling ID", y="log10(Bacterial load)", fill=NULL)
```

![plot of chunk PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)](figure/PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)-4.png)

```r
#Find fungal Shannon diversity to compare between Sampling site

df_bac_shannon = df_bac_count_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), shannon = diversity(Count, index = "shannon"))

#Group and aggregate/summarize by Sampling.site for Hutcheson's t-test between Sampling site groups
df_bac_site_agg = df_bac_shannon %>%
  group_by(Sampling.site) %>%
  pivot_wider(names_from = Sampling.site, values_from = shannon, values_fill = 0) %>%
  group_by(Kit.ID) %>%
  summarise(across(c(Front, Back), sum))

#Hutcheson's t-test between Sampling site groups for Shannon diversity indices: two-sided test
Hutcheson_t_test(df_bac_site_agg$Front, df_fungal_site_agg$Back, shannon.base = 10)
```

```
## 
## 	Hutcheson t-test for two communities
## 
## data:  df_bac_site_agg$Front ,  df_fungal_site_agg$Back
## Hutcheson t-statistic = -0.14806, df = 22.927, p-value = 0.8836
## alternative hypothesis: true difference in H' is not equal to 0
## sample estimates:
##        x        y 
## 1.159054 1.175177
```

```r
#Analysis of Variance to compare Shannon diversity between Sampling site groups
sppdiv_aov_bac <- aov(shannon ~ Sampling.site, data = df_bac_shannon)
summary(sppdiv_aov)
```

```
##               Df Sum Sq Mean Sq F value Pr(>F)
## Sampling.site  1  0.097 0.09669   0.833  0.367
## Residuals     36  4.178 0.11605
```

```r
print("The communities are not significantly different based on Shannon diversity indices")
```

```
## [1] "The communities are not significantly different based on Shannon diversity indices"
```

```r
#Plot Shannon diversity box plot for bacteria

df_bac_shannon %>%
  ggplot(mapping = aes(x = Sampling.site, y = shannon, fill = `Sampling.site`)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(colour = "black", linewidth = 0.8) +
  labs(x = "Sampling site", y = "Shannon diversity index") +
  theme(axis.title.y = element_text(size = 14, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(size = 14, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(size = 12, hjust = 0, color = "black")) +
  theme(legend.text = element_text(size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank())
```

![plot of chunk PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)](figure/PCoA plotting based on Bray-Curtis distances grouped based on Age (Sampling.site+Age was too messy)-5.png)

```r
#Transforming data for Bray-Curtis

df_bac_taxa_site_meta = df_bac_taxa_site
rownames(df_bac_taxa_site_meta) = df_bac_taxa_site_meta[,1]
df_bac_taxa_site_meta = df_bac_taxa_site_meta[,-1] #with Sampling site info column
df_bac_taxa_site_bray = df_bac_taxa_site_meta[,-1]
df_bac_taxa_site_bray <- df_bac_taxa_site_bray %>% mutate_if(is.character, as.numeric)

bray_bac <- vegdist(df_bac_taxa_site_bray, method = "bray")
bray_bac_matrix = as.matrix(bray_bac)
```

```r
pcoa.bac <- wcmdscale(bray_bac, eig=TRUE, k=2)

positions.bac <- pcoa.bac$points
positions.bac=as.data.frame(positions.bac)
colnames(positions.bac) <- c("PCoA1", "PCoA2")

positions2.bac=cbind(df_bac_taxa_site_meta[,1], positions.bac)
colnames(positions2.bac)[1] <- "Sampling.site"

percent_explained.bac <- 100 * pcoa.bac$eig / sum(pcoa.bac$eig)

pretty_pe.bac <- format(round(percent_explained.bac, digits =1), nsmall=1, trim=TRUE)

labels <- c(glue("PC1 ({pretty_pe.bac[1]}%)"),
            glue("PC2 ({pretty_pe.bac[2]}%)"))


ggplot(positions2.bac, aes(x = PCoA1, y = PCoA2, color = Sampling.site, shape = Sampling.site)) +
  geom_point(size=5) +
  scale_shape_manual(values=c(15, 17, 19)) +
  # scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
  stat_ellipse(geom = "polygon",
               aes(fill = Sampling.site), 
               alpha = 0, linewidth = 1) +
  xlab(labels[1]) +   #----------get the variance% value from pc_pct_variance[1]*100 for PC1
  ylab(labels[2]) +    #----------get the variance% value from pc_pct_variance[2]*100 for PC2
  #xlim(-0.55,0.45) +    #changing plot X axis limits
  #ylim(-0.5,0.45) +    #changing plot Y axis limits
  theme(axis.text.y   = element_text(size=20, color = "black", family = "sans"),
        axis.text.x   = element_text(size=20, color = "black", family = "sans"),
        axis.title.y  = element_text(size=20, color = "black", family = "sans"),
        axis.title.x  = element_text(size=20, color = "black", family = "sans"),
        legend.text = element_text(size=20, color = "black", family = "sans"),
        legend.title = element_text(size=20, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  coord_fixed(ratio = 0.9)
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-1.png)

```r
#Are groups different based on Sampling.site?: Using Permanova testing

PERMANOVA_Site.bac <- adonis2(bray_bac_matrix ~ Sampling.site, data = df_bac_taxa_site_meta, 
                          permutations = 10000)
print(PERMANOVA_Site.bac)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = bray_bac_matrix ~ Sampling.site, data = df_bac_taxa_site_meta, permutations = 10000)
##               Df SumOfSqs      R2      F Pr(>F)
## Sampling.site  1   0.1319 0.01482 0.5416 0.7769
## Residual      36   8.7661 0.98518              
## Total         37   8.8980 1.00000
```

```r
print("No significant differences in groups based on Sampling Site")
```

```
## [1] "No significant differences in groups based on Sampling Site"
```

```r
#Group and aggregate/summarize by Species and get top n species within each Sampling site group
df_bac_species_top = df_bac_count_longer %>%
  group_by(Sampling.site, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
#Statistical differences groups based on top 10 species: using PERMANOVA from Adonis2

df_bac_taxa_site_top10 = df_bac_taxa_site_bray%>%
  select(df_bac_species_top$Species)

PERMANOVA_Site_top10_bac <- adonis2(df_bac_taxa_site_top10 ~ Sampling.site, data =
                                      df_bac_taxa_site_meta, permutations = 10000)

print(PERMANOVA_Site_top10_bac)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = df_bac_taxa_site_top10 ~ Sampling.site, data = df_bac_taxa_site_meta, permutations = 10000)
##               Df SumOfSqs      R2      F Pr(>F)
## Sampling.site  1   0.1334 0.01506 0.5505 0.7608
## Residual      36   8.7232 0.98494              
## Total         37   8.8565 1.00000
```

```r
print("No significant differences in groups based on Sampling Site by top10 species")
```

```
## [1] "No significant differences in groups based on Sampling Site by top10 species"
```

```r
#Plot top 10 within each Sampling group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

ggplot(data = df_bac_species_top) +
  geom_col(mapping = aes(x = Sampling.site, y = Count, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Sampling site", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  scale_fill_manual(values = col_combined2)
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-2.png)

```r
#facet_wrap(vars(Sampling.site))

#Get Scalp type column in df_taxa_fungal dataframe

df_bac_taxa_scalp <- merge(df_meta_all, df_taxa_bac, by="Kit.ID", all = TRUE)
df_bac_taxa_scalp = na.omit(df_bac_taxa_scalp) #Remove the extra columns not having taxa info (NA rows)

#Make long format OTU/Species dataframe
df_bac_taxa_scalp_longer = df_bac_taxa_scalp %>% 
  pivot_longer(
    cols = colnames(df_bac_taxa_scalp)[9:208],
    names_to = "Species",
    values_to = "Count"
  ) 

#Make Count column numeric
df_bac_taxa_scalp_longer$Count<-as.numeric(df_bac_taxa_scalp_longer$Count)
df_bac_count_scalp_longer = df_bac_taxa_scalp_longer

#Group and aggregate/summarize by Scalp.type+Species and get top n species within each such group
df_bac_species_scalp_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Scalp.type, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Scalp.type'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_scalp_bac = list(df_bac_species_scalp_top, df_bac_species_top)
df_bac_species_scalp_top <- list_df_scalp_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join

##Plot top 10 within each Sampling.site+scalp.type group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_scalp_top) +
  geom_col(mapping = aes(x = Scalp.type, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Scalp type", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", angle = 90, hjust = 1,
                                   size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-3.png)

```r
#Group and aggregate/summarize by Hair.loss+Species and get top n species within each such group
df_bac_species_hair_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Hair.loss, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Hair.loss'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_hair_bac = list(df_bac_species_hair_top, df_bac_species_top)
df_bac_species_hair_top <- list_df_hair_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+hair.loss group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_hair_top) +
  geom_col(mapping = aes(x = Hair.loss, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Hair loss", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", angle = 90, hjust = 1,
                                   size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-4.png)

```r
#Group and aggregate/summarize by Flaking+Species and get top n species within each such group
df_bac_species_flaking_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Flaking, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Flaking'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
list_df_flaking_bac = list(df_bac_species_flaking_top, df_bac_species_top)
df_bac_species_flaking_top <- list_df_flaking_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+Flaking group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_flaking_top) +
  geom_col(mapping = aes(x = Flaking, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Flaking", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-5.png)

```r
#Group and aggregate/summarize by Itchiness+Species and get top n species within each such group
df_bac_species_itchiness_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Itchiness, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Itchiness'. You can override using the `.groups`
## argument.
## Selecting by Count
```

```r
list_df_itchiness_bac = list(df_bac_species_itchiness_top, df_bac_species_top)
df_bac_species_itchiness_top <- list_df_itchiness_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join


##Plot top 10 within each Sampling.site+Itchiness group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_itchiness_top) +
  geom_col(mapping = aes(x = Itchiness, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Itchiness", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-6.png)

```r
##===End of Part 2 Bacterial Analysis===========================

##======Part 3 Bacterial Analysis-----------------

##Shannon diversity
#Find bacterial Shannon diversity to compare between Sampling site

df_bac_shannon2 = df_bac_count_scalp_longer %>%
  group_by(Kit.ID) %>%
  summarize(Sampling.site = first(Sampling.site), Age = first(Age), shannon = diversity(Count, index = "shannon"))

#Group and aggregate/summarize by Sampling.site for Hutcheson's t-test between Sampling site+Age groups
df_bac_site_agg2 = df_bac_shannon2 %>%
  group_by(Sampling.site, Age) %>%
  pivot_wider(names_from = c(Sampling.site, Age), values_from = shannon, values_fill = 0) 

#Hutcheson's t-test between Sampling site groups for Shannon diversity indices: two-sided test
multiple_Hutcheson_t_test(x = df_bac_site_agg2[,2:5], shannon.base = 10)
```

```
## $p.values
##                   Back_30 H = 0.99 Front_40 H = 0.75 Front_30 H = 0.97 Back_40 H = 0.78
## Back_30 H = 0.99                NA           0.04282           0.43969          0.06427
## Front_40 H = 0.75          0.04282                NA           0.07854          0.39064
## Front_30 H = 0.97          0.43969           0.07854                NA          0.11249
## Back_40 H = 0.78           0.06427           0.39064           0.11249               NA
```

```r
#Analysis of Variance to compare Shannon diversity between Sampling site+Age groups
sppdiv_aov2_bac <- aov(shannon ~ Sampling.site * Age, data = df_bac_shannon2)
summary(sppdiv_aov2_bac)
```

```
##                   Df Sum Sq Mean Sq F value Pr(>F)  
## Sampling.site      1  0.018  0.0183   0.080 0.7791  
## Age                1  0.692  0.6924   3.028 0.0909 .
## Sampling.site:Age  1  0.044  0.0445   0.194 0.6620  
## Residuals         34  7.775  0.2287                 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
print("The communities are not significantly different based on Shannon diversity indices")
```

```
## [1] "The communities are not significantly different based on Shannon diversity indices"
```

```r
#Plot Shannon diversity box plot

df_bac_shannon2 %>%
  ggplot(mapping = aes(x = Age, y = shannon, fill = `Sampling.site`)) +
  stat_boxplot(geom = "errorbar", width = 0.2) + 
  geom_boxplot(colour = "black", linewidth = 0.8) +
  labs(x = "Sampling site", y = "Shannon diversity index") +
  theme(axis.title.y = element_text(size = 14, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(size = 14, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(size = 14, color = "black")) +
  theme(axis.text.x = element_text(size = 14, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(size = 12, hjust = 0, color = "black")) +
  theme(legend.text = element_text(size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank())+
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting based on Bray-Curtis distances](figure/PCoA plotting based on Bray-Curtis distances-7.png)

```r
##Bray-Curtis groups by Sampling.site + Age

#Transforming data for Bray-Curtis

df_bac_taxa_scalp_meta = df_bac_taxa_scalp
rownames(df_bac_taxa_scalp_meta) = df_bac_taxa_scalp_meta[,1]
df_bac_taxa_scalp_meta = df_bac_taxa_scalp_meta[,-1] #with Sampling site info column
df_bac_taxa_scalp_meta = df_bac_taxa_scalp_meta[,-c(2:6)]
df_bac_taxa_scalp_meta$Site_age = paste(df_bac_taxa_scalp_meta$Sampling.site, 
                                           df_bac_taxa_scalp_meta$Age, sep="_")
df_bac_taxa_scalp_meta = df_bac_taxa_scalp_meta %>% relocate(Site_age, .after = Sampling.site)

df_bac_taxa_scalp_bray = df_bac_taxa_scalp_meta[,-c(1:3)] #remove meta columns for bray input matrix
df_bac_taxa_scalp_bray <- df_bac_taxa_scalp_bray %>% mutate_if(is.character, as.numeric)

bray_bac2 <- vegdist(df_bac_taxa_scalp_bray, method = "bray")
bray_bac_matrix2 = as.matrix(bray_bac2)
```

```r
pcoa2.bac <- wcmdscale(bray_bac2, eig=TRUE, k=2)

positions2.bac <- pcoa2.bac$points
positions2.bac=as.data.frame(positions2.bac)
colnames(positions2.bac) <- c("PCoA1", "PCoA2")

positions2.2.bac=cbind(df_bac_taxa_scalp_meta[,c(1:3)], positions.bac)
colnames(positions2.2.bac)[1:3] <- c("Age", "Sampling.site","Sampling.site&Age")

percent_explained2.bac <- 100 * pcoa2.bac$eig / sum(pcoa2.bac$eig)

pretty_pe2.bac <- format(round(percent_explained2.bac, digits =1), nsmall=1, trim=TRUE)

labels2 <- c(glue("PC1 ({pretty_pe2.bac[1]}%)"),
             glue("PC2 ({pretty_pe2.bac[2]}%)"))


ggplot(positions2.2.bac, aes(x = PCoA1, y = PCoA2, color = Age, shape = Age)) +
  geom_point(size=5) +
  scale_shape_manual(values=c(15, 16, 17, 18)) +
  # scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))
  stat_ellipse(geom = "polygon",
               aes(fill = Age), 
               alpha = 0, linewidth = 1, linetype = 2) +
  xlab(labels2[1]) +   #----------get the variance% value from pc_pct_variance[1]*100 for PC1
  ylab(labels2[2]) +    #----------get the variance% value from pc_pct_variance[2]*100 for PC2
  #xlim(-0.55,0.45) +    #changing plot X axis limits
  #ylim(-0.5,0.45) +    #changing plot Y axis limits
  theme(axis.text.y   = element_text(size=20, color = "black", family = "sans"),
        axis.text.x   = element_text(size=20, color = "black", family = "sans"),
        axis.title.y  = element_text(size=20, color = "black", family = "sans"),
        axis.title.x  = element_text(size=20, color = "black", family = "sans"),
        legend.text = element_text(size=20, color = "black", family = "sans"),
        legend.title = element_text(size=20, color = "black", family = "sans"),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)
  ) +
  coord_fixed(ratio = 0.9)
```

![plot of chunk PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy](figure/PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy-1.png)

```r
#Are groups different based on Sampling.site and Age?: Using Permanova testing

#Group and aggregate/summarize by Age+Species and get top n species within each such group
#Groups: Front+30, Front+40, Back+30, Back+40
df_bac_species_age_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Age, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Age'. You can override using the `.groups` argument.
## Selecting by Count
```

```r
#Statistical differences groups based on top 10 species: using PERMANOVA from Adonis2

df_bac_taxa_age_top10 = df_bac_taxa_scalp_bray%>%
  select(df_bac_species_age_top$Species)

PERMANOVA_Age_top10_bac <- adonis2(df_bac_taxa_age_top10 ~ Sampling.site*Age, data =
                                 df_bac_taxa_scalp_meta, permutations = 10000)

print(PERMANOVA_Age_top10_bac)
```

```
## Permutation test for adonis under reduced model
## Terms added sequentially (first to last)
## Permutation: free
## Number of permutations: 10000
## 
## adonis2(formula = df_bac_taxa_age_top10 ~ Sampling.site * Age, data = df_bac_taxa_scalp_meta, permutations = 10000)
##                   Df SumOfSqs      R2      F Pr(>F)  
## Sampling.site      1   0.1316 0.01483 0.5638 0.7490  
## Age                1   0.6119 0.06895 2.6207 0.0297 *
## Sampling.site:Age  1   0.1921 0.02165 0.8228 0.5223  
## Residual          34   7.9389 0.89456                
## Total             37   8.8745 1.00000                
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

```r
print("Significant differences in groups based on Age by top10 species, not by Sampling.site")
```

```
## [1] "Significant differences in groups based on Age by top10 species, not by Sampling.site"
```

```r
### Note: I used top 10 species within each of these 4 groups. However, the analysis below, uses
### only the top10 species within each Sampling.site like before and compared across Age as there were
### too many species in the legend. Question was unclear on this.

##################
#Combine (To Use only previously used top10 species by Sampling.site)
list_df_age_bac = list(df_bac_species_age_top, df_bac_species_top)
df_bac_species_age_top <- list_df_age_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join

##Plot top 10 within each Sampling.site+Age group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_age_top) +
  geom_col(mapping = aes(x = Age, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Age", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy](figure/PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy-2.png)

```r
#Compare across pre-menopause groups

#Group and aggregate/summarize by Itchiness+Species and get top n species within each such group
df_bac_species_hormone_top = df_bac_count_scalp_longer %>%
  group_by(Sampling.site, Age, Hormonal.status, Species) %>%
  summarise(across(Count, sum)) %>%
  arrange(Count, .by_group = TRUE) %>%
  top_n(10)
```

```
## `summarise()` has grouped output by 'Sampling.site', 'Age', 'Hormonal.status'. You can override using the
## `.groups` argument.
## Selecting by Count
```

```r
list_df_hormone_bac = list(df_bac_species_hormone_top, df_bac_species_top)
df_bac_species_hormone_top <- list_df_hormone_bac %>% reduce(merge, by=c("Species","Sampling.site"), all = FALSE) #all = FALSE == inner_join

#subset by hormonal status:"None of these" for premenopause
df_bac_species_premenopause_top = subset(df_bac_species_hormone_top, Hormonal.status == 'None of these')

##Plot top 10 within each Sampling.site+Age+premenopausal group
#Define color scales
col_Cory = colorRampPalette(c("green", "darkgreen"))(4)
col_Cut = colorRampPalette(c("blue", "darkblue"))(3)
col_Stap = colorRampPalette(c("red", "darkred"))(4)
col_combined2 = c(col_Cory, col_Cut, "black", col_Stap)

#Plot
ggplot(data = df_bac_species_premenopause_top) +
  geom_col(mapping = aes(x = Age, y = Count.x, fill = Species), position = "fill", width = 0.5) +
  labs(x = "Age", y = "Composition") +
  #scale_fill_viridis_d(option = "viridis", direction = -1) +
  theme(axis.title.y = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(r=12))) +
  theme(axis.title.x = element_text(family = "sans", face = "plain", size = 16, color = "black", margin=margin(t=12))) +
  theme(axis.text.y = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.text.x = element_text(family = "sans", face = "plain", size = 16, color = "black")) +
  theme(axis.ticks.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.ticks.length.y = unit(0.1, "cm")) +
  theme(axis.ticks.length.x = unit(0.1, "cm")) +
  theme(axis.line.y = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(axis.line.x = element_line(linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(legend.title = element_text(family = "sans", face = "plain", size = 14, hjust = 0, color = "black")) +
  theme(legend.text = element_text(family = "sans", face = "plain", size = 12, color="black")) +
  theme(panel.background = element_blank()) +
  theme(panel.border = element_rect(fill = NA, linewidth = 0.7, linetype = "solid", color = "black")) +
  theme(panel.grid.major.y = element_blank()) +
  theme(panel.grid.major.x = element_blank()) +
  theme(strip.background = element_rect(color=NULL, fill="white" )) +
  theme(strip.text = element_text(size = 20, color = "blue")) +
  scale_fill_manual(values = col_combined2) +
  facet_wrap(vars(Sampling.site))
```

![plot of chunk PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy](figure/PCoA plotting by Bray-Curtis distances grouped based on Age (Sampling.site + Age was too messy-3.png)

```r
###End of bacterial analysis##############

#############################################################################################
#############################################################################################

###################### End of analysis ######################################################
```

