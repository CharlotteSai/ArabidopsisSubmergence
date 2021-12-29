### This quick gene expression check is based on the DGE list ###
library(biomaRt)
library(tidyverse)
library(magrittr)
# To choose BioMart database and construct information needed
listMarts(host="plants.ensembl.org")
m <- useMart("plants_mart", dataset="athaliana_eg_gene", host="plants.ensembl.org")
# listDatasets(m) # check plants info version 
# attributes <- listAttributes(m) # Choose data types you want to download

genesPos <- getBM(attributes=c("ensembl_gene_id","external_gene_name",
                               "chromosome_name","start_position",
                               "end_position"), 
                  mart=m) %>% 
  set_colnames(c("GeneID", "GeneSymble","Chr",
                 "GeneStart","GeneEnd")) 

ca_signal <- readxl::read_excel("Ca2+ signalling components_for Charlotte_20210427.xlsx", 
                        sheet = "string_ver.", col_names = FALSE)
ca_signal <-  ca_signal[[1]] %>% 
  str_c("^",.) %>%
  str_c(., collapse = "|")
  
caGenes <- genesPos %>% 
  filter(str_detect(genesPos$GeneSymble, regex(ca_signal, ignore_case = TRUE))) %>% 
  arrange(GeneSymble)

genes <- genesPos %>% 
  filter(str_detect(GeneSymble, "GAD|ALMT")) %>% # GAD|MPK12|ALMT|PRXIIC|PRXIIB
  arrange(GeneSymble) %>% pull(GeneID)

genesPos %>%
  filter(GeneID %in% c("AT5G39410", "AT5G60100"))

genesPos %>%
  filter(GeneID %in% c("AT1G67105", "AT1G69900",
                       "AT3G15310", "AT4G26150"))

# ExpData <- countList %>% # need also check the gene expression right after gad2
#   cpm(log = TRUE) %>%

ExpData <- countList %>% 
  cpm() %>% 
  as.data.frame()

ExpData <- log2(ExpData+1) %>% 
  rownames_to_column("GeneID") %>% 
  left_join(genesPos, by = "GeneID") %>% 
  dplyr::select(-Chr, -GeneStart, -GeneEnd) %>% 
  mutate(GeneSymble = if_else(is.na(GeneSymble),
                              "",GeneSymble)) %>% 
  reshape2::melt(id.vars = c("GeneID", "GeneSymble"),
                 variable.name = "Sample",
                 value.name = "Counts") %>% 
  na.omit() %>% 
  mutate(Genotype = str_extract(Sample,".+(?=-(cont|sub))"),
         GeneSymble = paste(GeneID,GeneSymble)) 

genes <-c("AT5G39410", "AT5G60100")
genes <- c("AT1G67105", "AT1G69900",
           "AT3G15310", "AT4G26150")
genes <- intersect(DGEs$gad1245$GeneID,
                   DGEs$gad1245_gad2$GeneID) %>% 
  setdiff(c("AT5G39410", "AT5G60100")) #11

genes <- setdiff(DGEs$gad1245$GeneID,
                   DGEs$gad21$GeneID) %>% 
  setdiff(c("AT5G39410", "AT5G60100")) # 489

ExpData %>% 
  filter(GeneID %in% genes# & Genotype != "gad1SALK") %>% 
  ) %>%
  # mutate(GeneSymble = factor(GeneSymble,
  #                            levels = c("AT5G17330 GAD1",
  #                                       "AT1G65960 GAD2",
  #                                       "AT2G02010 GAD4"))) %>% 
  mutate(Genotype = factor(Genotype,
                           levels = c("Col", "gad1SALK",
                                      "gad21","gad1245"))) %>% 
  ggplot(aes(y = Counts, # gad2 mutate have absolutely no reads for mpk12
             x = Genotype)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center',
               position = position_dodge(1)) +
  theme_bw() +
  theme(text = element_text(size=16)) +
  labs(x = "",
       y = "log2(cpm+1)") +
  facet_wrap("GeneSymble", scales = "free_y") #-> geneExp

# export::graph2office(x = geneExp, type = "ppt",
#              file = "geneExp.pptx", width = 8,
#              aspectr = 1.5, append = TRUE)
   
