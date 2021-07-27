### This quick gene expression check is based on the DGE list ###
library(biomaRt)
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
  filter(str_detect(GeneSymble, "GAD|MPK12|ALMT|PRXIIC|PRXIIB")) %>% 
  arrange(GeneSymble)

countList %>% # need also check the gene expression right after gad2
  cpm(
    log = TRUE
    ) %>%
  as.data.frame() %>% 
  rownames_to_column("GeneID") %>% 
  right_join(genes, by = "GeneID") %>% 
  dplyr::select(-Chr, -GeneStart, -GeneEnd) %>% 
  reshape2::melt(id.vars = c("GeneID", "GeneSymble"),
                 variable.name = "Sample",
                 value.name = "Counts") %>% 
  na.omit() %>% 
  mutate(Genotype = str_extract(Sample,".+(?=-(cont|sub))")) %>% 
  ggplot(aes(y = Counts, # gad2 mutate have absolutely no reads for mpk12
             x = Genotype)) +
  geom_boxplot() +
  geom_dotplot(binaxis = 'y', 
               stackdir = 'center',
               position = position_dodge(1)) +
  facet_wrap("GeneSymble", scales = "free")
   
