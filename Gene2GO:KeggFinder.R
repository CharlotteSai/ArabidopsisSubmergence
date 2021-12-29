geneIDs <- setdiff(DGEs$gad1245_special$GeneID,c(DGEs$gad21$GeneID,DGEs$gad1245$GeneID))


lapply(intersect(geneIDs, names(gene2GO)), function(x){
  listGenes <- filter(go, GeneID == x)
  tibble(GO = gene2GO[[x]],
         Description = annotate::Term(GO),
         gene = x,
         origin = ifelse(GO %in% listGenes$GO_terms, "direct", "linked")
         ) %>% 
    arrange(origin) %>% 
    filter(origin == "direct") %>% as.data.frame()
})

lapply(intersect(geneIDs, names(gene2pathway)), function(x){
  tibble(keggID = gene2pathway[[x]],
         gene = x) %>% 
    left_join(keggSummary, by = "keggID") %>% as.data.frame()
})

