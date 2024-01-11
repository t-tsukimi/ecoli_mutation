rm(list = ls())
library(tidyverse)

# input merged mutation rate and mutation prevalence data -----------------
dt_merge <- read.csv("merged_mutation_rate_tidy.csv")
prevalence <- read.csv("mutation_prevalence.csv")
prevalenceVec <- c()
prevalenceGenes <- prevalence$MutationGene[prevalence$GeneCountSum >= 5]



# ###Extract mutations common to each isolator#### ------------------------
library(stringr)

extract_common_mutation_genes <- function(isolator_data_list) {
        unique_isolators <- names(isolator_data_list)
        
        result <- data.frame(isolator = unique_isolators, stringsAsFactors = FALSE)
        result$CommonMutationGenes <- vector("list", length(unique_isolators))
        
        for (isolator_idx in seq_along(unique_isolators)) {
                isolator <- unique_isolators[isolator_idx]
                isolator_data <- isolator_data_list[[isolator]]
                isolator_rep_data_list <- split(isolator_data, isolator_data$isolator_rep)
                common_mutation_genes_list <- lapply(isolator_rep_data_list, function(x) x$MutationGene)
                # Calculate the number of occurrences of MutationGene
                mutation_gene_counts <- table(unlist(common_mutation_genes_list))
                # Extract MutationGene common to more than 60% of replicates in a isolator
                common_mutation_genes <- names(mutation_gene_counts[mutation_gene_counts >= length(isolator_rep_data_list) * 0.6])
                # Sort CommonMutationGenes alphabetically
                common_mutation_genes <- sort(common_mutation_genes)
                result$CommonMutationGenes[[isolator_idx]] <- common_mutation_genes
        }
        return(result)
}

dt.filter <- filter(dt_merge, NewImpactVec == "HIGHMODERATE", 
                    MutationRate >= 20, Day == 84)
isolator_data_list <- split(dt.filter, dt.filter$isolator)

result <- extract_common_mutation_genes(isolator_data_list)

names(result$CommonMutationGenes) <- result$isolator
result$CommonMutationGenes <- sapply(result$CommonMutationGenes, function(x) paste(x, collapse = ", "))

isolatorCommonGenes <- list(
        "WT_CMF_1st" = strsplit(result$CommonMutationGenes[5], ", ")[[1]],
        "WT_CMF_2nd" = strsplit(result$CommonMutationGenes[6], ", ")[[1]],
        "WT_CMF_mutS" = strsplit(result$CommonMutationGenes[7], ", ")[[1]]
)


# Confirmation of common mutant genes -------------------------------------
intersect(isolatorCommonGenes$WT_CMF_1st, isolatorCommonGenes$WT_CMF_mutS)
intersect(isolatorCommonGenes$WT_CMF_2nd, isolatorCommonGenes$WT_CMF_mutS)
intersect(isolatorCommonGenes$WT_CMF_1st, isolatorCommonGenes$WT_CMF_2nd)

intersect(isolatorCommonGenes$WT_CMF_2nd, 
          intersect(isolatorCommonGenes$WT_CMF_1st, isolatorCommonGenes$WT_CMF_mutS))


# Draw a Venn diagram of 3 sets -------------------------------------------
vennData <- list(mutL_CMF1 = isolatorCommonGenes$WT_CMF_1st, 
                 mutL_CMF2 = isolatorCommonGenes$WT_CMF_2nd,
                 mutS_CMF = isolatorCommonGenes$WT_CMF_mutS)
pdf("venn_Diagram.pdf")
ggvenn(vennData, fill_alpha = 0, text_size = 6, set_name_size = 8)
dev.off()

