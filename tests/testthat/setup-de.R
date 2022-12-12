# Data set ----

library(SummarizedExperiment)

# see vignette for suggestions on generating
# count tables from RNA-Seq data
set.seed(1)
cnts <- matrix(rnbinom(n = 1000, mu = 100, size = 1 / 0.5), ncol = 10)
storage.mode(cnts) <- "integer"
rownames(cnts) <- paste0("gene", seq_len(nrow(cnts)))
cond <- factor(rep(1:2, each = 5))

se <- SummarizedExperiment(
    assays = list(counts = cnts),
    colData = DataFrame(cond)
)
rm(cnts, cond)

# DESeq2 ---

library(DESeq2)

dds <- DESeqDataSet(se, ~cond)
dds <- DESeq(dds, fitType = "local")
res_deseq2 <- results(dds)

rm(dds)

# edgeR ----

library(edgeR)

design <- model.matrix(~ 0 + cond, data = colData(se))

fit <- glmFit(se, design, dispersion = 0.1)
lrt <- glmLRT(fit, contrast = c(-1, 1))
res_edger <- topTags(lrt, n = Inf)

rm(design, fit, lrt)

# limma ----

design <- model.matrix(~ 0 + cond, data = colData(se))

keep <- filterByExpr(se, design)
fit <- voomLmFit(se[keep, ], design, plot = FALSE)
contr <- makeContrasts("cond2 - cond1", levels = design)
fit <- contrasts.fit(fit, contr)
fit <- eBayes(fit)
res_limma <- topTable(fit, sort.by = "P", n = Inf)

rm(design, keep, fit, contr)
