# Data set ----

library(SummarizedExperiment)

# see vignette for suggestions on generating
# count tables from RNA-Seq data
set.seed(1)
cnts <- matrix(rnbinom(n=1000, mu=100, size=1/0.5), ncol=10)
storage.mode(cnts) <- "integer"
rownames(cnts) <- paste0("gene", seq_len(nrow(cnts)))
cond <- factor(rep(1:2, each=5))

se <- SummarizedExperiment(assays = list(counts = cnts), colData = DataFrame(cond))
rm(cnts, cond)

# DESeq2 ---

library(DESeq2)

dds <- DESeqDataSet(se, ~ cond)
dds <- DESeq(dds)
res_deseq2 <- results(dds)

rm(dds)

# edgeR ----

library(edgeR)

counts <- assay(se, "counts")
design <- model.matrix(~ 0 + cond, data = colData(se))

fit <- glmFit(counts, design, dispersion=0.1)
lrt <- glmLRT(fit, contrast = c(-1, 1))
res_edger <- topTags(lrt, n = Inf)

rm(counts, design, fit, lrt)

# limma ----

counts <- assay(se, "counts")
design <- model.matrix(~ 0 + cond, data = colData(se))

keep <- filterByExpr(counts, design)
v <- voom(counts[keep,], design, plot=FALSE)
fit <- lmFit(v, design)
contr <- makeContrasts("cond2 - cond1", levels = colnames(coef(fit)))
tmp <- contrasts.fit(fit, contr)
tmp <- eBayes(tmp)
res_limma <- topTable(tmp, sort.by = "P", n = Inf)

rm(counts, design, keep, v, fit, contr, tmp)
