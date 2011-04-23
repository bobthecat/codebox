#
# hapmap.LDplot.r
#
# Created by David Ruau on 2011-04-22.  
# Copyright (c) 2011 Department of Pediatrics/Cancer Biology Stanford University. All rights reserved.
# 
#
##################### USAGE #########################
# 
# hapmap.LDplot("POU5F1")
# output: the LD heatmap is in POU5F1.pdf in the working directory
# 
#####################################################

hapmap.LDplot <- function(gene.name=NULL, chr=NULL, 
  start=NULL, stop=NULL, method=c("r2", "Dprime"),
  mysnp=NULL) {
    require(NCBI2R)
    require(annotate)
    require(org.Hs.eg.db)
    require(LDheatmap)
    require(RColorBrewer)
    require(foreach)
    require(doMC)
    require(multicore)
    ncore = multicore:::detectCores()
    registerDoMC(cores = ncore)
    
    if(length(method)>1){
      method="r2"
      cat("LD measure not provided: set to R^2'\n")
    }
    
    if(!is.null(gene.name)){
      # grab gene region
      geneID <- as.vector(unlist(mget(gene.name, env=revmap(org.Hs.egSYMBOL))))
      # this is NCBI gene info not HapMap info.
      # coordinates can be different.
      x <- GetGeneInfo(geneID)
    }
    else{
      if(is.null(chr) | is.null(start) | is.null(stop)){
        stop("If no gene name is define then chr, start and stop are needed!")
      } else{
        x <- list(chr=chr, GeneLowPoint=start, GeneHighPoint=stop)
      }
    }
    # grab LD info from HapMap
    f <- GetLDInfo(x$chr, x$GeneLowPoint, x$GeneHighPoint)

    # No LDdata available
    if(is.null(dim(f))){
      cat("No LD data available\n")
      return(f)
    }
    
    # make LD matrix of R^2
    u.snp <- unique(f$SNPA)

    # label the supplied SNP with stars
    if(!is.null(mysnp)){
      u.snp <- sapply(u.snp, function(x){ifelse(x %in% mysnp, paste(x,"*", sep=""), x)})
    }
    
    # chromosome position
    gen.dist <- unique(f$chrpos1)

    cat("making square LD matrix\n")
    t <- foreach(i = 1:length(u.snp), .combine='rbind') %dopar% {
      # select submatrix from f for SNP i
      tt <- subset(f, SNPA==u.snp[i])
      myrow <- rep(NA, length(u.snp))
      for(j in 1:length(u.snp)){
        if(u.snp[j] == u.snp[i]){
          myrow[j] <- 0
        }
        else{
          value <- tt[which(tt$SNPB == u.snp[j]), method]
          if(length(value) > 0){
            myrow[j] <- value
          }
        }
      }
      myrow
    }
    
    # needed for the label in the heatmap
    rownames(t) <- u.snp
    colnames(t) <- u.snp
    mycol <- c(brewer.pal(5,"Reds")[5:1], "#F9F9F9")
    # plot the LD heatmap with the R^2    
    # c(bottom, left, top, right) c(5, 4, 4, 2) + 0.1
    # par(mar=c(15,4,4,8))
    pdf(file=paste(gene.name, ".pdf"), height=7, width=7)
    MyHeatmap <- LDheatmap2(t, genetic.distances=gen.dist, 
      SNP.name=u.snp, color=mycol, 
      geneMapLocation=0.1, rs.cex=0.3)
    dev.off()
}

LDheatmap2 <- function (gdat, genetic.distances = NULL, distances = "physical", 
    LDmeasure = "r", title = "Pairwise LD", add.map = TRUE, add.key = TRUE, 
    geneMapLocation = 0.15, geneMapLabelX = 0.5, geneMapLabelY = 0.3, 
    SNP.name = NULL, color = grey.colors(20), newpage = TRUE, 
    name = "ldheatmap", vp.name = NULL, pop = FALSE, rs.cex=0.4) {
    makeImageRect <- function(nrow, ncol, cols, name) {
        xx <- (1:ncol)/ncol
        yy <- (1:nrow)/nrow
        right <- rep(xx, nrow)
        top <- rep(yy, each = ncol)
        rectGrob(x = right, y = top, width = 1/ncol, height = 1/nrow, 
            just = c("right", "top"), gp = gpar(col = NA, fill = cols), 
            name = name)
    }
    LDheatmap.Legend.add <- function(color, vp = heatmapVP) {
        ImageRect <- makeImageRect(2, length(color), col = c(rep(NA, 
            length(color)), color[length(color):1]), "colorKey")
        keyVP <- viewport(x = 1.1, y = -0.1, height = 0.1, width = 0.5, 
            just = c("right", "bottom"))
        title <- textGrob("Color Key", x = 0.5, y = 1.25, name = "title", 
            gp = gpar(cex = 0.8))
        labels <- textGrob(paste(0.2 * 0:5), x = 0.2 * 0:5, y = 0.25, 
            gp = gpar(cex = 0.6), name = "labels")
        ticks <- segmentsGrob(x0 = c(0:5) * 0.2, y0 = rep(0.4, 
            6), x1 = c(0:5) * 0.2, y1 = rep(0.5, 6), name = "ticks")
        box <- linesGrob(x = c(0, 0, 1, 1, 0), y = c(0.5, 1, 
            1, 0.5, 0.5), name = "box")
        key <- gTree(children = gList(ImageRect, title, labels, 
            ticks, box), name = "Key", vp = vpTree(vp, vpList(keyVP)))
        key
    }
    LDheatmap.Map.add <- function(nsnps, add.map, genetic.distances, geneMapLocation = 0.15, geneMapLabelX = 0.65, geneMapLabelY = 0.3, distances = "physical", vp = heatmapVP, SNP.name = NULL, ind = 0) {
        snp <- ((1:nsnps - 1) + 0.5)/nsnps
        if (add.map) {
            min.dist <- min(genetic.distances)
            max.dist <- max(genetic.distances)
            total.dist <- max.dist - min.dist
            seq.x <- c(0.5 * geneMapLocation + 1/(nsnps * 2), 
                1 + 0.5 * geneMapLocation - 1/(nsnps * 2))
            seq.y <- c(-0.5 * geneMapLocation + 1/(nsnps * 2), 
                1 - 0.5 * geneMapLocation - 1/(nsnps * 2))
            diagonal <- linesGrob(seq.x, seq.y, gp = gpar(lty = 1), 
                name = "diagonal")
            regionx <- seq.x[1] + ((genetic.distances - min.dist)/total.dist) * 
                (seq.x[2] - seq.x[1])
            regiony <- seq.y[1] + ((genetic.distances - min.dist)/total.dist) * 
                (seq.y[2] - seq.y[1])
            segments <- segmentsGrob(snp, snp, regionx, regiony, 
                name = "segments")
            if (distances == "physical") 
                mapLabel <- paste("Physical Length:", round((total.dist/1000), 
                  1), "kb", sep = "")
            else mapLabel <- paste("Genetic Map Length:", round(total.dist, 
                1), "cM", sep = "")
            ## Physical distance. useful but plotted int he middle...
            # title <- textGrob(mapLabel, geneMapLabelX, geneMapLabelY, 
                # gp = gpar(cex = 1), just = "left", name = "title")
            geneMap <- gTree(children = gList(diagonal, segments, 
                title), name = "geneMap", vp = vp)
            if (!is.null(SNP.name) && (any(ind != 0))) {
                symbols <- pointsGrob(snp[ind], snp[ind], pch = "", 
                  gp = gpar(cex = 1.25, bg = "blue", col = "blue"), 
                  name = "symbols")
                SNPnames <- textGrob(paste(" ", SNP.name), just = "left", 
                  rot = -45, regionx[ind], regiony[ind], gp = gpar(cex = rs.cex, 
                    col = "blue"), name = "SNPnames")
                geneMap <- gTree(children = gList(diagonal, segments, 
                  title, symbols, SNPnames), name = "geneMap", 
                  vp = vp)
            }
        }
        else if (!add.map && !is.null(SNP.name) && (any(ind != 
            0))) {
            geneMap <- textGrob(paste(" ", SNP.name), just = "left", 
                rot = -45, snp[ind], snp[ind], gp = gpar(cex = 0.6, 
                  col = "blue"), name = "SNPnames", vp = vp)
        }
        else geneMap <- NULL
        geneMap
    }
    if (is.null(genetic.distances)) {
        if (inherits(gdat, "data.frame")) 
            genetic.distances = 1000 * (1:ncol(gdat))
        else if (inherits(gdat, "matrix")) 
            genetic.distances = 1000 * (1:length(gdat[1, ]))
        else genetic.distances = gdat$genetic.distances
    }
    if (inherits(gdat, "data.frame")) {
        for (i in 1:ncol(gdat)) {
            if (!genetics::is.genotype(gdat[, i])) 
                stop("column ", i, " is not a genotype object\n")
        }
        gvars <- unlist(sapply(gdat, function(x) genetics::nallele(x) == 
            2))
        genetic.distances <- genetic.distances[gvars]
        gdat <- gdat[gvars]
        if (!is.vector(genetic.distances)) {
            stop("Distance should be in the form of a vector")
        }
        o <- order(genetic.distances)
        genetic.distances <- genetic.distances[o]
        gdat <- gdat[, o]
        myLD <- genetics::LD(gdat)
        if (LDmeasure == "r") 
            LDmatrix <- myLD[[LDmeasure]]^2
        else if (LDmeasure == "D'") 
            LDmatrix <- abs(myLD[[LDmeasure]])
        else stop("Invalid LD measurement, choose r or D'.")
    }
    else if (inherits(gdat, "LDheatmap")) {
        LDmatrix <- gdat$LDmatrix
        distances <- gdat$distances
    }
    else if (inherits(gdat, "matrix")) {
        if (nrow(gdat) != ncol(gdat)) 
            stop("The matrix of linkage disequilibrium measurements must be a square matrix")
        LDmatrix <- gdat
        LDmatrix[lower.tri(LDmatrix, diag = TRUE)] <- NA
    }
    else if (!missing(gdat)) 
        stop(paste("No method for an object of class", class(gdat)))
    else stop("Need to supply LD matrix or genotypes")
    heatmapVP <- viewport(width = unit(0.8, "snpc"), height = unit(0.8, 
        "snpc"), name = vp.name)
    if (color[1] == "blueToRed") 
        color = rainbow(20, start = 4/6, end = 0, s = 0.7)[20:1]
    if (newpage) 
        grid.newpage()
    mybreak <- 0:length(color)/length(color)
    colcut <- as.character(cut(1 - LDmatrix, mybreak, labels = as.character(color), 
        include.lowest = TRUE))
    if (is.numeric(color)) 
        colcut <- as.integer(colcut)
    ImageRect <- makeImageRect(dim(LDmatrix)[1], dim(LDmatrix)[2], 
        colcut, name = "heatmap")
    title <- textGrob(title, 0.5, 1.05, gp = gpar(cex = 1), name = "title")
    heatMap <- gTree(children = gList(ImageRect, title), name = "heatMap", 
        vp = heatmapVP)
    nsnps <- ncol(LDmatrix)
    step <- 1/(nsnps - 1)
    ind <- match(SNP.name, names(LDmatrix[, 1]), nomatch = 0)
    geneMap <- LDheatmap.Map.add(nsnps, genetic.distances = genetic.distances, 
        geneMapLocation = geneMapLocation, add.map, geneMapLabelX = geneMapLabelX, 
        geneMapLabelY = geneMapLabelY, distances = distances, 
        vp = heatmapVP, SNP.name = SNP.name, ind = ind)
    if (add.key) 
        Key <- LDheatmap.Legend.add(color, vp = heatmapVP)
    else Key <- NULL
    LDheatmapGrob <- gTree(children = gList(heatMap, geneMap, 
        Key), childrenvp = heatmapVP, name = name, cl = "ldheatmap")
    grid.draw(LDheatmapGrob)
    if (pop) {
        downViewport(heatmapVP$name)
        popViewport()
    }
    ldheatmap <- list(LDmatrix = LDmatrix, LDheatmapGrob = LDheatmapGrob, 
        heatmapVP = heatmapVP, genetic.distances = genetic.distances, 
        distances = distances)
    class(ldheatmap) <- "LDheatmap"
    invisible(ldheatmap)
}

