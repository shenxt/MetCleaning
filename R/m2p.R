#' @title m2p
#' @description Transform metabolite infromation to pathway information using pathifier.
#' @author Xiaotao Shen
#' \email{shenxt@@sioc.ac.cn}
#' @param data Data (matrix) for transfomration, column correspond to samples and row correspond to metabolite.
#' @param tags Information (matrix) for metabolite. It must contains "Identification" which is the names of metabolites.
#' @param info Information (list) for samples. It must contains "normal" which are the control sample names.
#' @return All the results can be got form other functions and instruction.
#' @export
#' @details #' @details The manual of MetCleaning can be found in \href{https://github.com/jaspershen/MetCleaning/blob/master/vignettes/MetCleaning.pdf}{github}
#' or in \href{https://www.readcube.com/library/fe13374b-5bc9-4c61-9b7f-6a354690947e:d8b5fcff-c725-4689-a97d-7ff106322fb6}{my library}.
#' @seealso \code{\link[pathifier]{pathifier}}

m2p <- function(data = data,
                tags = tags,
                info = NULL) {
  data(c("my.pathway", "hmdb.synonym"))
  identification <- as.character(tags[,"identification"])
  ide.idx <- which(!is.na(identification))
  data <- data[ide.idx,]
  tags <- tags[ide.idx,]
  identification <- as.character(tags[,"identification"])
  identification <- sapply(identification, function(x) {
    temp <- gregexpr(pattern = "_", x)[[1]]
    x <- ifelse(temp < 0, x, substr(x, 1, temp-1))
  })

  ##match HMDB ID
  pathway.ide <- NULL
  index1 <- match(tolower(identification), tolower(hmdb.synonym[,2]))
  id <- as.character(hmdb.synonym[index1, 1])
  met <- as.character(hmdb.synonym[index1, 2])
  ##Fuzzy Matching
  if (sum(is.na(index1)) != 0) {
    idx <- which(is.na(id))
    match.index <- sapply(identification[idx], function(x)
      {index <- agrep(x, as.character(hmdb.synonym[,2]))
      ifelse(length(index) > 50, index <- index[1:50], index <- index)
      index})

    match.met <- lapply(match.index,function(x) {as.character(hmdb.synonym[x,2])})
    ncol.num <- max(max(unlist(lapply(match.met, length))),1)
    output <- matrix(NA, nrow = length(match.met), ncol = ncol.num+4)
    colnames(output) <- c("Index", "name","new name" ,"which is right", paste("Matched",c(1:ncol.num)))
    for (i in 1:(nrow(output))) {
      output[i,5:(ncol(output))] <-
        c(as.character(match.met[[i]]), rep(NA, ncol.num - length(match.met[[i]])))
    }
    output[,1] <- idx
    output[,2] <- identification[idx]
    output[,3] <- identification[idx]
    output[,4] <- rep(NA, nrow(output))

    ##first edit
    output <- edit(output)
  }

  which.is.right <- as.numeric(output[,4])
  matched.idx <- which(!is.na(which.is.right))
  output.old <- output[matched.idx,]
  output.new <- output[-matched.idx,]


  rematch <- readline("Do you want to rematch? yes(yes) or no(no)")
    while(rematch == "yes") {
    new.name <- as.character(output.new[,"new name"])
    ##begnin match
    match.index <- lapply(new.name, function(x)
    {index <- agrep(x, as.character(hmdb.synonym[,2]))
    ifelse(length(index) > 50, index <- index[1:50], index <- index)
    index})
    match.met <- lapply(match.index,function(x) {as.character(hmdb.synonym[x,2])})
    ncol.num <- max(max(unlist(lapply(match.met, length))),1)

    output.new1 <- matrix(NA, nrow = length(match.met), ncol = ncol.num+4)
    for (i in 1:(nrow(output.new1))) {
      output.new1[i,5:(ncol(output.new1))] <-
        c(as.character(match.met[[i]]), rep(NA, ncol.num - length(match.met[[i]])))
    }

    output.new1[,1] <- output.new[,1]
    output.new1[,2] <- output.new[,2]
    output.new1[,3] <- output.new[,3]
    output.new1[,4] <- output.new[,4]

    colnames(output.new1) <-
      c("Index", "name","new name" ,"which is right", paste("Matched",c(1:ncol.num)))

    output.new1 <- edit(output.new1)
    which.is.right <- as.numeric(output.new1[,4])
    matched.idx <- which(!is.na(which.is.right))

    temp <- output.new1[matched.idx,,drop = F]
    ncol1 <- ncol(temp)
    ncol2 <- ncol(output.old)
    if (ncol1 == ncol2) {output.old <- rbind(output.old, temp)}
    if (ncol1 > ncol2) {
      output.old <-
        cbind(output.old, matrix(NA, ncol = ncol1 - ncol2, nrow = nrow(output.old)))
        colnames(output.old) <- colnames(temp)
        output.old <- rbind(output.old, temp)
    }
    if (ncol1 < ncol2) {
      temp <- cbind(temp, matrix(NA, ncol = ncol2 - ncol1, nrow = nrow(temp)))
      colnames(temp) <- colnames(output.old)
      output.old <- rbind(output.old, temp)
    }

    output.new <- output.new1[-matched.idx,,drop = FALSE]
    rematch <- readline("Do you want to rematch? yes(yes) or no(no)")

    if (rematch == "no") {
      ##go on?
      new.name <- output.new[,3]
      which.is.right <- as.numeric(output.new[,4])
      no.idx <- which(is.na(which.is.right))
      if (length(no.idx) != 0) {
        cat(new.name[no.idx])
        cat("\n")
        rematch <- readline("are not matched and will be removed form dataset, do you want to rematch? yes(yes) or no(no)")
    }
    }
    }

  ##combine old and new
  ncol1 <- ncol(output.new)
  ncol2 <- ncol(output.old)
  if (ncol1 == ncol2) {output.old <- rbind(output.old, output.new)}
  if (ncol1 > ncol2) {
    output.old <-
      cbind(output.old, matrix(NA, ncol = ncol1 - ncol2, nrow = nrow(output.old)))
    colnames(output.old) <- colnames(output.new)
    output.old <- rbind(output.old, output.new)
  }
  if (ncol1 < ncol2) {
    temp <- cbind(temp, matrix(NA, ncol = ncol2 - ncol1, nrow = nrow(output.new)))
    colnames(output.new) <- colnames(output.old)
    output.old <- rbind(output.old, output.new)
  }

    ##matched end
    output.old <- output.old[order(as.numeric(output.old[,1])),]
    which.is.right <- as.numeric(output.old[,4])
    remain.idx <- which(!is.na(which.is.right))

    new.data <- output.old[remain.idx,]
    which.is.right <- as.numeric(new.data[,4])

    matched <- NULL
    for (i in 1:nrow(new.data)) {
      matched[i] <-new.data[i,which.is.right[i] + 4]
    }

    new.data <- cbind(new.data[,c(1:3)], matched)

    matched.id <- as.character(hmdb.synonym[,1][match(matched, hmdb.synonym[,2])])
    idx.new <- as.numeric(new.data[,1])
    id[idx.new] <- matched.id
    met[idx.new] <- matched
    ##

    tags <- cbind(met, id, tags)
   colnames(tags)[1:2] <- c("matched name", "HMDB ID")
   new.data <- data[which(!is.na(id)),]
   new.tags <- tags[which(!is.na(id)),]

   ###pathifier
   #construct sheffer
   data.pds <- list()
   data.pds$data <- new.data
   data.pds$allgenes <- as.character(new.tags[,"HMDB ID"])
   data.pds$samples <- as.list(colnames(new.data))
   normal <- rep(FALSE, ncol(new.data))
   normal[match(info[["normal"]], colnames(new.data))] <- TRUE
   data.pds$normal <- normal

   #construct kegg
   my.pathway$gs <- lapply(my.pathway$gs, as.matrix)

   PDS <- pathifier::quantify_pathways_deregulation(data.pds$data,
                                                    data.pds$allgenes,
                                                    my.pathway$gs,
                                                    my.pathway$pathwaynames,
                                                    data.pds$normals,
                                                    attempts = 100,
                                                    logfile = "data.my.pathway.log",
                                                    min_exp = 4,
                                                    min_std = 0.2254005)
save(PDS, file = "PDS")
score <- PDS$scores
score <- matrix(unlist(score), ncol = length(score[[1]]), byrow = TRUE)
colnames(score) <- colnames(new.data)
row.names(score) <- names(PDS$genesinpathway)
write.csv(score, "score.csv", row.names = F)
}
