MakeDecisionTree <- function(n.breaks = 5) {
    data <- LoadData(binarize = FALSE);
    data$Class <- cut2(data$Age, g = n.breaks);
    ## str(data);
    fmla <- as.formula(c("Class ~ ", paste0(kAttributeNames[-9], collapse = " + ")));
    ## print(fmla);
    tree <- NULL;
    tree <- rpart(formula = fmla,
                  data = data, method = "class",
                  ## control = rpart.control(
### minsplit - the minimum number of observations that must exist in a
### node in order for a split to be attempted.
                      ## minsplit = n.breaks,
### minbucket - the minimum number of observations in any terminal
### ‘<leaf>’ node.
                      ## minbucket = 1,
### cp - complexity parameter.  Any split that does not decrease the
### overall lack of fit by a factor of ‘cp’ is not attempted.
                      ## cp = 0.01,
### "information", "gini"
                  ##     parms = list(split = "information")
                  ## )
                  );
    if (!is.null(tree)) {
        return(tree);
    } else {
        warning(">> returning NULL!");
        return(NULL);
    }
}  ## End of MakeDecisionTree



#################### Code snipplets
if (FALSE) {
                                        # grow tree
    tree <- rpart(Kyphosis ~ Age + Number,
                  method = "class", data = kyphosis);
    plot(tree);
    text(tree);
    
    ## prp(tree, varlen = 10);
    
                                        # Interatively prune the tree
    ## new.tree.1 <- prp(tree, snip = TRUE)$obj # interactively trim the tree
    ## prp(new.tree.1) # display the new tree
    
    pdf(file = paste0(project.path, "/output/test_plot.pdf"), width = 12, height = 9, bg = "white");
    fancyRpartPlot(model = tree, main = "Main Title", sub = "Subtitle for the plot",
                   cex = 1, cex.main = 2);
    dev.off();
    
    printcp(fit) # display the results
    plotcp(fit) # visualize cross-validation results
    summary(fit) # detailed summary of splits
    
                                        # plot tree
    plot(fit, uniform = TRUE,
         main = "Classification Tree for Kyphosis");
    text(fit, use.n = TRUE, all = TRUE, cex = .8);
    
    plot()
                                        # create attractive postscript plot of tree
    ## post(fit, file = "tree.ps",
    ##      title = "Classification Tree for Kyphosis")
    ## pdf(file = "~/output/test.pdf", width = 12, height = 9, bg = "white");
}
