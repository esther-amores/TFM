library(dplyr)
library(sjlabelled)
library(sjmisc)
library(sjstats)
library(sjPlot)
library(summarytools) # sudo apt-get install libmagick++-dev
library(ggplot2)
library(ggthemes)

tab_corr(train[, -1],                  # the dataset
         na.deletion = c("pairwise"),  # pairwise deletion keeps more data than listwise
         corr.method = c("pearson"),   # pearsons r is the standard way to calculate correlation coefficients
         title = NULL,                 # You can put a title for your table here
         var.labels = NULL,            # This is for variable lables if they aren't in your data already
         wrap.labels = 100,            # stops long variable labels wrapping
         show.p = TRUE,                # show's p-values as stars (*) or (**) or (***)
         p.numeric = FALSE,            # show's p-values as numbers
         fade.ns = TRUE,               # non-significant results appear in light grey
         val.rm = NULL,                # 
         digits = 2,                   # number of digits to round correlation coefficient
         triangle = "both",            # Above and below the diagonal in a matrix are the same. Do you want both?
         string.diag = NULL,           # 
         CSS = NULL,                   # This is for putting css code to make tables pretty
         encoding = NULL,              #
         file = NULL,                  # 
         use.viewer = TRUE,            # Sends output to browser
         remove.spaces = TRUE)         #


sjp.corr(train[, -1],          # the dataset
         decimals = 1,         # sets number of decimals for correlations
         wrap.labels =100,     # stops variable labels wrapping 
         show.p=TRUE,          # shows stars for p-values      
         show.values=TRUE,     # shows correlation coefficients in figure 
         show.legend = TRUE,   # shows the ledgent on the right hand side of figure
         sort.corr = TRUE,     # sorts the rows and columns by correlations 
         # - this makes it easier to see patterns
         geom.colors = "RdBu",       # colour scale for cells. This is Red-Blue.
         corr.method = "pearson",    # calculates pearson correlation. Default is spearman correlation.
         na.deletion = "pairwise") + # uses pairwise deletion. Default is listwise deletion.
  theme(axis.text.x=element_text(angle=90,
                                 hjust=0.95,vjust=0.2)) +  # rotates x-axis labels and aligns them to the right
  labs(fill="Pearson's r") +  # put a title on the legend
  theme(legend.title = element_text(hjust = 0.5)) + # centres the legend title
  guides(fill = guide_colourbar(ticks = FALSE,barwidth = 2, barheight = 15))  # removes the white ticks
