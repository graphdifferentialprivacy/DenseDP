library(ggplot2)
library(reshape2)
library(plyr)
library(dplyr)
library(scales)

nets = c(
     "ca-GrQc",
    "ca-HepTh",
    "ca-HepPh",
    "ca-AstroPh",
    "ca-CondMat",
    "email-Enron",
   "loc-gowalla_edges",
  "loc-brightkite_edges",
  "facebook_combined",
  "lastfm_asia_edges",
  "deezer_europe_edges",
  "musae_DE_edges",
  "musae_ENGB_edges",
  "musae_facebook_edges",
  "musae_FR_edges",
  "musae_git_edges",
  "musae_squirrel_edges",
  "musae_crocodile_edges",
  "musae_chameleon_edges",
  "musae_PTBR_edges"
    )

edges = c(
  14496,
  25998,
  118521,
  198110,
  93497,
  183831,
  950327,
  214078,
  88234,
  27806,
  92752,
  153138,
  35324,
  171002,
  112666,
  289003,
  198493,
  170918,
  31421,
  31299
)

names(edges) <- nets

#Original data
data <- read.csv("bench_accuracy.txt", header = F, sep = ",")

names(data) <- c("net", "nnodes", "edges", "epsilon", "delta",
                     "baseline", "baseline_size",
                     "seq_mean", "seq_std", "seq_size_mean", "seq_size_std", "seq_jac_mean", "seq_jac_std", "seq_rec_mean", "seq_rec_std",
                     "par_mean", "par_std", "par_size_mean", "par_size_std", "par_jac_mean", "par_jac_std", "par_rec_mean", "par_rec_std",
                     "par_iter_mean", "par_iter_std")

# data$seq_std <- sqrt(data$seq_var)
# data$par_std <- sqrt(data$par_var)
# data$par_iter_std <- sqrt(data$par_iter_var)

data$seq_acc <- data$seq_mean / data$baseline
data$par_acc <- data$par_mean / data$baseline
#data$edges <- edges[data$net]
data$density <- data$edges / data$nnodes

density_data <- data.frame(net=data$net, density=data$density)

#old pick 6 before phase exprs
old_pick_6_index <- c(1, 10, 13, 4, 12, 9)

#pick_6 after phase exprs
# repick to select some good networks
pick_6 <- c(1, 13, 4, 12, 9, 17)


pick6_data <- subset(data,net %in% nets[pick_6] )
pick6_density <- subset(density_data,net %in% nets[pick_6] )
pick6_data$net <- factor(pick6_data$net, levels=nets[pick_6])
pick6_density$net <- factor(pick6_density$net, levels=nets[pick_6])

text_size <- 8

acc_data <- data[c("net", "epsilon", "delta", "seq_acc", "par_acc")]
names(acc_data) <- c("net", "epsilon", "delta", "Sᴇᴏ̨DᴇɴsᴇDP",  "PᴀʀDᴇɴsᴇDP")
#names(acc_data) <- c("net", "epsilon", "delta", "Bounded", "Unbounded")
acc_data <- melt(acc_data, id=c("net", "epsilon", "delta"))
plot <- ggplot(data = acc_data, aes(x = epsilon, y =value))
plot <- plot + geom_point(size = 1,aes(color=variable, shape=factor(delta)))
plot <- plot + geom_line(aes(color=variable, shape=factor(delta)))
plot <- plot + facet_wrap(~net)
plot <- plot + theme_bw()
plot <- plot + scale_x_continuous(trans = log2_trans())
plot <- plot + labs(y = "Accuracy", x="epsilon (\u03B5)")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size),
                     legend.text=element_text(size=text_size),
                     legend.title=element_text(size=text_size),
                     strip.text = element_text(size=text_size)
                     )
plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): "))
plot <- plot + geom_text(data=density_data,
                         mapping = aes(x = 0.5, y = 0.75, label = format(round(density, 2), nsmall=2)))
plot
ggsave("Accuracy_agg.png", plot, device = "png", width = 10, height = 6)

acc6_data <- pick6_data[c("net", "epsilon", "delta", "seq_acc", "par_acc")]
names(acc6_data) <- c("net", "epsilon", "delta", "Sᴇᴏ̨DᴇɴsᴇDP",  "PᴀʀDᴇɴsᴇDP")
#names(acc_data) <- c("net", "epsilon", "delta", "Bounded", "Unbounded")
acc6_data <- melt(acc6_data, id=c("net", "epsilon", "delta"))
plot <- ggplot(data = acc6_data, aes(x = epsilon, y =value))
plot <- plot + geom_point(size = 1,aes(color=variable, shape=factor(delta)))
plot <- plot + geom_line(aes(color=variable, shape=factor(delta)))
plot <- plot + facet_wrap(~net)
plot <- plot + theme_bw()
plot <- plot + scale_x_continuous(trans = log2_trans())
plot <- plot + labs(y = "Accuracy", x="epsilon (\u03B5)")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size+2),
                     legend.text=element_text(size=text_size+2),
                     legend.title=element_text(size=text_size+2),
                     strip.text = element_text(size=text_size),
                     plot.margin=grid::unit(c(0,0,-1,0), "mm"),
                     legend.margin = margin(t = -0.25, unit='cm'))
plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): ")
                      )
plot <- plot + geom_text(data=pick6_density,
                         mapping = aes(x = 0.5, y = 0.75, label = format(round(density, 2), nsmall=2)))
plot

ggsave("Accuracy_pick6.png", plot, device = "png", width = 6, height = 3)




iter_data <- pick6_data[c("net", "epsilon", "delta", "nnodes", "par_iter_mean")]
iter_data$par_iter_mean <- abs(iter_data$par_iter_mean)
iter_data$iter_ratio <- iter_data$par_iter_mean / iter_data$nnodes
plot <- ggplot(data = iter_data, aes(x = epsilon, y =iter_ratio ))
plot <- plot + geom_point(size = 1, aes(color=factor(delta)))
plot <- plot + geom_line(aes(color=factor(delta)))
#plot <- plot + scale_y_continuous(limits = c(0, 0.025))
plot <- plot + coord_cartesian(ylim = c(0, 0.025))
plot <- plot + facet_wrap(.~net)
plot <- plot + theme_bw()
plot <- plot + labs(y = "Ratio of iterations between PᴀʀDᴇɴsᴇDP over Sᴇᴏ̨DᴇɴsᴇDP", x="epsilon (\u03B5)")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size+2),
                     legend.text=element_text(size=text_size+2),
                     legend.title=element_text(size=text_size+2),
                     strip.text = element_text(size=text_size),
                     plot.margin=grid::unit(c(0,0,-1,0), "mm"),
                     legend.margin = margin(t = -0.25, unit='cm'))
plot <- plot + guides(color=guide_legend("Delta (\u03B4): "))
plot <- plot + geom_text(data=pick6_density,
                         mapping = aes(x = 0.5, y = 0.02, label = format(round(density, 2), nsmall=2)))
plot <- plot + scale_x_continuous(trans = log2_trans())
plot
ggsave("Par_Iterations_agg.png", plot, device = "png", width = 6, height = 3)

jacc_data <- pick6_data[c("net", "epsilon", "delta", "seq_jac_mean", "par_jac_mean")]
names(jacc_data) <- c("net", "epsilon", "delta", "Sᴇᴏ̨DᴇɴsᴇDP", "PᴀʀDᴇɴsᴇDP")
jacc_data <- melt(jacc_data, id=c("net", "epsilon", "delta"))
plot <- ggplot(data = jacc_data, aes(x = epsilon, y =value))
plot <- plot + geom_point(size = 1,aes(color=variable, shape=factor(delta)))
plot <- plot + geom_line(aes(color=variable, shape=factor(delta)))
plot <- plot + scale_x_continuous(trans = log2_trans())
plot <- plot + facet_wrap(~net)
plot <- plot + theme_bw()
plot <- plot + labs(y = "Jaccard", x="epsilon (\u03B5)")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size+2),
                     legend.text=element_text(size=text_size+2),
                     legend.title=element_text(size=text_size+2),
                     strip.text = element_text(size=text_size),
                     plot.margin=grid::unit(c(0,0,-1,0), "mm"),
                     legend.margin = margin(t = -0.25, unit='cm'))
plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): "))
plot <- plot + geom_text(data=pick6_density,
                         mapping = aes(x = 0.5, y = 0.75, label = format(round(density, 2), nsmall=2)))
plot
ggsave("Jaccard_agg.png", plot, device = "png", width = 6, height = 3)

## Comment out size plot, as we may not need it

# data$seq_size_ratio <- data$seq_size_mean / data$baseline_size
# data$par_size_ratio <- data$par_size_mean / data$baseline_size
# size_data <- data[c("net", "epsilon", "delta", "seq_size_ratio", "par_size_ratio")]
# names(size_data) <- c("net", "epsilon", "delta", "Sequential", "Parallel")
# size_data <- melt(size_data, id=c("net", "epsilon", "delta"))
# plot <- ggplot(data = size_data, aes(x = epsilon, y =value))
# plot <- plot + geom_point(size = 1,aes(color=variable, shape=factor(delta)))
# plot <- plot + geom_line(aes(color=variable, shape=factor(delta)))
# plot <- plot + scale_y_log10()
# plot <- plot + facet_wrap(~net)
# plot <- plot + theme_bw()
# plot <- plot + labs(y = "Size Ratio", x="epsilon (\u03B5)")
# plot <- plot + theme(legend.position="bottom",
#                      axis.text=element_text(size=text_size),
#                      axis.title=element_text(size=text_size),
#                      legend.text=element_text(size=text_size),
#                      legend.title=element_text(size=text_size),
#                      strip.text = element_text(size=text_size)
#                      )
# plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): "))
# plot <- plot + geom_text(data=density_data,
#                          mapping = aes(x = 0.25, y = 0.3, label = format(round(density, 2), nsmall=2)))
# plot
# ggsave("SizeRatio_agg.png", plot, device = "png", width = 8, height = 6)


rec_data <- pick6_data[c("net", "epsilon", "delta", "seq_rec_mean", "par_rec_mean")]
names(rec_data) <- c("net", "epsilon", "delta", "Sᴇᴏ̨DᴇɴsᴇDP", "PᴀʀDᴇɴsᴇDP")
rec_data <- melt(rec_data, id=c("net", "epsilon", "delta"))

plot <- ggplot(data = rec_data, aes(x = epsilon, y =value ))
plot <- plot + geom_point(size = 1,aes(color=variable, shape=factor(delta)))
plot <- plot + geom_line(aes(color=variable, shape=factor(delta)))
#plot <- plot + scale_y_log10()
plot <- plot + facet_wrap(~net)
plot <- plot + theme_bw()
plot <- plot + labs(y = "Recall", x="epsilon (\u03B5)")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size+2),
                     legend.text=element_text(size=text_size+2),
                     legend.title=element_text(size=text_size+2),
                     strip.text = element_text(size=text_size),
                     plot.margin=grid::unit(c(0,0,-1,0), "mm"),
                     legend.margin = margin(t = -0.25, unit='cm'))
plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): "))
plot <- plot + geom_text(data=pick6_density,
                         mapping = aes(x = 8, y = 0.25, label = format(round(density, 2), nsmall=2)))
plot <- plot + scale_x_continuous(trans = log2_trans())
plot
ggsave("Recall_agg.png", plot, device = "png", width = 6, height = 3)


epsilon_labels <- c("\u03B5 = 1", "\u03B5 = 2" )
names(epsilon_labels) <- c("1", "2")

acc_dens_data <- subset(data, epsilon %in% c(1, 2) & delta==1e-6)
acc_dens_data <- acc_dens_data[c("net", "density", "epsilon", "seq_acc", "par_acc")]
names(acc_dens_data) <- c("net", "density", "epsilon", "Sᴇᴏ̨DᴇɴsᴇDP", "PᴀʀDᴇɴsᴇDP")
acc_dens_data <- melt(acc_dens_data, id=c("net", "density", "epsilon"))
plot <- ggplot(acc_dens_data)
plot <- plot + geom_point(aes(x=density, y=value, color=variable))
plot <- plot + theme_bw()
plot <- plot + labs(y = "Accuracy", x="Density")
plot <- plot + theme(legend.position="bottom",
                     axis.text=element_text(size=text_size),
                     axis.title=element_text(size=text_size+2),
                     legend.text=element_text(size=text_size+2),
                     legend.title=element_text(size=text_size+2),
                     strip.text = element_text(size=text_size),
                     plot.margin=grid::unit(c(0,0,-1,0), "mm"),
                     legend.margin = margin(t = -0.25, unit='cm'))
plot <- plot + facet_wrap(epsilon~., labeller = labeller(epsilon = epsilon_labels)) 
plot <- plot + geom_smooth(aes(x=density, y=value, color=variable), 
                           method="lm",
                           #formula = y~log(x),
                           se=F)
plot <- plot + guides(color=guide_legend("Algorithm: "))
plot
ggsave("DensityVsAcc.png", plot, device = "png", width = 6, height = 3)


