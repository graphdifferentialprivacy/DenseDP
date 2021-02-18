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

#Phase data
data <- read.csv("bench_accuracy_phase.txt", header = F, sep = ","  )

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




#Bounded vs Unbounded plots
p6_acc_data <- pick6_data[c("net", "epsilon", "delta", "seq_acc", "par_acc")]
names(p6_acc_data) <- c("net", "epsilon", "delta", "PʜᴀsᴇDᴇɴsᴇDP", "PᴀʀDᴇɴsᴇDP")
p6_data <- melt(p6_acc_data, id=c("net", "epsilon", "delta"))
plot <- ggplot(data = p6_data, aes(x = epsilon, y =value))
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
plot <- plot + guides(color=guide_legend("Alg: "), shape=guide_legend("Delta (\u03B4): "))
plot <- plot + geom_text(data=pick6_density,
                         mapping = aes(x = 0.5, y = 0.75, label = format(round(density, 2), nsmall=2)))
plot
ggsave("Accuracy_bounded_undbounded.png", plot, device = "png", width = 6, height = 3)
