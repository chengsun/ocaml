library(ggplot2)

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

data <- read.csv("results")

plot_bench <- function (git_hash, bench) {
    filename <- sprintf("resultsbybench-%s-%s.png", git_hash, bench)
    if (!file.exists(filename)) {
        ggplot(data[data$benchmark == bench & data$git_hash == git_hash, ], aes(factor(compiler), time)) + geom_boxplot()
        ggsave(filename,width=29.7,height=21,units="cm",dpi=300)
    }
}

git_hash <- "5ff40d9bd"

apply(unique(data[c("git_hash", "benchmark")]), 1, function(i) {
    plot_bench(i["git_hash"], i["benchmark"])
})


data_averages <- aggregate(data["time"], data[, names(data) != "time"], gm_mean)

data_denoms <- data_averages[data_averages$compiler == "ocaml", ]
names(data_denoms)[names(data_denoms) == 'time'] <- 'base_time'
data_denoms$compiler <- NULL;

data_averages <- merge(data_averages, data_denoms);
data_averages$speedup <- with(data_averages, base_time/time);
data_averages$normalised_times <- with(data_averages, time/base_time);
data_averages$base_time <- NULL;

benchmarks <- unique(data[, "benchmark"])
git_hashes <- unique(data[, "git_hash"])

for (git_hash in git_hashes) {
    subset <- data_averages[
        data_averages$git_hash == git_hash &
        data_averages$compiler %in% c("gcc-O3","clang-O3", "ocamlopt") &
        (!startsWith(as.character(data_averages$benchmark), "closure") | endsWith(as.character(data_averages$benchmark), "_1")),
    ]
    #ggplot(subset, aes(benchmark, speedup)) + geom_bar(aes(fill = compiler), position = "dodge", stat = "identity") + scale_y_log10(breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,2,3,4,5,6,7,8,9,10),labels=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,2,3,4,5,6,7,8,9,10))
    ggplot(subset, aes(benchmark, normalised_times)) + geom_bar(aes(fill = compiler), position = "dodge", stat = "identity")
    ggsave(sprintf("resultsummary-%s.png", git_hash),width=29.7,height=21,units="cm",dpi=300)
}
