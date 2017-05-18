library(ggplot2)
library(reshape2)

WIDTH <- 16
HEIGHT <- 10

gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
se <- function(x) sqrt(var(x)/length(x))

data <- read.csv("results")

plot_bench <- function (git_hash, bench) {
    filename <- sprintf("resultsbybench-%s-%s.png", git_hash, bench)
    if (!file.exists(filename)) {
        ggplot(data[data$benchmark == bench & data$git_hash == git_hash, ], aes(factor(compiler), time)) + geom_boxplot(outlier.colour = "red", outlier.shape = 1) +
theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1)) +
        labs(x="compiler", y="time (s)") + ggtitle(bench)
        ggsave(filename,width=WIDTH/2,height=HEIGHT/2,units="cm",dpi=300)
    }
}

apply(unique(data[c("git_hash", "benchmark")]), 1, function(i) {
    if (!startsWith(i["benchmark"], "closure") | endsWith(as.character(i["benchmark"]), "_1")) {
        plot_bench(i["git_hash"], i["benchmark"])
    }
})


data_averages <- aggregate(data["time"], data[, names(data) != "time"], mean)

mod_data_mean <- cbind(data_averages)
mod_data_mean$variable <- factor("mean")
mod_data_stddev <- aggregate(data["time"], data[, names(data) != "time"], sd)
mod_data_stddev$variable <- factor("sd")

data_denoms <- data_averages[data_averages$compiler == "ocaml", ]
names(data_denoms)[names(data_denoms) == 'time'] <- 'base_time'
data_denoms$compiler <- NULL;

data_averages <- merge(data_averages, data_denoms);
mod_data_normalised_times <- cbind(data_averages);

data_min <- aggregate(data["time"], data[, names(data) != "time"], min)
names(data_min)[names(data_min) == 'time'] <- 'min'
data_max <- aggregate(data["time"], data[, names(data) != "time"], max)
names(data_max)[names(data_max) == 'time'] <- 'max'
data_se <- aggregate(data["time"], data[, names(data) != "time"], se)
names(data_se)[names(data_se) == 'time'] <- 'se'
data_averages <- merge(data_averages, data_min);
data_averages <- merge(data_averages, data_max);
data_averages <- merge(data_averages, data_se);

data_averages$speedup <- with(data_averages, base_time/time);
data_averages$normalised_times <- with(data_averages, time/base_time);
data_averages$base_time <- NULL;

mod_data_normalised_times$time <- with(mod_data_normalised_times, time/base_time);
mod_data_normalised_times$variable <- factor("norm_time");
mod_data_normalised_times <- mod_data_normalised_times[, names(mod_data_normalised_times) != "base_time"];

data_summary <- rbind(mod_data_mean, mod_data_stddev, mod_data_normalised_times)

benchmarks <- unique(data$benchmark)
git_hashes <- unique(data[, "git_hash"])

for (git_hash in git_hashes) {
    subset <- data_averages[
        data_averages$git_hash == git_hash &
        data_averages$compiler %in% c("gcc-O3","clang-O3", "ocamlopt") &
        (!startsWith(as.character(data_averages$benchmark), "closure") | endsWith(as.character(data_averages$benchmark), "_1")),
    ]
    #ggplot(subset, aes(benchmark, speedup)) + geom_bar(aes(fill = compiler), position = "dodge", stat = "identity") + scale_y_log10(breaks=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,2,3,4,5,6,7,8,9,10),labels=c(.1,.2,.3,.4,.5,.6,.7,.8,.9,1,2,3,4,5,6,7,8,9,10))
    ggplot(subset, aes(benchmark, normalised_times)) + geom_bar(aes(fill = compiler), position = "dodge", stat = "identity") +
        geom_hline(yintercept = 1) +
        theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust=1), plot.margin = margin(10, 10, 10, 30)) +
        scale_y_continuous(limits = c(0, max(subset$normalised_times)*1.05), expand=c(0,0)) +
        labs(x="benchmark", y="normalised time")
    ggsave(sprintf("resultsummary-%s.png", git_hash),width=WIDTH,height=HEIGHT,units="cm",dpi=300)

    plot_closure <- function(out_name, bench_type, compilers) {
        subset <- data_averages[
            data_averages$git_hash == git_hash &
            data_averages$compiler %in% compilers &
            startsWith(as.character(data_averages$benchmark), sprintf("closure_%s", bench_type)),
        ]
        subset$n <- sapply(subset$benchmark, function(s) {
            strtoi(gsub(sprintf("^closure_%s_", bench_type),"",s), base=10)
        })
        if (length(subset$n) > 0) {
            ggplot(subset, aes(n, time, group=compiler, color=compiler, shape=compiler)) +
              geom_line() +
              # geom_point() +
              scale_x_continuous(breaks = seq(min(subset$n), max(subset$n), by = 1)) +
              scale_y_continuous(limits = c(0, max(subset$time)*1.05), expand=c(0,0)) +
              #geom_errorbar(colour="black",width=0.3,aes(ymin=time-se, ymax=time+se)) +
              labs(x="n",y="time (s)")
            ggsave(sprintf("resultclosure_%s_summary_%s-%s.png", bench_type, out_name, git_hash),width=WIDTH,height=HEIGHT,units="cm",dpi=300)
        } else {
            print(sprintf("skipping resultclosure_%s_summary_%s-%s.png", bench_type, out_name, git_hash))
        }
    }

    plot_closure("all", "capture", c("gcc-O3","clang-O3", "ocaml", "ocamlopt"))
    plot_closure("all", "create", c("gcc-O3","clang-O3", "ocaml", "ocamlopt"))
    plot_closure("all", "invoke", c("gcc-O3","clang-O3", "ocaml", "ocamlopt"))

    plot_closure("opts", "capture", c("gcc-O3","clang-O3", "gcc-O2","clang-O2","ocamlopt"))
    plot_closure("opts", "create", c("gcc-O3","clang-O3", "gcc-O2","clang-O2","ocamlopt"))
    plot_closure("opts", "invoke", c("gcc-O3","clang-O3", "gcc-O2","clang-O2","ocamlopt"))
}


#plot_invoke <- function(git_hash, bench_type, compiler){
#    subset <- data[
#        data$git_hash == git_hash &
#        data$compiler == compiler &
#        startsWith(as.character(data$benchmark), sprintf("closure_%s", bench_type)),
#    ]
#    subset$n <- sapply(subset$benchmark, function(s) {
#        strtoi(gsub(sprintf("^closure_%s_", bench_type),"",s), base=10)
#    })
#    ggplot(subset, aes(n, time)) + geom_smooth() + geom_point() +
#scale_x_continuous(breaks = seq(min(subset$n), max(subset$n), by = 1)) +
#scale_y_continuous(breaks = round(seq(0, max(subset$time), by = 0.5),1))
#    ggsave(sprintf("resultclosure_%s_%s-%s.png", bench_type, compiler, git_hash),width=WIDTH,height=HEIGHT,units="cm",dpi=300)
#}
#
#plot_invoke("b477d4580", "invoke", "gcc-O3")


write_stats <- function(git_hash) {
    subset <- data_summary[
        data_summary$git_hash == git_hash &
        (!startsWith(as.character(data_summary$benchmark), "closure") | endsWith(as.character(data_summary$benchmark), "_1")),
    ]
    the_table <- acast(subset, benchmark~compiler+variable, value.var="time")
    write.table(format(the_table, digits=1), file=sprintf("summary-%s.csv", git_hash),
            row.names = TRUE, col.names = NA, sep=",")
    for (i in levels(subset$benchmark)) {
        thisset <- subset[subset$benchmark == i,]
        if (nrow(thisset) > 0) {
            the_table <- acast(thisset, compiler~variable, value.var="time")
            write.table(format(the_table, digits=1), file=sprintf("summary_%s-%s.csv", i, git_hash),
                    row.names = TRUE, col.names = NA, sep=",")
        }
    }
}

write_stats("b477d4580")
