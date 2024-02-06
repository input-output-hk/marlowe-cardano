#!/usr/bin/env nix-shell
#!nix-shell -p "rWrapper.override{packages = [ rPackages.data_table rPackages.lubridate rPackages.ggplot2 rPackages.svglite ];}"
#!nix-shell -i "Rscript --vanilla"

require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(RColorBrewer)

rawHeaderSync <- fread("HeaderSync.tsv")
rawBulkSync <- fread("BulkSync.tsv")
rawSync <- fread("Sync.tsv")
rawQuery <- fread("Query.tsv")
rawLifecycle <- fread("Lifecycle.tsv")

startHeaderSync <- rawHeaderSync[, min(start)]
startBulkSync <- rawBulkSync[, min(start)]
startSync <- rawSync[, min(start)]
startQuery <- rawQuery[, min(start)]
startLifecycle <- rawLifecycle[, min(start)]
finishLifecycle <- rawLifecycle[, max(finish)]

podstats <- rbind(
    cbind(Service="cardano-node", fread("benchmark-node.tsv")),
    cbind(Service="postgresql", fread("benchmark-postgres.tsv")),
    cbind(Service="chain-indexer", fread("benchmark-chain-indexer.tsv")),
    cbind(Service="chain-sync", fread("benchmark-chain-sync.tsv")),
    cbind(Service="marlowe-indexer", fread("benchmark-marlowe-indexer.tsv")),
    cbind(Service="marlowe-sync", fread("benchmark-marlowe-sync.tsv")),
    cbind(Service="marlowe-contract", fread("benchmark-contract.tsv")),
    cbind(Service="marlowe-tx", fread("benchmark-tx.tsv")),
    cbind(Service="marlowe-proxy", fread("benchmark-proxy.tsv"))
)[order(`Service`)]
podstats[, `Time from Start [s]` := as.numeric(lubridate::as_datetime(`Epoch`) - startHeaderSync)]

podstats %>% summary

loads <-
  data.table(
    Load=c("HeaderSync", "BulkSync", "Sync", "Query", "Lifecycle"),
    time=c(
      rawHeaderSync[, (min(as.numeric(start))+max(as.numeric(finish)))/2] - as.numeric(startHeaderSync),
      rawBulkSync[, (min(as.numeric(start))+max(as.numeric(finish)))/2] - as.numeric(startHeaderSync),
      rawSync[, (min(as.numeric(start))+max(as.numeric(finish)))/2] - as.numeric(startHeaderSync),
      rawQuery[, (min(as.numeric(start))+max(as.numeric(finish)))/2] - as.numeric(startHeaderSync),
      rawLifecycle[, (min(as.numeric(start))+max(as.numeric(finish)))/2] - as.numeric(startHeaderSync)
    ),
    cpu=sum(podstats[, .(w=last(`CPU [%/100]`, 1)), by=`Service`]$w) / 2,
    memory=sum(podstats[, .(w=last(`Memory [MB]`, 1)), by=`Service`]$w) / 2
  )

services <-
  podstats[
    `Time from Start [s]` <= as.numeric(finishLifecycle) - as.numeric(startHeaderSync) + 120,
    .(
      time=tail(`Time from Start [s]`, 1),
      cpu=tail(`CPU [%/100]`, 1),
      memory=tail(`Memory [MB]`, 1)
    ),
    by=`Service`
  ][order(-`Service`)]
services[, `:=`(cpu=cumsum(cpu)-cpu/2, memory=cumsum(memory)-memory/2)]

g <-
  ggplot(
    podstats[`Time from Start [s]` >= -120 & `Time from Start [s]` <= as.numeric(finishLifecycle) - as.numeric(startHeaderSync) + 120],
    aes(x=`Time from Start [s]`, y=`CPU [%/100]`)
  ) +
  geom_area(aes(fill=`Service`)) +
  geom_text(data=loads, mapping=aes(x=time, y=cpu, label=Load), hjust=0, angle=90) +
  geom_text(data=services, mapping=aes(x=time, y=cpu, label=Service), hjust=0, nudge_x=20) +
  geom_vline(xintercept=as.numeric(startHeaderSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startBulkSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startQuery) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startLifecycle) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(finishLifecycle) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  xlim(-120, as.numeric(finishLifecycle) - as.numeric(startHeaderSync) + 500) +
  scale_fill_manual(values=c("#A6CEE3", "#B2DF8A", "#33A02C", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6")) +
  theme(legend.position="none") +
  ggtitle("Benchmark Execution")
ggsave(g, file="benchmark-cpu.png", width=7, height=5, units="in")

g <-
  ggplot(
    podstats[`Time from Start [s]` >= -120 & `Time from Start [s]` <= as.numeric(finishLifecycle) - as.numeric(startHeaderSync) + 120],
    aes(x=`Time from Start [s]`, y=`Memory [MB]`)
  ) +
  geom_area(aes(fill=`Service`)) +
  geom_text(data=loads, mapping=aes(x=time, y=memory, label=Load), hjust=0, angle=90) +
  geom_text(data=services, mapping=aes(x=time, y=memory, label=Service), hjust=0, nudge_x=20) +
  geom_vline(xintercept=as.numeric(startHeaderSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startBulkSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startSync) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startQuery) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(startLifecycle) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  geom_vline(xintercept=as.numeric(finishLifecycle) - as.numeric(startHeaderSync), linetype="solid", color="black") +
  xlim(-120, as.numeric(finishLifecycle) - as.numeric(startHeaderSync) + 500) +
  scale_fill_manual(values=c("#A6CEE3", "#B2DF8A", "#33A02C", "#33A02C", "#FB9A99", "#E31A1C", "#FDBF6F", "#FF7F00", "#CAB2D6")) +
  theme(legend.position="none") +
  ggtitle("Benchmark Execution")
ggsave(g, file="benchmark-memory.png", width=7, height=5, units="in")

g <- ggplot(
  melt(
    rawHeaderSync[, .(`Metric`=metric, `Blocks [/s]`=blocksPerSecond, `Contracts [/s]`=contractsPerSecond)],
    id.vars=c("Metric"), variable.name="Benchmark", measure.vars=c("Blocks [/s]", "Contracts [/s]"), value.name="Measurement"
  ),
  aes(y=Measurement)
) +
  geom_boxplot() +
  facet_wrap(. ~ Benchmark, scales="free") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Header Sync Benchmark")
ggsave(g, file="header-sync.png", width=4, height=3, units="in")

g <- ggplot(
  melt(
    rawBulkSync[, .(`Metric`=metric, `Blocks [/s]`=blocksPerSecond, `Creates [/s]`=createsPerSecond, `Applies [/s]`=applyInputsPerSecond, `Withdraws [/s]`=withdrawsPerSecond)],
    id.vars=c("Metric"), variable.name="Benchmark", measure.vars=c("Blocks [/s]", "Creates [/s]", "Applies [/s]", "Withdraws [/s]"), value.name="Measurement"
  ),
  aes(y=Measurement)
) +
  geom_boxplot() +
  facet_wrap(. ~ Benchmark, scales="free") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Bulk Sync Benchmark")
ggsave(g, file="bulk-sync.png", width=5, height=6, units="in")

g <- ggplot(
  melt(
    rawSync[, .(`Metric`=metric, `Contracts [/s]`=contractsPerSecond, `Steps [/s]`=stepsPerSecond)],
    id.vars=c("Metric"), variable.name="Benchmark", measure.vars=c("Contracts [/s]", "Steps [/s]"), value.name="Measurement"
  ),
  aes(y=Measurement)
) +
  geom_boxplot() +
  facet_wrap(. ~ Benchmark, scales="free") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Sync Benchmark")
ggsave(g, file="sync.png", width=5, height=3, units="in")

g <- ggplot(
  melt(
    rawQuery[, .(`Metric`=metric, `Queries [/s]`=queriesPerSecond, `Pages [/s]`=pagesPerSecond)],
    id.vars=c("Metric"), variable.name="Benchmark", measure.vars=c("Queries [/s]", "Pages [/s]"), value.name="Measurement"
  ),
  aes(y=Measurement)
) +
  geom_boxplot() +
  facet_wrap(. ~ Benchmark, scales="free") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Query Benchmark")
ggsave(g, file="query.png", width=5, height=3, units="in")

g <- ggplot(
  melt(
    rawLifecycle[, .(`Metric`=metric, `Contracts [/s]`=contractsPerSecond, `Creates [/s]`=creationsPerSecond, `Applies [/s]`=appliesPerSecond, `Withdraws [/s]`=withdrawsPerSecond)],
    id.vars=c("Metric"), variable.name="Benchmark", measure.vars=c("Contracts [/s]", "Creates [/s]", "Applies [/s]", "Withdraws [/s]"), value.name="Measurement"
  ),
  aes(y=Measurement)
) +
  geom_boxplot() +
  facet_wrap(. ~ Benchmark, scales="free") +
  theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank()) +
  ggtitle("Lifecycle Benchmark")
ggsave(g, file="lifecycle.png", width=5, height=6, units="in")
