#!/usr/bin/env nix-shell
#!nix-shell -p "rWrapper.override{packages = [ rPackages.data_table rPackages.lubridate rPackages.ggplot2 rPackages.svglite ];}"
#!nix-shell -i "Rscript --vanilla"

require(data.table)
require(ggplot2)
require(lubridate)
require(magrittr)
require(RColorBrewer)
require(scales)

timings <- fread("sync-results.tsv")

podstats <- rbind(
    cbind(Service="cardano-node", fread("sync-node.tsv")),
    cbind(Service="postgresql", fread("sync-postgres.tsv")),
    cbind(Service="chain-indexer", fread("sync-chain-indexer.tsv")),
    cbind(Service="chain-sync", fread("sync-chain-sync.tsv")),
    cbind(Service="marlowe-indexer", fread("sync-marlowe-indexer.tsv"))
)[order(`Service`)]
podstats[, `Time from Start [s]` := `Epoch` - timings[, min(`Start Epoch`)]]

podstats %>% summary

nodeStart <- podstats[`Service` == "cardano-node", min(`Time from Start [s]`)]
chainStart <- podstats[`Service` == "chain-indexer", min(`Time from Start [s]`)]
marloweStart <- podstats[`Service` == "marlowe-indexer", min(`Time from Start [s]`)]
finish <- podstats[, max(`Time from Start [s]`)]

loads <-
  data.table(
    Load=c("Sync node database", "Sync chain schema", "Sync marlowe schema"),
    time=c(
      chainStart - 90,
      (chainStart + marloweStart) / 2,
      (marloweStart + finish) / 2
    ),
    cpu=sum(podstats[, .(w=last(`CPU [%/100]`, 1)), by=`Service`]$w) / 2,
    memory=sum(podstats[, .(w=last(`Memory [MB]`, 1)), by=`Service`]$w) / 2
  )
loads
services <-
  podstats[,
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
    podstats[`Time from Start [s]` >= chainStart - 120],
    aes(x=`Time from Start [s]`, y=`CPU [%/100]`)
  ) +
  geom_area(aes(fill=`Service`)) +
  geom_text(data=loads, mapping=aes(x=time, y=cpu, label=Load), hjust=0, angle=90) +
  geom_text(data=services, mapping=aes(x=time, y=cpu, label=Service), hjust=0, nudge_x=10) +
  geom_vline(xintercept=chainStart, linetype="solid", color="black") +
  geom_vline(xintercept=marloweStart, linetype="solid", color="black") +
  geom_vline(xintercept=finish, linetype="solid", color="black") +
  xlim(chainStart - 120, finish + 100) +
  scale_fill_manual(values=c("#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99", "#CAB2D6")) +
  theme(legend.position="none") +
  ggtitle("Database Syncing")
ggsave(g, file="sync-cpu.png", width=7, height=5, units="in")

g <-
  ggplot(
    podstats[`Time from Start [s]` >= chainStart - 120],
    aes(x=`Time from Start [s]`, y=`Memory [MB]`)
  ) +
  geom_area(aes(fill=`Service`)) +
  geom_text(data=loads, mapping=aes(x=time, y=memory, label=Load), hjust=0, angle=90) +
  geom_text(data=services, mapping=aes(x=time, y=memory, label=Service), hjust=0, nudge_x=10) +
  geom_vline(xintercept=chainStart, linetype="solid", color="black") +
  geom_vline(xintercept=marloweStart, linetype="solid", color="black") +
  geom_vline(xintercept=finish, linetype="solid", color="black") +
  xlim(chainStart - 120, finish + 100) +
  scale_fill_manual(values=c("#A6CEE3", "#B2DF8A", "#33A02C", "#FB9A99", "#CAB2D6")) +
  theme(legend.position="none") +
  ggtitle("Database Syncing")
ggsave(g, file="sync-memory.png", width=7, height=5, units="in")

rawDisk <- fread("disk.tsv")
rawDisk[, `Size [MB]`:=`Size [B]`/1000000]

g <-
  ggplot(rawDisk, aes(x=`Database`, y=`Size [MB]`, fill=`Database`)) +
  geom_bar(stat="identity") +
  theme(legend.position="none") +
  ggtitle("Disk Usage")
ggsave(g, file="disk.png", width=7, height=5, units="in")
