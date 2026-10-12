# Rebuilt from the saved results of 03_analysis.R; writes output/figures_bw/.

source("code/00_config.R")
load_packages(c("data.table", "ggplot2"))

FIG <- file.path(OUT_DIR, "figures_bw"); dir.create(FIG, showWarnings = FALSE)
TAB <- file.path(OUT_DIR, "tables")
ann <- fread(file.path(TAB, "annual_series.csv"))
ysz <- fread(file.path(TAB, "annual_b_by_size.csv"))

theme_bwp <- theme_bw(base_size = 12, base_family = "serif") +
  theme(panel.grid.minor = element_blank(), panel.grid.major = element_line(colour = "grey88", linewidth = .3),
        legend.position = "bottom", legend.title = element_blank(), legend.key.width = unit(1.6, "cm"),
        plot.title = element_blank(), strip.background = element_rect(fill = "grey92", colour = "black"))
vline <- geom_vline(xintercept = 1998.5, colour = "black", linewidth = .4, linetype = "dotted")
save <- function(p, f, w = 8, h = 4.5) ggsave(file.path(FIG, f), p, width = w, height = h, dpi = 300, bg = "white")

# Figure 1.1: distribution of DEF
p <- ggplot(ann, aes(fyear)) +
  geom_ribbon(aes(ymin = def_p10, ymax = def_p90, fill = "10th–90th percentile")) +
  geom_ribbon(aes(ymin = def_p25, ymax = def_p75, fill = "25th–75th percentile")) +
  geom_line(aes(y = def_med, linetype = "Median"), linewidth = .7) +
  geom_line(aes(y = def_mean, linetype = "Mean"), linewidth = .7) +
  scale_fill_manual(values = c("10th–90th percentile" = "grey88", "25th–75th percentile" = "grey70")) +
  scale_linetype_manual(values = c(Median = "solid", Mean = "longdash")) +
  vline + labs(x = NULL, y = "DEF / net assets") + theme_bwp
save(p, "fig_q1_def_distribution.png")

# Figure 1.2: components of DEF
comp <- melt(ann[, .(fyear, `Cash dividends` = div_mean, Investment = inv_mean, `Δ working capital` = dwc_mean,
                     `Internal cash flow` = cf_mean, `Financing deficit` = def_mean)], id.vars = "fyear")
p <- ggplot(comp, aes(fyear, value, linetype = variable, shape = variable)) +
  geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .55) + geom_point(size = 1.4) +
  scale_linetype_manual(values = c("dotted", "solid", "dotdash", "longdash", "solid")) +
  scale_shape_manual(values = c(NA, 16, NA, 2, 15)) + vline +
  labs(x = NULL, y = "Mean / net assets") + theme_bwp +
  guides(linetype = guide_legend(nrow = 2), shape = guide_legend(nrow = 2)) + theme(legend.key.width = unit(1.2, "cm"))
save(p, "fig_q1_def_components.png")

# Figure 1.3: sample composition
ch <- melt(ann[, .(fyear, `Loss firms (share)` = loss_share, `R&D reporters (share)` = rd_share,
                   `Dividend payers (share)` = payer_share, `Negative internal cash flow (share)` = neg_cf_share,
                   `Cash & ST investments / assets` = cash_mean, `PP&E / assets` = tang_mean)], id.vars = "fyear")
p <- ggplot(ch, aes(fyear, value)) + geom_line(linewidth = .6) + vline +
  facet_wrap(~variable, ncol = 3, scales = "free_y") + labs(x = NULL, y = NULL) + theme_bwp
save(p, "fig_q1_composition.png", 9, 5)

# Figure 3.1: b_t by size tercile
p <- ggplot(ysz, aes(fyear, b, linetype = group, shape = group)) +
  geom_ribbon(aes(ymin = b - 1.96 * se, ymax = b + 1.96 * se, group = group), fill = "grey85", alpha = .6, colour = NA) +
  geom_line(linewidth = .7) + geom_point(size = 1.5) +
  scale_linetype_manual(values = c("solid", "dashed")) + scale_shape_manual(values = c(16, 1)) +
  vline + labs(x = NULL, y = expression(b[t])) + theme_bwp
save(p, "fig_q3_b_by_size.png")

# Figure 1 (F&G): DEF, net debt, net equity
f1 <- melt(ann[, .(fyear, `Financing deficit/net assets` = def_mean, `Net debt issued/net assets` = nd_mean,
                   `Net equity issued/net assets` = ne_mean)], id.vars = "fyear")
fig1 <- function(x, brk) ggplot(x, aes(fyear, value, linetype = variable, shape = variable)) +
  geom_hline(yintercept = 0, linewidth = .3) + geom_line(linewidth = .65) + geom_point(size = 1.5) +
  scale_linetype_manual(values = c("solid", "dotted", "longdash")) + scale_shape_manual(values = c(NA, 1, 17)) +
  scale_x_continuous(breaks = brk) + labs(x = "Year", y = "Mean / net assets") + theme_bwp
save(fig1(f1[fyear <= 1998], seq(1971, 1998, 3)) + theme(legend.text = element_text(size = 9)), "fig1_replicated_1971_1998.png")
save(fig1(f1, seq(1971, 2025, 6)) + vline, "fig1_replicated.png")

# Figure 4.3: b_t and c_t
bc <- rbind(ann[, .(fyear, coef = b, se = b_se, series = "Net debt: b_t")], ann[, .(fyear, coef = c, se = c_se, series = "Net equity: c_t")])
p <- ggplot(bc, aes(fyear, coef, linetype = series, shape = series)) +
  geom_hline(yintercept = c(0, 1), linewidth = .3) + geom_hline(yintercept = .5, linewidth = .3, linetype = "dotted") +
  geom_ribbon(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se, group = series), fill = "grey85", alpha = .7, colour = NA) +
  geom_line(linewidth = .65) + geom_point(size = 1.5) +
  scale_linetype_manual(values = c("dashed", "solid")) + scale_shape_manual(values = c(1, 16)) +
  scale_x_continuous(breaks = seq(1971, 2025, 6)) + vline +
  labs(x = NULL, y = "Slope on DEF / net assets") + theme_bwp
save(p, "fig_q4_bt_ct.png", 8, 4.8)

# Figure 4.4: binned scatter
d <- readRDS(file.path(PROC_DIR, "panel.rds"))[in_reg == TRUE]
d[, period := fifelse(fyear <= 1998, "1971–1998", "1999–2025")]
d[, bin := cut(frank(def_na) / .N, 40, labels = FALSE), by = period]
bs <- melt(d[, .(def = mean(def_na), `Net debt` = mean(nd_na), `Net equity` = mean(ne_na)), by = .(period, bin)],
           id.vars = c("period", "bin", "def"))
p <- ggplot(bs, aes(def, value, shape = variable, linetype = variable)) +
  geom_abline(slope = 1, intercept = 0, linewidth = .3, linetype = "dotted") +
  geom_point(size = 1.8) + geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = .5, formula = y ~ x) +
  facet_wrap(~period) + scale_shape_manual(values = c(1, 16)) + scale_linetype_manual(values = c("dashed", "solid")) +
  labs(x = "Financing deficit / net assets", y = "Issue / net assets") + theme_bwp
save(p, "fig_q4_binned_scatter.png", 8.5, 4.5)
cat("B&W figures written to", FIG, "\n")
