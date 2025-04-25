# It creates publication-ready figures and tables and saves them in
# `/results/plots/figures` and `/results/tables`

# setup
source("00_source.R")

# GAM - spline machinery
library(mgcv)

# Bayesian models
library(rstan)
library(posterior)
library(tidybayes)
library(ggdist)

# table making
library(kableExtra)
library(gt)

# load analysis setup --------------------------------------------------------------------

df_iter <-
  readRDS(file.path(
    res_path, "R_output", g("association-analysis_setup_gen-quant.Rds")
  ))

# order
df_iter <-
  df_iter |> 
  mutate(
    exposure = factor(exposure, levels=all_env),
    disease = factor(disease, levels=c(main_dis, alt_dis, other_dis, unre_dis))
  ) |> 
  arrange(disease, exposure)

# subset diseases and exposures
# df_iter <-
#   df_iter |>
#   filter(
#     # exposure %in% main_env,
#     disease %in% main_dis
#   )

# define exposure-response cases of interest ---------------------------------------------

main_cases <-
  tribble(
    ~ exposure, ~ disease,
    "pm2p5", "Chronic Respiratory Disease",
    "pm2p5", "Infectious Eye Disease",
    "pm2p5", "Malaria",
    "greenness", "Diabetes Mellitus",
    "greenness", "Urinary Tract Infections",
    "greenness", "Malaria",
    "n_raindays", "Diarrhea",
    "n_raindays", "Urinary Tract Infections",
    "n_raindays", "Upper Respiratory Infections",
    
    # "n_raindays", "Malaria",
  )

# create filtering id
main_cases <-
  main_cases |> 
  mutate(
    filt_id = g("{exposure}.{disease}")
  )

# e.g.
df_iter |> 
  filter(
    g("{exposure}.{disease}") %in% main_cases$filt_id
  ) |> 
  arrange(exposure)

# supp cases
supp_cases <-
  tribble(
    ~ exposure, ~ disease,
    "pm2p5", "Skin Infection - Fungal",
    "utci", "Malnutrition",
    "utci", "Tuberculosis",
    "total_rainfall", "Malnutrition",
    "total_rainfall", "Peptic Ulcers",
    "n_raindays", "Peptic Ulcers"
  )

# create filtering id
supp_cases <-
  supp_cases |> 
  mutate(
    filt_id = g("{exposure}.{disease}")
  )

# e.g.
df_iter |> 
  filter(
    g("{exposure}.{disease}") %in% supp_cases$filt_id
  ) |> 
  arrange(exposure)
  


# get disease name grobs -----------------------------------------

df_p_names <-
  tibble(
    disease = unique(df_iter$disease),
    disease_short = str_replace(disease, "^(\\S+\\s+\\S+)\\s+", "\\1\n")
  )

df_p_names <-
  df_p_names |> 
  mutate(
    p_name = map(disease, \(x) {
      grid::textGrob(
        label=x,
        hjust=0,
        x=0.01, y=0.5,
        gp=grid::gpar(fontsize=22, fontface="bold")
      )
    }),
    p_name_short = map(disease_short, \(x) {
      grid::textGrob(
        label=x,
        hjust=0,
        x=0.01, y=0.5,
        gp=grid::gpar(fontsize=22, fontface="bold")
      )
    })
  )

df_p_names |> 
  filter(disease %in% main_dis) |> 
  pull(p_name_short) |> 
  wrap_plots(ncol=1)

# Exposure-response ----------------------------------------------------------------------

## get base plots -----------------

# which(df_iter$disease == "Tuberculosis")
# df_iter$data[[4]]$E |> sort()

df_iter$p_expo_resp <-
  pmap(
    df_iter,
    function(fit_path, pred_expo_resp_path, cf_cond_path, label, units, disease, data,
             exposure, ...) {
      print(fit_path)
      
      # load data
      pred_expo_resp <- readRDS(pred_expo_resp_path)
      cf_cond <- readRDS(cf_cond_path)
      
      # tidy counterfactual conditions
      cf_cond <-
        cf_cond |> 
        rename(centering = eta) |> 
        select(- E)
      # center the data at cf exposure
      df_plot <-
        pred_expo_resp |> 
        left_join(cf_cond, join_by(L)) |> 
        mutate(
          district = factor(L, labels = levels(data$district))
        ) |> 
        mutate(
          eta = eta / centering
        )
    
      # if Rainfall, remove data for exposure >= 350
      # (too much uncertainty, few values observed in data)
      if (exposure == "total_rainfall") {
        df_plot <-
          df_plot |>
          filter(E <= 350)

      } else if (exposure == "utci"){
      # if UTCI, remove data for exposure <= 18
      # (there were a couple outliers further away from 18)
        df_plot <-
          df_plot |>
          filter(E >= 18)
      }
      
      # get ylim
      eta_rng <- df_plot |> 
        median_qi(eta) |> 
        pull(eta) |> 
        range()
      
      # if the median curve out of (0.7, 2), use the range
      if (any(eta_rng <= 0.7 | eta_rng >= 2)) {
        ylim <- eta_rng
        annot_yax <- TRUE
      } else {
        ylim <- c(0.7, 2)
        annot_yax <- FALSE
      }
      
      p <-
        df_plot |> 
        ggplot() +
        # facet_grid(
        #   cols = vars(district)
        # ) +
        geom_hline(yintercept=1, linetype=2) +
        stat_lineribbon(
          aes(x=E, ydist=eta,
              color=stage(district, after_scale = muted(color)),
              fill=stage(district, after_scale = alpha(fill, 0.3)),
              ),
          # .width=c(0.95, 0.66, 0),
          .width=c(0.95, 0),
          linewidth=1.5,
        ) |> 
        ggblend::blend("darken") +
        labs(
          y = "IRR",
          x = TeX(glue("{label} {units}")),
          color="District",
          fill = "District",
          title = disease
        ) +
        coord_cartesian(
          ylim = ylim
        ) +
        theme(
          axis.title.x = element_text(face="bold")
        )
      
      # add annotation if special y axis
      if (annot_yax) {
        p +
          annotate(
            "text",
            x = min(df_plot$E),
            y = ylim[2],
            label="*",
            hjust = 0.5, vjust = 1,
            size = 20
          )
      } else {
        p
      }
    }
  )

df_iter$p_expo_resp[[6]]


## compose figure ---------------------------------

# plotting function
plot_expo_resp <- function(df_iter, bottom_axis_title=FALSE, histo_plot=TRUE) {
  
  # disease vector (df_iter could be pre-filtered)
  disease_vec <- unique(df_iter$disease)
  
  l_p_row <-
    imap(
      disease_vec,
      function(dis, i) {
        # get disease name grob
        p_name <-  
          df_p_names |> 
          filter(disease == dis) |> 
          pull(p_name) |> 
          pluck(1) |> 
          wrap_plots()
        
        # get row plots
        p_row <-
          df_iter |> 
          filter(disease == dis) |> 
          pull(p_expo_resp) |> 
          wrap_plots(nrow = 1) +
          plot_layout(
            guides="collect",
            axis_titles = "collect_y"
          ) &
          ggtitle(NULL)
        
        # add theme
        p_row <-
          p_row &
          theme(
            legend.position = "none",
            axis.title.x = element_blank()
          )
        
        
        # if want axis titles at bottom row and bottom row, change theme
        if (bottom_axis_title & (i == length(disease_vec))) {
          p_row <-
            p_row &
            theme(
              axis.title.x = element_text(),
              legend.position = "bottom",
              legend.title = element_text(size=rel(1.2)),
              legend.text = element_text(size=rel(1.2))
            )
        }
        
        (
          p_name / p_row +
            plot_layout(heights = c(1,99))
        )
      }
    ) |> 
    wrap_plots(ncol=1)
  
  l_p_row
  
  
  # if want histograms of exposures
  if (histo_plot) {
    
    # plot them
    expo_hists <-
      pmap(
        filter(df_iter, disease == df_iter$disease[1]),
        function(exposure, data, label, units, ...) {
          
          if (exposure == "total_rainfall") {
            xlim <- c(0, 350)
            bins <- 25
            
          } else if (exposure == "utci") {
            xlim <- c(18, NA)
            bins <- 20
            
          }
          else {
            xlim <- range(data$E)
            bins <- 20
          }
          
          data |>
            ggplot() +
            geom_histogram(
              aes(x=E, fill=district, color=after_scale(fill)), bins=bins,
              position = "identity",
              alpha=0.5
            ) |>
            ggblend::blend("multiply") +
            scale_y_continuous(
              breaks = breaks_pretty(4)
              # label = label_number(accuracy=1)
            ) +
            coord_cartesian(
              xlim = xlim
            ) +
            labs(
              y = "Count",
              x = TeX(glue("{label} {units}")),
              fill = "District",
            ) +
            guides(
              fill = guide_legend(override.aes = list(alpha=1))
            )
        }
      )
    
    # wrap histograms
    p_hist <-
      expo_hists |> 
      wrap_plots(nrow=1) +
      plot_layout(
        guides="collect",
        axis_titles = "collect"
      ) &
      theme(
        legend.position = "bottom",
        legend.title = element_text(size=rel(1.2)),
        legend.text = element_text(size=rel(1.2))
      )
    
    # get final plot
    p_final <-
      (
        (
          l_p_row |> 
            wrap_plots(ncol=1)
        ) / (
          p_hist
        )
      ) +
      plot_layout(
        heights = c(length(l_p_row), 0.7)
      )
    
    # if no histogram of exposures
  } else {
    
    # get final plot
    p_final <-
      l_p_row |> 
      wrap_plots(ncol=1)
  }
  
  return(p_final)
}

# test
plot_expo_resp(
  df_iter = {
    df_iter |> 
      filter(
        disease %in% main_dis[1:2],
        exposure %in% all_env[1:2]
      )},
  bottom_axis_title=FALSE,
  histo_plot=TRUE
)

# main diseases
pdf(
  file.path(res_path, "plots", "figures", "exposure-response_main.pdf"),
  width = 20 * 0.2 * length(all_env),
  height = 10.6 * 0.4 * (length(main_dis) + 0.7) 
)

plot_expo_resp(
  df_iter = {
    df_iter |> 
      filter(
        disease %in% main_dis,
        exposure %in% all_env
      )},
  bottom_axis_title=FALSE,
  histo_plot=TRUE
)

dev.off()

# alternative diseases (interesting candidates)
pdf(
  file.path(res_path, "plots", "figures", "exposure-response_alt.pdf"),
  width = 20 * 0.2 * length(all_env),
  height = 10.6 * 0.4 * (length(alt_dis)) 
)

plot_expo_resp(
  df_iter = {
    df_iter |> 
      filter(
        disease %in% alt_dis,
        exposure %in% all_env
      )},
  bottom_axis_title=TRUE,
  histo_plot=FALSE
)

dev.off()


# other diseases
pdf(
  file.path(res_path, "plots", "figures", "exposure-response_other.pdf"),
  width = 20 * 0.2 * length(all_env),
  height = 10.6 * 0.4 * (length(other_dis)) 
)

plot_expo_resp(
  df_iter = {
    df_iter |> 
      filter(
        disease %in% other_dis,
        exposure %in% all_env
      )},
  bottom_axis_title=TRUE,
  histo_plot=FALSE
)

dev.off()


# unreliable diseases
pdf(
  file.path(res_path, "plots", "figures", "exposure-response_unreliable.pdf"),
  width = 20 * 0.2 * length(all_env),
  height = 10.6 * 0.4 * (length(unre_dis)) 
)

plot_expo_resp(
  df_iter = {
    df_iter |> 
      filter(
        disease %in% unre_dis,
        exposure %in% all_env
      )},
  bottom_axis_title=TRUE,
  histo_plot=FALSE
)

dev.off()


## compose filt figure ------------------

### main --------------

df_iter_filt <-
  df_iter |> 
  filter(
    g("{exposure}.{disease}") %in% main_cases$filt_id
  ) |> 
  mutate(
    exposure = fct_relevel(exposure, c(
      "pm2p5",
      "greenness",
      "n_raindays"
    ))
  ) |> 
  arrange(exposure)

# iterate over exposures
iter_elem <- unique(df_iter_filt$exposure)

p_assemb <-
  imap(
    iter_elem,
    function(expo, i) {
      
      # get row plot
      p_row <-
        df_iter_filt |> 
        filter(exposure == expo) |> 
        pull(p_expo_resp) |> 
        wrap_plots(nrow = 1) +
        plot_layout(
          axis_titles = "collect",
          guides = "collect"
        )
      
      # if last row, keep legend
      if (i == length(iter_elem)) {
        p_row &
          theme(
            legend.position = "bottom"
          )
        
      } else {
        p_row &
          theme(
            legend.position = "none"
          )
      }
      
    }) |> 
  wrap_plots(
    ncol=1
  )

# p_assemb[[1]] <- (p_assemb[[1]] + plot_spacer())



pdf(
  file.path(res_path, "plots", "figures", "exposure-response_filtered_main.pdf"),
  width = 20 * 0.21 * 3,
  height = 10.6 * 0.44 * length(unique(df_iter_filt$exposure))
)

p_assemb &
  theme(
    plot.title = element_text(size = rel(0.8))
  )

dev.off()


### supp -----------

df_iter_filt <-
  df_iter |> 
  filter(
    g("{exposure}.{disease}") %in% supp_cases$filt_id
  ) |> 
  mutate(
    exposure = fct_relevel(exposure, c(
      "pm2p5",
      "utci",
      "total_rainfall",
      "n_raindays"
    ))
  ) |> 
  arrange(exposure)

# 3 plots per row
iter_elem <- 1:floor(nrow(df_iter_filt)/3)

p_assemb <-
  imap(
    iter_elem,
    function(i_row, i) {
      print(i_row)
      
      # get row plot
      
      sub_df <-
        df_iter_filt[(3*(i_row-1) + 1:3),]
      
      p_row <-
        sub_df |> 
        pull(p_expo_resp) |> 
        wrap_plots(nrow = 1) +
        plot_layout(
          axis_titles = "collect_y",
          guides = "collect"
        )
      
      # if last row, keep legend
      if (i == length(iter_elem)) {
        p_row &
          theme(
            legend.position = "bottom"
          )
        
      } else {
        p_row &
          theme(
            legend.position = "none"
          )
      }
      
    }) |> 
  wrap_plots(
    ncol=1
  )




pdf(
  file.path(res_path, "plots", "figures", "exposure-response_filtered_supp.pdf"),
  width = 20 * 0.2 * 3,
  height = 10.6 * 0.46 * length(iter_elem)
)

p_assemb &
  theme(
    plot.title = element_text(size = rel(0.8)),
    axis.title.x = element_text(size = rel(0.8))
  )

dev.off()


# p_assemb <-
#   df_iter_filt$p_expo_resp |> 
#   wrap_plots(
#     nrow=2
#   ) + 
#   plot_layout(
#     guides="collect"
#   ) &
#   theme(
#     legend.position = "bottom"
#   )
# 
# p_assemb +
#   plot_layout(
#     axis_titles = "collect_y"
#   )



## free up memory ------------
df_iter$p_expo_resp <- NULL


### Table -----------------------------------------

tab_expo_resp <-
  pmap(
  df_iter,
  function(pred_expo_resp_path, cf_cond_path, data,
           exposure, disease, model_name, ...) {
    
    print(model_name)
    
    # load data
    pred_expo_resp <- readRDS(pred_expo_resp_path)
    cf_cond <- readRDS(cf_cond_path)
    
    # tidy counterfactual conditions
    cf_cond <-
      cf_cond |> 
      rename(
        centering = eta,
        cf_E = E
      )
    
    # center the data at cf exposure
    pred_expo_resp <-
      pred_expo_resp |> 
      left_join(cf_cond, join_by(L)) |> 
      mutate(
        district = factor(L, labels = levels(data$district))
      ) |> 
      mutate(
        eta = eta / centering
      ) |> 
      select(- centering)
    
    # E values at which to compute IRR
    E_target <-
      data |> 
      group_by(district) |> 
      summarise(
        E_q5 = quantile(E, 0.05),
        E_q95 = quantile(E, 0.95)
      )
    
    # join target E values
    pred_expo_resp <-
      pred_expo_resp |> 
      left_join(E_target, join_by(district))
    
    # decimal places for IRR
    acc <- 0.01
    
    # get target IRRs
    IRR_E_q5 <-
      pred_expo_resp |> 
      group_by(district) |> 
      filter(
        abs(E - E_q5) == min(abs(E - E_q5))
      ) |> 
      mutate(
        IRR_E_q5 = cint_str(eta, 0.025, 0.975, acc, star=TRUE, star_val=1, star_bold=TRUE)
      ) |> 
      select(district, cf_E, IRR_E_q5)
    
    IRR_E_q95 <-
      pred_expo_resp |> 
      group_by(district) |> 
      filter(
        abs(E - E_q95) == min(abs(E - E_q95))
      ) |> 
      mutate(
        IRR_E_q95 = cint_str(eta, 0.025, 0.975, acc, star=TRUE, star_val=1, star_bold=TRUE)
      ) |> 
      select(district, cf_E, IRR_E_q95)
    
    # results table
    res <-
      IRR_E_q5 |> 
      left_join(IRR_E_q95) |> 
      ungroup()
    
    
    # decimal places for exposure
    if (exposure == "greenness") {
      acc2 <- 0.01
    } else {
      acc2 <- 0.1
    }
    
    res |> 
      mutate(exposure, disease) |> 
      relocate(exposure, disease) |> 
      mutate(cf_E = number(cf_E, acc2))
    
  }) |> 
  bind_rows()

tab_expo_resp |> 
  print(n=Inf)

# save table data
write_csv(
  tab_expo_resp,
  file.path(
    res_path, "tables",
    "expo_resp.csv"
  )
)


# format tables

#### CF is min E -------------

# main diseases
tab_wide <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ),
    disease %in% main_dis
  ) |> 
  mutate(
    disease = factor(disease, levels=main_dis),
    exposure = factor(exposure, levels=c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # remove unnecessary cols
  select(- c(cf_E, IRR_E_q5)) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(IRR_E_q95),
    names_sep = "."
  )

gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "IRR at 95th percentile",
    columns = c(pm2p5, total_rainfall, n_raindays)
  ) |> 
  # column labels
  cols_label(
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_pm-rain_main.{extension}"))
  )
)

# the rest

tab_wide <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ),
    disease %in% c(alt_dis, other_dis, unre_dis)
  ) |> 
  mutate(
    disease = factor(disease, levels=c(alt_dis, other_dis, unre_dis)),
    exposure = factor(exposure, levels=c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # remove unnecessary cols
  select(- c(cf_E, IRR_E_q5)) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(IRR_E_q95),
    names_sep = "."
  )

gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "IRR at 95th percentile",
    columns = c(pm2p5, total_rainfall, n_raindays)
  ) |> 
  # column labels
  cols_label(
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_pm-rain_all-else.{extension}"))
  )
)



#### CF is E of min IR -------------

# select temp_max from all temperatures

# main diseases

tab_wide <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "temp_max",
      "utci",
      "greenness"
    ),
    disease %in% main_dis
  ) |> 
  mutate(
    disease = factor(disease, levels=main_dis),
    exposure = factor(exposure, levels=c(
      "temp_max",
      "utci",
      "greenness"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(cf_E, IRR_E_q5, IRR_E_q95),
    names_sep = ".",
    names_vary = "slowest"
  )

gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanners
  tab_spanner(
    label = "Max. temperature (ºC)",
    columns = c(cf_E.temp_max, IRR_E_q5.temp_max, IRR_E_q95.temp_max)
  ) |> 
  tab_spanner(
    label = "UTCI",
    columns = c(cf_E.utci, IRR_E_q5.utci, IRR_E_q95.utci)
  ) |> 
  tab_spanner(
    label = "Greenness (NDVI)",
    columns = c(cf_E.greenness, IRR_E_q5.greenness, IRR_E_q95.greenness)
  ) |> 
  # column labels
  cols_label(
    # cf_E.temp_max = "Counterfactual <br>exposure",
    # cf_E.utci = "Counterfactual <br>exposure",
    # cf_E.greenness = "Counterfactual <br>exposure",
    cf_E.temp_max = "CF. <br>exposure",
    cf_E.utci = "CF. <br>exposure",
    cf_E.greenness = "CF. <br>exposure",
    IRR_E_q5.temp_max = "IRR at <br>5th perc.",
    IRR_E_q5.utci = "IRR at <br>5th perc.",
    IRR_E_q5.greenness = "IRR at <br>5th perc.",
    IRR_E_q95.temp_max = "IRR at <br>95th perc.",
    IRR_E_q95.utci = "IRR at <br>95th perc.",
    IRR_E_q95.greenness = "IRR at <br>95th perc.",
    .fn = md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_temp-utci-ndvi_main.{extension}"))
  )
)

# the rest

tab_wide <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "temp_max",
      "utci",
      "greenness"
    ),
    disease %in% c(alt_dis, other_dis, unre_dis)
  ) |> 
  mutate(
    disease = factor(disease, levels=c(alt_dis, other_dis, unre_dis)),
    exposure = factor(exposure, levels=c(
      "temp_max",
      "utci",
      "greenness"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(cf_E, IRR_E_q5, IRR_E_q95),
    names_sep = ".",
    names_vary = "slowest"
  )

gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanners
  tab_spanner(
    label = "Max. temperature (ºC)",
    columns = c(cf_E.temp_max, IRR_E_q5.temp_max, IRR_E_q95.temp_max)
  ) |> 
  tab_spanner(
    label = "UTCI",
    columns = c(cf_E.utci, IRR_E_q5.utci, IRR_E_q95.utci)
  ) |> 
  tab_spanner(
    label = "Greenness (NDVI)",
    columns = c(cf_E.greenness, IRR_E_q5.greenness, IRR_E_q95.greenness)
  ) |> 
  # column labels
  cols_label(
    # cf_E.temp_max = "Counterfactual <br>exposure",
    # cf_E.utci = "Counterfactual <br>exposure",
    # cf_E.greenness = "Counterfactual <br>exposure",
    cf_E.temp_max = "CF. <br>exposure",
    cf_E.utci = "CF. <br>exposure",
    cf_E.greenness = "CF. <br>exposure",
    IRR_E_q5.temp_max = "IRR at <br>5th perc.",
    IRR_E_q5.utci = "IRR at <br>5th perc.",
    IRR_E_q5.greenness = "IRR at <br>5th perc.",
    IRR_E_q95.temp_max = "IRR at <br>95th perc.",
    IRR_E_q95.utci = "IRR at <br>95th perc.",
    IRR_E_q95.greenness = "IRR at <br>95th perc.",
    .fn = md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_temp-utci-ndvi_all-else.{extension}"))
  )
)

#### all together minus UTCI -------------------

# main diseases

tab_wide1 <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ),
    disease %in% main_dis
  ) |> 
  mutate(
    disease = factor(disease, levels=main_dis),
    exposure = factor(exposure, levels=c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # remove unnecessary cols
  select(- c(cf_E, IRR_E_q5)) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(IRR_E_q95),
    names_sep = "."
  )

tab_wide2 <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "temp_max",
      "greenness"
    ),
    disease %in% main_dis
  ) |> 
  mutate(
    disease = factor(disease, levels=main_dis),
    exposure = factor(exposure, levels=c(
      "temp_max",
      "utci",
      "greenness"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(cf_E, IRR_E_q5, IRR_E_q95),
    names_sep = ".",
    names_vary = "slowest"
  )

tab_wide <-
  tab_wide1 |> 
  left_join(tab_wide2)


gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanners
  tab_spanner(
    label = "PM2.5",
    columns = c(pm2p5)
  ) |> 
  tab_spanner(
    label = "Rainfall",
    columns = c(total_rainfall)
  ) |> 
  tab_spanner(
    label = "No. rain days",
    columns = c(n_raindays)
  ) |> 
  tab_spanner(
    label = "Max. temperature (ºC)",
    columns = c(cf_E.temp_max, IRR_E_q5.temp_max, IRR_E_q95.temp_max)
  ) |> 
  # tab_spanner(
  #   label = "UTCI",
  #   columns = c(cf_E.utci, IRR_E_q5.utci, IRR_E_q95.utci)
  # ) |> 
  tab_spanner(
    label = "Greenness (NDVI)",
    columns = c(cf_E.greenness, IRR_E_q5.greenness, IRR_E_q95.greenness)
  ) |> 
  # column labels
  cols_label(
    pm2p5 = "IRR at <br>95th perc.",
    total_rainfall = "IRR at <br>95th perc.",
    n_raindays = "IRR at <br>95th perc.",
    cf_E.temp_max = "CF. <br>exposure",
    # cf_E.utci = "CF. <br>exposure",
    cf_E.greenness = "CF. <br>exposure",
    IRR_E_q5.temp_max = "IRR at <br>5th perc.",
    # IRR_E_q5.utci = "IRR at <br>5th perc.",
    IRR_E_q5.greenness = "IRR at <br>5th perc.",
    IRR_E_q95.temp_max = "IRR at <br>95th perc.",
    # IRR_E_q95.utci = "IRR at <br>95th perc.",
    IRR_E_q95.greenness = "IRR at <br>95th perc.",
    .fn = md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_selected-expo_main.{extension}"))
  )
)

# the rest of diseases

tab_wide1 <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ),
    disease %in% c(alt_dis, other_dis, unre_dis)
  ) |> 
  mutate(
    disease = factor(disease, levels=c(alt_dis, other_dis, unre_dis)),
    exposure = factor(exposure, levels=c(
      "pm2p5",
      "total_rainfall",
      "n_raindays"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # remove unnecessary cols
  select(- c(cf_E, IRR_E_q5)) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(IRR_E_q95),
    names_sep = "."
  )

tab_wide2 <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "temp_max",
      "greenness"
    ),
    disease %in% c(alt_dis, other_dis, unre_dis)
  ) |> 
  mutate(
    disease = factor(disease, levels=c(alt_dis, other_dis, unre_dis)),
    exposure = factor(exposure, levels=c(
      "temp_max",
      "utci",
      "greenness"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(cf_E, IRR_E_q5, IRR_E_q95),
    names_sep = ".",
    names_vary = "slowest"
  )

tab_wide <-
  tab_wide1 |> 
  left_join(tab_wide2)


gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanners
  tab_spanner(
    label = "PM2.5",
    columns = c(pm2p5)
  ) |> 
  tab_spanner(
    label = "Rainfall",
    columns = c(total_rainfall)
  ) |> 
  tab_spanner(
    label = "No. rain days",
    columns = c(n_raindays)
  ) |> 
  tab_spanner(
    label = "Max. temperature (ºC)",
    columns = c(cf_E.temp_max, IRR_E_q5.temp_max, IRR_E_q95.temp_max)
  ) |> 
  # tab_spanner(
  #   label = "UTCI",
  #   columns = c(cf_E.utci, IRR_E_q5.utci, IRR_E_q95.utci)
  # ) |> 
  tab_spanner(
    label = "Greenness (NDVI)",
    columns = c(cf_E.greenness, IRR_E_q5.greenness, IRR_E_q95.greenness)
  ) |> 
  # column labels
  cols_label(
    pm2p5 = "IRR at <br>95th perc.",
    total_rainfall = "IRR at <br>95th perc.",
    n_raindays = "IRR at <br>95th perc.",
    cf_E.temp_max = "CF. <br>exposure",
    # cf_E.utci = "CF. <br>exposure",
    cf_E.greenness = "CF. <br>exposure",
    IRR_E_q5.temp_max = "IRR at <br>5th perc.",
    # IRR_E_q5.utci = "IRR at <br>5th perc.",
    IRR_E_q5.greenness = "IRR at <br>5th perc.",
    IRR_E_q95.temp_max = "IRR at <br>95th perc.",
    # IRR_E_q95.utci = "IRR at <br>95th perc.",
    IRR_E_q95.greenness = "IRR at <br>95th perc.",
    .fn = md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_selected-expo_all-else.{extension}"))
  )
)

#### UTCI -----------------------------


tab_wide <-
  tab_expo_resp |> 
  filter(
    exposure %in% c(
      "utci"
    ),
    disease %in% c(main_dis, alt_dis, other_dis, unre_dis)
  ) |> 
  mutate(
    disease = factor(disease, levels=c(main_dis, alt_dis, other_dis, unre_dis)),
    exposure = factor(exposure, levels=c(
      "utci"
    ))
  ) |> 
  arrange(disease, exposure) |> 
  relocate(disease, exposure) |> 
  # to wide
  pivot_wider(
    names_from = exposure,
    values_from = c(cf_E, IRR_E_q5, IRR_E_q95),
    names_sep = ".",
    names_vary = "slowest"
  )

gtab <-
  tab_wide |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanners
  tab_spanner(
    label = "UTCI",
    columns = c(cf_E.utci, IRR_E_q5.utci, IRR_E_q95.utci)
  ) |> 
  # column labels
  cols_label(
    cf_E.utci = "CF. <br>exposure",
    IRR_E_q5.utci = "IRR at <br>5th perc.",
    IRR_E_q95.utci = "IRR at <br>95th perc.",
    .fn = md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 1"
  )


gtab


walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("expo_resp_utci_all-diseases.{extension}"))
  )
)

    
# Aggregated AFs -------------------------------------------------------------------------

# combine all AFs in one dataframe
df_af <-
  pmap(
    df_iter,
    function(pred_cf_agg_path, exposure, disease, data, label, ...) {
      
      print(pred_cf_agg_path)
      
      # load data
      pred_cf_agg <- readRDS(pred_cf_agg_path)
      
      # tidy
      pred_cf_agg |> 
        mutate(
          district = factor(L, labels=levels(data$district)),
          exposure,
          label,
          disease
        ) |> 
        select(- L) |> 
        relocate(exposure, label, disease, district) |> 
        rename(n_cases = D)
    }
  ) |> 
  bind_rows()


# sort exposure labels
sorted_lab <-
  df_af |> 
  arrange(exposure) |> 
  pull(label) |> 
  unique()

df_af <-
  df_af |> 
  mutate(
    label = factor(label, levels=sorted_lab)
  )

## compose figures ----------------------

# main diseases
df_af |> 
  filter(
    exposure %in% main_env,
    disease %in% main_dis
  ) |> 
  ggplot() +
  geom_vline(xintercept = 0) +
  stat_slab(
    aes(
      xdist=AF, y=disease, color=district,
      fill=after_scale(alpha(color, 0.1))
    ),
    normalize = "groups", scale=0.6
  ) |> 
  ggblend::blend("darken") +
  stat_pointinterval(
    aes(xdist=AF, y=disease, color=district),
    .width=c(0.95),
    # interval_size_range = c(1.2, 2.5),
    position = position_dodgejust(
      preserve = "single",
      width=0.2
    ),
    justification = 0.4
  ) +
  scale_x_continuous(
    labels = \(x) scales::percent(x, suffix=""),
    breaks = c(-0.2, 0, 0.2, 0.4, 0.6)
    
  ) +
  scale_y_discrete(limits = rev) +
  facet_grid(
    cols = vars(label),
    scales = "free_x"
  ) +
  coord_cartesian(
    xlim = c(-0.2, 0.7)
  ) +
  labs(
    y = "",
    x = "Attributable fraction (%)",
    color = "District"
  ) +
  theme(
    legend.position = "bottom"
  )


ggsave(
  file.path(res_path, "plots", "figures", "attributable-fraction_main.pdf"),
  width = 20 * 0.18 * length(main_env),
  height = 10.6 * 0.11 * length(main_dis),
  device = cairo_pdf
)

# the rest
df_af |> 
  filter(
    exposure %in% main_env,
    disease %in% c(alt_dis, other_dis, unre_dis)
    # ! disease %in% main_dis
  ) |> 
  ggplot() +
  geom_vline(xintercept = 0) +
  stat_slab(
    aes(
      xdist=AF, y=disease, color=district,
      fill=after_scale(alpha(color, 0.1))
      ),
    normalize = "groups", scale=0.6
  ) +
  stat_pointinterval(
    aes(xdist=AF, y=disease, color=district),
    .width=c(0.95),
    # interval_size_range = c(1.2, 2.5),
    position = position_dodgejust(
      preserve = "single",
      width=0.2
    ),
    justification = 0.4
  ) +
  scale_x_continuous(
    labels = \(x) scales::percent(x, suffix=""),
    breaks = c(-0.2, 0, 0.2, 0.4, 0.6),
  ) +
  scale_y_discrete(limits = rev) +
  facet_grid(
    cols = vars(label),
  ) +
  coord_cartesian(
    xlim = c(-0.2, 0.7),
  ) +
  labs(
    y = "",
    x = "Attributable fraction (%)",
    color = "District"
  ) +
  theme(
    legend.position = "bottom"
  )


ggsave(
  file.path(res_path, "plots", "figures", "attributable-fraction_all-else.pdf"),
  width = 20 * 0.18 * length(main_env),
  height = 10.6 * 0.08 * length(
    c(alt_dis, other_dis, unre_dis)
    )
)


# filtered cases

df_af_filt <-
  df_af |> 
  filter(
    g("{exposure}.{disease}") %in% main_cases$filt_id
  ) |> 
  mutate(
    label = fct_relevel(label, c(
      "PM2.5",
      "Greenness",
      "No. rain days"
    ))
  ) |> 
  arrange(label) |> 
  mutate(
    disease_short = str_replace(disease, "^(\\S+\\s+\\S+)\\s+", "\\1\n")
  )

# sort disease labels
sorted_lab <-
  df_af_filt |> 
  arrange(disease) |> 
  pull(disease_short) |> 
  unique()

df_af_filt <-
  df_af_filt |> 
  mutate(
    disease_short = factor(disease_short, levels=sorted_lab)
  )

df_af_filt |> 
  ggplot() +
  geom_vline(xintercept = 0) +
  stat_slab(
    aes(
      xdist=AF, color=district, y=disease_short,
      fill=after_scale(alpha(color, 0.1))
    ),
    normalize = "groups", scale=0.6
  ) +
  stat_pointinterval(
    aes(xdist=AF, y=disease_short, color=district),
    .width=c(0.95),
    position = position_dodgejust(
      preserve = "single",
      width=0.2
    ),
    justification = 0.4
  ) +
  scale_x_continuous(
    labels = \(x) scales::percent(x, suffix=""),
    breaks = c(-0.2, 0, 0.2, 0.4, 0.6),
  ) +
  scale_y_discrete(limits = rev) +
  facet_wrap(
    vars(label),
    scales = "free_y"
  ) +
  coord_cartesian(
    xlim = c(-0.2, 0.7),
  ) +
  labs(
    y = "",
    x = "Attributable fraction (%)",
    color = "District"
  ) +
  theme(
    legend.position = "bottom"
  )



ggsave(
  file.path(res_path, "plots", "figures", "attributable-fraction_filtered_main.pdf"),
  width = 20 * 0.3 * 3,
  height = 10.6 * 0.2 * 3,
  device = cairo_pdf
)


## Table -----------------------------------------

tab_af <-
  df_af |> 
  mutate(
    AF_perc = cint_str(AF*100, 0.025, 0.975, 0.1, star=TRUE, star_val=0, star_bold=TRUE),
    AN = cint_str(AN, 0.025, 0.975, 1, star=TRUE, star_val=0, star_bold=TRUE)
  ) |> 
  select(- c(AF, label)) |> 
  relocate(AF_perc, .before = AN)

# save table data
write_csv(
  tab_af,
  file.path(
    res_path, "tables",
    "attributable-fraction.csv"
  )
)


# format tables

# only AF ------------------------.

tab_wide <-
  tab_af |> 
    filter(
      exposure %in% main_env
    ) |> 
    select( - AN) |> 
    pivot_wider(
      names_from = exposure,
      values_from = c(AF_perc),
      names_sep = "."
    ) |> 
  mutate(n_cases = number(n_cases, 1)) |> 
  select(- n_cases)

# main diseases

gtab <-
  tab_wide |> 
  filter(disease %in% main_dis) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "Attributable fraction (%)",
    columns = c(pm2p5:greenness)
  ) |> 
  # column labels
  cols_label(
    # n_cases = "Total cases",
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    temp_max = "Max. temperature",
    utci = "UTCI",
    greenness = "Greenness",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )


gtab

walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("attributable-fraction_AF_main.{extension}"))
  )
)

# the rest

gtab <-
  tab_wide |> 
  filter(! disease %in% main_dis) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "Attributable fraction (%)",
    columns = c(pm2p5:greenness)
  ) |> 
  # column labels
  cols_label(
    # n_cases = "Total cases",
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    temp_max = "Max. temperature",
    utci = "UTCI",
    greenness = "Greenness",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )


gtab

walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("attributable-fraction_AF_all-else.{extension}"))
  )
)


# only AN ------------------------.

tab_wide <-
  tab_af |> 
  filter(
    exposure %in% main_env
  ) |> 
  select( - AF_perc) |> 
  pivot_wider(
    names_from = exposure,
    values_from = c(AN),
    names_sep = "."
  ) |> 
  mutate(n_cases = number(n_cases, 1))

# main diseases

gtab <-
  tab_wide |> 
  filter(disease %in% main_dis) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "Attributable number",
    columns = c(pm2p5:greenness)
  ) |> 
  # column labels
  cols_label(
    n_cases = "Total cases",
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    temp_max = "Max. temperature",
    utci = "UTCI",
    greenness = "Greenness",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )


gtab

walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("attributable-fraction_AN_main.{extension}"))
  )
)

# the rest

gtab <-
  tab_wide |> 
  filter(! disease %in% main_dis) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  # column spanner
  tab_spanner(
    label = "Attributable number",
    columns = c(pm2p5:greenness)
  ) |> 
  # column labels
  cols_label(
    n_cases = "Total cases",
    pm2p5 = "PM2.5",
    total_rainfall = "Rainfall",
    n_raindays = "No. rain days",
    temp_max = "Max. temperature",
    utci = "UTCI",
    greenness = "Greenness",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )


gtab

walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("attributable-fraction_AN_all-else.{extension}"))
  )
)




## free up memory --------------------------------

rm(df_af)

# Model prediction explanation -----------------------------------------------------------

pwalk(
  df_iter,
  function(pred_explain_path, data, exposure, disease, disease_print, ...) {
    
    print(g("{exposure}; {disease}"))
    
    # load pred
    pred_explain <- readRDS(pred_explain_path)
    
    plot_df <-
      pred_explain |> 
      left_join(data)
    
    # fit
    p1 <-
      plot_df |> 
      ggplot() +
      facet_grid(
        cols = vars(district)
      ) +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)),
        linewidth=0.5, color="gray70"
      ) +
      geom_point(
        aes(x=date, y=case_rate_1e5), size=2
      ) +
      # fit line
      stat_lineribbon(
        aes(x=date, ydist=pred_fit),
        .width=c(0.95, 0),
        fill = alpha("black", 0.3)
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        x = "Date (monthly)",
        y = "Incidence rate\nper 100,000 people",
        title = g("{exposure}; {disease}")
      )
    
    # contributions to log-IR
    line_labs_cols <- c(
      "Trend"="black",
      "Seasonality"="royalblue",
      "Exposure contribution"="firebrick"
    )
    
    p2 <-
      plot_df |> 
      ggplot() +
      facet_grid(
        cols = vars(district)
      ) +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)),
        linewidth=0.5, color="gray70"
      ) +
      geom_hline(yintercept=0, linetype=2) +
      # trend
      stat_lineribbon(
        aes(x=date, ydist=eta_x,
            color=names(line_labs_cols)[1],
        ),
        .width=c(0),
      ) +
      # season
      stat_lineribbon(
        aes(x=date, ydist=eta_s,
            color=names(line_labs_cols)[2],
        ),
        .width=c(0),
      ) +
      # exposure contribution
      stat_lineribbon(
        aes(x=date, ydist=eta_E,
            color=names(line_labs_cols)[3],
        ),
        .width=c(0),
      ) +
      scale_color_manual(
        aesthetics = c("colour"),
        name = "",
        values = line_labs_cols,
        limits = names(line_labs_cols)
      ) +
      scale_fill_discrete(guide = "none") +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        x = "Date (monthly)",
        y = "log-IR\nper 100,000 people"
      ) +
      theme(
        legend.position = "bottom",
      )
    
    # exposure
    p3 <-
      plot_df |> 
      ggplot() +
      facet_grid(
        cols = vars(district)
      ) +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)),
        linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=E),
        linewidth=1
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        x = "Date (monthly)"
      )
    
    # compose figure
    p1 / p2 / p3 +
      plot_layout(
        axis_titles = "collect"
      )
    
    ggsave(
      file.path(
        res_path, "plots", "interpretation-help", "associations",
        g("prediction-explanation_{exposure}_{disease_print}.pdf")
      ),
      width = 20 * 0.9,
      height = 10.6 * 0.45 * 3,
      device = cairo_pdf
    )
    
  }
)


# Counterfactual IRs ---------------------------------------------------------------------

pwalk(
  df_iter,
  function(pred_cf_path, data, exposure, disease, disease_print,
           cf_cond_path, ...) {
    print(pred_cf_path)
    
    # load data
    pred_cf <- readRDS(pred_cf_path)
    
    plot_df <-
      pred_cf |> 
      left_join(data)
    
    
    # counterfactual conditions
    cf_cond <- readRDS(cf_cond_path)
    # tidy counterfactual conditions
    cf_cond <-
      cf_cond |> 
      mutate(
        district = factor(L, labels = levels(data$district))
      ) |> 
      arrange(district)
    # prepare annotation
    annot <-
      g("E* {cf_cond$district}: {round(cf_cond$E, 1) |> format(nsmall=1)}") |>
      paste0(collapse="\n")
    
    line_labs_cols <- c(
      "IR for observed E"="firebrick",
      "IR for counterfactual E"="royalblue"
    )
    
    # counterfactual IRs
    p1 <-
      plot_df |> 
      ggplot() +
      facet_grid(cols = vars(district)) +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)),
        linewidth=0.5, color="gray70"
      ) +
      geom_point(
        aes(x=date, y=case_rate_1e5), size=2
      ) +
      # counterfactual exposure
      stat_lineribbon(
        aes(x=date, ydist=eta_cf,
            color=names(line_labs_cols)[2],
            fill = after_scale(alpha(color, 0.3)),
        ),
        .width=c(0.95, 0),
      ) +
      # observed exposure
      stat_lineribbon(
        aes(x=date, ydist=eta,
            color=names(line_labs_cols)[1],
            fill = after_scale(alpha(color, 0.3)),
        ),
        .width=c(0.95, 0),
      ) +
      scale_color_manual(
        aesthetics = c("colour", "fill"),
        name = "",
        values = line_labs_cols,
        limits = names(line_labs_cols)
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        x = "Date (monthly)",
        y = "Incidence rate\nper 100,000 people",
        title = g("{exposure}; {disease}"),
        caption = annot
      ) +
      theme(
        legend.position = "bottom"
      )
    
    # observed exposures
    p2 <-
      plot_df |> 
      ggplot() +
      facet_grid(
        cols = vars(district)
      ) +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)),
        linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date, y=E),
        linewidth=1
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        x = "Date (monthly)"
      )
    
    # compose figure
    p1 / p2 +
      plot_layout(
        axis_titles = "collect",
        heights = c(53, 47)
      )
    
    ggsave(
      file.path(
        res_path, "plots", "interpretation-help", "associations",
        g("cf-IR_{exposure}_{disease_print}.pdf")
      ),
      width = 20 * 0.9,
      height = 10.6 * 0.55 * 2,
      device = cairo_pdf
    )
    
  }
)

# Descriptive tables ---------------------------------------------------------------------

## Env --------------------------------------------

tab_env_desc <-
  pmap(
  filter(df_iter, disease == first(disease)),
  function(exposure, data, cf_cond_path, label, units, ...) {
    
    print(exposure)
    
    # load cf_cond
    cf_cond <- readRDS(cf_cond_path)
    
    # decimal places
    if (exposure == "greenness") {
      acc <- 0.01
    } else {
      acc <- 0.1
    }
    
    # tidy counterfactual conditions
    cf_cond <-
      cf_cond |> 
      mutate(
        district = factor(L, labels = levels(data$district))
      ) |> 
      rename(
        cf_E = E
      ) |> 
      select(- c(L, eta))
    
    # counterfactual string
    if (exposure %in% c("pm2p5", "total_rainfall", "n_raindays")) {
      cf_cond <-
        cf_cond |> 
        mutate(
          cf_E = g("Min. exposure: {number(cf_E, acc)}")
        )
      
    } else {
      cf_cond <-
        cf_cond |> 
        mutate(
          cf_E = "Exposure of min. IR"
        )
    }
    
    # summarise data
    data_summ <-
      data |> 
      group_by(district) |> 
      summarise(
        E_med = median(E) |> number(acc),
        E_range = g(
          "{
          range(E)[1] |> number(acc)
          }, {
          range(E)[2] |> number(acc)
          }"
        ),
        E_q5_q95 = g(
          "{
          quantile(E, 0.05) |> number(acc)
          }, {
          quantile(E, 0.95) |> number(acc)
          }"
        )
      )
    
    # final table
    data_summ |> 
      left_join(cf_cond) |> 
      mutate(exposure, label, units) |> 
      relocate(exposure, label, units)
  }) |> 
  bind_rows()
    
    
tab_env_desc

# save table data
write_csv(
  tab_env_desc,
  file.path(
    res_path, "tables",
    "env_descriptive.csv"
  )
)

# format table
gtab <-
  tab_env_desc |> 
  # sort
  mutate(
    exposure = factor(exposure, levels = all_env)
  ) |> 
  arrange(exposure) |> 
  mutate(
    exposure = g("{label} {units}")
  ) |> 
  select(- c(label, units)) |> 
  # make table
  gt(
    groupname_col = "exposure",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Exposure") |> 
  cols_label(
    E_med = "Median",
    E_range = "Range",
    E_q5_q95 = "5th, 95th <br>percentiles",
    cf_E = "Counterfactual <br>exposure",
    .fn=md
  )

gtab

# save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("env_descriptive.{extension}"))
  )
)


# Trend description ----------------------------------------------------------------------

## Environmental ------------------------------------------------

# load analysis setup
df_env <- readRDS(file.path(
  res_path, "R_output", "environmental_descriptive-analysis_setup.Rds"
))

### get base plots -------------------

df_env$p_trend <-
  pmap(
  # df_env[5,],
  df_env,
  function(pred_trend_path, data, label, units, ...) {
    
    print(pred_trend_path)
    
    # load fit
    pred_trend <- readRDS(pred_trend_path)
    
    pred_trend |> 
      left_join(data) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_line(
        aes(x=date,
            y=E,
            color=district),
        linewidth=1,
        # alpha=0.6,
        linetype="31"
      ) +
      # geom_point(
      #   aes(x=date,
      #       y=E,
      #       color=district),
      #   size=2,
      #   # alpha=0.6
      # ) +
      stat_lineribbon(
        aes(
          x=date, ydist=pred_trend,
          # color = district,
          color = stage(district, after_scale = alpha(color, 0.7)),
          fill = after_scale(alpha(color, 0.3))
        ),
        .width=c(0),
        linewidth=2.5,
        # linetype="31"
        linetype=1,
      ) +
      # ggblend::blend("darken") +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = TeX(glue("{label} {units}"))
      ) +
      theme(
        legend.position = "bottom"
      )
  }
)


### compose figures ------------------

pdf(
  file.path(res_path, "plots", "figures", "environmental-trends.pdf"),
  width = 20 * 0.6,
  height = 10.6 * 0.22 * length(all_env)
)
  
df_env |> 
  mutate(
    exposure = factor(exposure, levels = all_env)
  ) |> 
  arrange(exposure) |> 
  pull(p_trend) |> 
  wrap_plots(ncol=1) +
  plot_layout(
    axis_titles = "collect",
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.7)),
    axis.text = element_text(size = rel(0.7)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.8))
  )

dev.off()


### free up memory ------------
df_env$p_trend <- NULL


### trend explanation -------------------------

pwalk(
  df_env,
  function(pred_trend_path, data, label, units, exposure, ...) {
    
    print(pred_trend_path)
    
    # load fit
    pred_trend <- readRDS(pred_trend_path)
    
    # fit plot
    p1 <-
      pred_trend |> 
      left_join(data) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_point(
        aes(x=date, y=E, color=district),
        size=2
      ) +
      stat_lineribbon(
        aes(
          x=date, ydist=pred_fit,
          color=district, fill=after_scale(alpha(color, 0.3))
          ),
        .width=c(0, 0.95)
      ) |> 
      ggblend::blend("darken") +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = TeX(glue("{label} {units}"))
      ) +
      theme(
        legend.position = "bottom"
      )
      
    # explanation plot
    p2 <-
      pred_trend |> 
      left_join(data) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_hline(
        yintercept = 0, linetype=2
      ) +
      stat_lineribbon(
        aes(
          x=date, ydist=eta_x,
          # color=district,
          color = stage(district, after_scale = alpha(color, 0.8)),
          fill=after_scale(alpha(color, 0.3))
        ),
        .width=c(0),
        linewidth=2.5,
        show.legend = FALSE
      ) |> 
      ggblend::blend("darken") +
      stat_lineribbon(
        aes(
          x=date, ydist=eta_s,
          color=district, fill=after_scale(alpha(color, 0.3))
        ),
        .width=c(0),
        linetype="32",
        linewidth=2,
        show.legend = FALSE
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "Partial contribution\nto exposure"
      ) +
      theme(
        legend.position = "none"
      )
    
    
    (p1 / p2) +
      plot_layout(
        guides = "collect",
        axis_titles = "collect"
      ) &
      theme(
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8))
      )
    
    ggsave(
      file.path(
        res_path, "plots", "interpretation-help", "trends",
        g("descriptive_env_{exposure}_trend_help.pdf")
      ),
      width = 20 * 0.7,
      height = 10.6 * 0.35 * 2,
      device = cairo_pdf
    )
    
  }
)

### Table -----------------------------------------

tab_env_trend <-
  pmap(
  df_env,
  function(pred_trend_summ_path, exposure, data, label, units, ...) {
    print(exposure)
    
    pred_trend_summ <- readRDS(pred_trend_summ_path)
    
    # decimal places
    if (exposure == "greenness") {
      acc <- 0.01
    } else {
      acc <- 0.1
    }
    
    # get data summary
    # data_summ <-
    #   data |> 
    #   group_by(district) |> 
    #   summarise(
    #     q50 = median(E),
    #     q5 = quantile(E, 0.05),
    #     q95 = quantile(E, 0.95)
    #   ) |> 
    #   mutate_at(
    #     vars(q50:q95), \(x) number(x, acc)
    #   ) |> 
    #   mutate(
    #     q50_q5_q95 = g("{q50} ({q5}, {q95})")
    #   ) |> 
    #   select(- c(q50:q95))
    
    # get intervals from rvars
    pred_trend_summ <-
      pred_trend_summ |> 
      mutate(
        trend_start = cint_str(trend_start, 0.025, 0.975, acc),
        trend_end = cint_str(trend_end, 0.025, 0.975, acc),
        trend_change = cint_str(trend_change, 0.025, 0.975, acc*0.1, star=TRUE,
                                star_val=0, star_bold=TRUE),
        an_trend_change = cint_str(an_trend_change, 0.025, 0.975, acc*0.1, star=TRUE,
                                   star_val=0, star_bold=TRUE)
      )
    
    # table
    pred_trend_summ |> 
      # left_join(data_summ) |> 
      # relocate(q50_q5_q95, .after = district) |> 
      mutate(exposure, label, units) |> 
      relocate(exposure, label, units)
  }
) |> 
  bind_rows()

tab_env_trend


# save table data
write_csv(
  tab_env_trend,
  file.path(
    res_path, "tables",
    "env_trends.csv"
  )
)

# format table
gtab <-
  tab_env_trend |> 
  # sort
  mutate(
    exposure = factor(exposure, levels = all_env)
  ) |> 
  arrange(exposure) |> 
  mutate(
    exposure = g("{label} {units}")
  ) |> 
  select(- c(label, units)) |> 
  # make table
  gt(
    groupname_col = "exposure",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Exposure") |> 
  cols_label(
    # q50_q5_q95 = "Median <br>(5th, 95th percentile)",
    trend_start = "Trend start level",
    trend_end = "Trend end level",
    trend_change = "Trend change <br>(end - start)",
    an_trend_change = "Annual average <br>trend change",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )

gtab

# save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("env_trends.{extension}"))
  )
)


## Disease ----------------------------------------------------

# load analysis setup
df_dis <- readRDS(file.path(
  res_path, "R_output", "disease_descriptive-analysis_setup.Rds"
))

### get base plots -------------------


df_dis$p_trend <-
  pmap(
    # df_dis[1,],
    df_dis,
    function(pred_trend_path, data, disease, ...) {
      
      print(pred_trend_path)
      
      # load fit
      pred_trend <- readRDS(pred_trend_path)
      
      pred_trend |> 
        left_join(data) |> 
        ggplot() +
        geom_vline(
          aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
        ) +
        geom_vline(
          aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
        ) +
        geom_line(
          aes(x=date,
              y=case_rate_1e5,
              color=district),
          linewidth=1,
          linetype="31"
        ) +
        stat_lineribbon(
          aes(
            x=date, ydist=pred_trend,
            color = stage(district, after_scale = alpha(color, 0.7)),
            fill = after_scale(alpha(color, 0.3))
          ),
          .width=c(0),
          linewidth=2.5,
          linetype=1,
        ) +
        # ggblend::blend("darken") +
        scale_x_date(
          date_breaks="year", date_labels = "%Y"
        ) +
        labs(
          color = "District",
          x = "Date (monthly)",
          y = "Incidence rate\nper 100,000 people",
          title = disease
        ) +
        theme(
          legend.position = "bottom",
          plot.title = element_text(face="bold")
        )
    }
  )




### compose figures ------------------

# main diseases
pdf(
  file.path(res_path, "plots", "figures", "disease-trends_main.pdf"),
  width = 20 * 0.6,
  height = 10.6 * 0.25 * length(main_dis)
)

df_dis |> 
  filter(
    disease %in% main_dis
  ) |> 
  mutate(disease = factor(disease, levels=main_dis)) |> 
  arrange(disease) |> 
  pull(p_trend) |> 
  wrap_plots(ncol=1) +
  plot_layout(
    axis_titles = "collect_x",
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.6)),
    axis.text = element_text(size = rel(0.65)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.8)),
    plot.title = element_text(size = rel(0.7), face="bold")
  )

dev.off()


# alternative
pdf(
  file.path(res_path, "plots", "figures", "disease-trends_alt.pdf"),
  width = 20 * 0.6,
  height = 10.6 * 0.25 * length(alt_dis)
)

df_dis |> 
  filter(
    disease %in% alt_dis
  ) |> 
  mutate(disease = factor(disease, levels=alt_dis)) |> 
  arrange(disease) |> 
  pull(p_trend) |> 
  wrap_plots(ncol=1) +
  plot_layout(
    axis_titles = "collect_x",
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.6)),
    axis.text = element_text(size = rel(0.65)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.8)),
    plot.title = element_text(size = rel(0.7), face="bold")
  )

dev.off()


# other
pdf(
  file.path(res_path, "plots", "figures", "disease-trends_other.pdf"),
  width = 20 * 0.6,
  height = 10.6 * 0.25 * length(other_dis)
)

df_dis |> 
  filter(
    disease %in% other_dis
  ) |> 
  mutate(disease = factor(disease, levels=other_dis)) |> 
  arrange(disease) |> 
  pull(p_trend) |> 
  wrap_plots(ncol=1) +
  plot_layout(
    axis_titles = "collect_x",
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.6)),
    axis.text = element_text(size = rel(0.65)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.8)),
    plot.title = element_text(size = rel(0.7), face="bold")
  )

dev.off()


# unreliable
pdf(
  file.path(res_path, "plots", "figures", "disease-trends_unreliable.pdf"),
  width = 20 * 0.6,
  height = 10.6 * 0.25 * length(unre_dis)
)

df_dis |> 
  filter(
    disease %in% unre_dis
  ) |> 
  mutate(disease = factor(disease, levels=unre_dis)) |> 
  arrange(disease) |> 
  pull(p_trend) |> 
  wrap_plots(ncol=1) +
  plot_layout(
    axis_titles = "collect_x",
    guides = "collect"
  ) &
  theme(
    legend.position = "bottom",
    axis.title.x = element_text(size = rel(0.8)),
    axis.title.y = element_text(size = rel(0.6)),
    axis.text = element_text(size = rel(0.65)),
    legend.title = element_text(size = rel(0.8)),
    legend.text = element_text(size = rel(0.8)),
    plot.title = element_text(size = rel(0.7), face="bold")
  )

dev.off()

### free up memory ------------
df_dis$p_trend <- NULL

### trend explanation -------------------------


pwalk(
  df_dis,
  function(pred_trend_path, data, disease, disease_print, ...) {
    
    print(pred_trend_path)
    
    # load fit
    pred_trend <- readRDS(pred_trend_path)
    
    
    # fit plot
    p1 <-
      pred_trend |> 
      left_join(data) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_point(
        aes(x=date, y=case_rate_1e5, color=district),
        size=2
      ) +
      stat_lineribbon(
        aes(
          x=date, ydist=pred_fit,
          color=district, fill=after_scale(alpha(color, 0.3))
        ),
        .width=c(0, 0.95)
      ) |> 
      ggblend::blend("darken") +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "Incidence rate\nper 100,000 people",
        title = disease
      ) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face="bold")
      )
    
    # explanation plot
    p2 <-
      pred_trend |> 
      left_join(data) |> 
      ggplot() +
      geom_vline(
        aes(xintercept = floor_date(date, "year")), linewidth=0.5, color="gray70"
      ) +
      geom_vline(
        aes(xintercept = max(floor_date(date, "year")) + years(1)), linewidth=0.5, color="gray70"
      ) +
      geom_hline(
        yintercept = 0, linetype=2
      ) +
      stat_lineribbon(
        aes(
          x=date, ydist=eta_x,
          # color=district,
          color = stage(district, after_scale = alpha(color, 0.8)),
          fill=after_scale(alpha(color, 0.3))
        ),
        .width=c(0),
        linewidth=2.5,
        show.legend = FALSE
      ) |> 
      ggblend::blend("darken") +
      stat_lineribbon(
        aes(
          x=date, ydist=eta_s,
          color=district, fill=after_scale(alpha(color, 0.3))
        ),
        .width=c(0),
        linetype="32",
        linewidth=2,
        show.legend = FALSE
      ) +
      scale_x_date(
        date_breaks="year", date_labels = "%Y"
      ) +
      labs(
        color = "District",
        x = "Date (monthly)",
        y = "Partial contribution\nto log rate"
      ) +
      theme(
        legend.position = "none"
      )
    
    
    (p1 / p2) +
      plot_layout(
        guides = "collect",
        axis_titles = "collect"
      ) &
      theme(
        legend.position = "bottom",
        axis.title.x = element_text(size = rel(0.8)),
        axis.title.y = element_text(size = rel(0.8)),
        axis.text = element_text(size = rel(0.8)),
        legend.title = element_text(size = rel(0.8)),
        legend.text = element_text(size = rel(0.8)),
        plot.title = element_text(size = rel(0.8))
      )
    
    ggsave(
      file.path(
        res_path, "plots", "interpretation-help", "trends",
        g("descriptive_dis_{disease_print}_trend_help.pdf")
      ),
      width = 20 * 0.7,
      height = 10.6 * 0.37 * 2,
      device = cairo_pdf
    )
    
  }
)

### Table -----------------------------------------

tab_dis_trend <-
  pmap(
    df_dis,
    function(pred_trend_summ_path, data, disease, ...) {
      print(disease)
      
      
      pred_trend_summ <- readRDS(pred_trend_summ_path)
      
      # decimal places
      acc <- 1
      
      # get data summary
      data_summ <-
        data |> 
        group_by(district) |> 
        summarise(
          q50 = median(case_rate_1e5),
          q5 = quantile(case_rate_1e5, 0.05),
          q95 = quantile(case_rate_1e5, 0.95)
        ) |> 
        mutate_at(
          vars(q50:q95), \(x) number(x, acc)
        ) |> 
        mutate(
          q50_q5_q95 = g("{q50} ({q5}, {q95})")
        ) |> 
        select(- c(q50:q95))
      
      # get intervals from rvars
      pred_trend_summ <-
        pred_trend_summ |> 
        mutate(
          trend_start = cint_str(trend_start, 0.025, 0.975, acc),
          trend_end = cint_str(trend_end, 0.025, 0.975, acc),
          trend_change = cint_str(trend_change, 0.025, 0.975, acc, star=TRUE,
                                  star_val=0, star_bold=TRUE),
          an_trend_change = cint_str(an_trend_change, 0.025, 0.975, acc*0.1, star=TRUE,
                                     star_val=0, star_bold=TRUE)
        )
      
      # table
      pred_trend_summ |> 
        left_join(data_summ) |> 
        relocate(q50_q5_q95, .after = district) |> 
        mutate(disease) |> 
        relocate(disease)
    }
  ) |> 
  bind_rows()

tab_dis_trend


# save table data
write_csv(
  tab_dis_trend,
  file.path(
    res_path, "tables",
    "dis_trends.csv"
  )
)

# format table

# main diseases
gtab <-
  tab_dis_trend |> 
  filter(disease %in% main_dis) |> 
  mutate(disease = factor(disease, levels=main_dis)) |> 
  arrange(disease) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  cols_label(
    q50_q5_q95 = "Median <br>(5th, 95th percentile)",
    trend_start = "Trend start level",
    trend_end = "Trend end level",
    trend_change = "Trend change <br>(end - start)",
    an_trend_change = "Annual average <br>trend change",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )

gtab

walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("dis_trends_main.{extension}"))
  )
)

# the rest
gtab <-
  tab_dis_trend |> 
  filter(disease %in% c(alt_dis, other_dis, unre_dis)) |> 
  mutate(disease = factor(disease,
                          levels=c(alt_dis, other_dis, unre_dis))) |> 
  arrange(disease) |> 
  gt(
    groupname_col = "disease",
    rowname_col = "district",
    process_md = TRUE
  ) |> 
  fmt_markdown() |> 
  cols_align(
    "right"
  ) |> 
  tab_stubhead("Disease") |> 
  cols_label(
    q50_q5_q95 = "Median <br>(5th, 95th percentile)",
    trend_start = "Trend start level",
    trend_end = "Trend end level",
    trend_change = "Trend change <br>(end - start)",
    an_trend_change = "Annual average <br>trend change",
    .fn=md
  ) |> 
  tab_footnote(
    footnote = "*: 95% credibility interval excluding 0"
  )

gtab

# save
walk(
  c("html", "tex", "docx"),
  \(extension) gtsave(
    gtab,
    file.path(res_path, "tables",
              g("dis_trends_all-else.{extension}"))
  )
)

