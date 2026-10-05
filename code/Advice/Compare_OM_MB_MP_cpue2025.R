# ============================================================
# Script: Compare_OM_MP_CPUE2025.R
#
# Purpose:
#   Compare OM trajectories with MP trajectories using
#   B/Bmsy and F/Fmsy indicators up to 2025.
#
# Inputs:
#   - Aggregated FLBEIA outputs
#   - RefPts_FLBRP_format.csv
#   - SA_mpb.csv
#
# Outputs:
#   - OM_MP_MPnew_2025cpue_bio.png
#
# Author: AZTI
# ============================================================


library(ggplot2)
library(patchwork)
library(here)

proj_dir = here::here()
setwd(proj_dir)

# Sharepoint path:
source(file.path('code','Others','AuxiliaryFunctions.R'))
source('sharepoint_path.R')
setwd(shrpoint_path)


#  path:
dir_in <-  file.path("FLoutput","Summary","ModelBased_25var")
dir_out <- dir_in

#  scenarios:
sc_nm_all <- c("Ftg0.8_25%","Ftg1_25%",
               "Ftg0.8_25%_R-","Ftg0.8_25%_R+","Ftg0.8_25%_sigmaR+",
               "Ftg1_25%_R-","Ftg1_25%_R+","Ftg1_25%_sigmaR+")

sc_run <-  c("2S31","2S33","2S31_R0dw","2S31_R0up","2S31_sigma","2S33_R0dw","2S33_R0up","2S33_sigma")

#reference points OM
ref.pts <- read.csv(file.path("Output","Tables","RefPts_FLBRP_format.csv"))

#input file name
file_nm <- c(paste0(sc_run, "_AggregatedOutput_ALB.RData"))

refPtsMod <- TRUE

j<-1

load(paste0(dir_out,"/",sc_run[j], "_Aggregated_bio_FLBRP_RefPts_Q.RData"))

# ── 0. Cargar datos del Current MP desde el CSV ───────────────────────────────
# El CSV tiene: yr (año), stock (B/Bmsy), stock.1 (F/Fmsy)
sa_mpb <- read.csv("Advice/SA_mpb.csv")

# Convertir a formato largo para poder filtrarlo dentro de hacer_panel()
# igual que result_det, con columnas: year, indicator, valor
current_mp <- data.frame(
  year      = rep(sa_mpb$yr, 2),
  indicator = c(
    rep("ssb2Btarget", nrow(sa_mpb)),   # stock     → B/Bmsy
    rep("f2Ftarget",   nrow(sa_mpb))    # stock.1   → F/Fmsy
  ),
  valor = c(sa_mpb$stock, sa_mpb$stock.1)
)

# Filtrar hasta 2025 (por si el CSV tuviera años posteriores)
current_mp <- current_mp[current_mp$year <= 2025, ]

# ── 1. Filtrar bioQ95 y result_det hasta el año 2025 ─────────────────────────
bioQ95_plot <- bioQ95[bioQ95$indicator %in% c("ssb2Btarget", "f2Ftarget") &
                        bioQ95$year <= 2025, ]

result_det <- data.frame(
  year      = rep(resultados$year[resultados$year <= 2025], 2),
  indicator = c(
    rep("ssb2Btarget", sum(resultados$year <= 2025)),
    rep("f2Ftarget",   sum(resultados$year <= 2025))
  ),
  valor = c(
    resultados$BBmsy[resultados$year <= 2025],
    resultados$FFmsy[resultados$year <= 2025]
  )
)

# Solo valores de inicio de año (parte decimal == 0)
result_det <- result_det[result_det$year %% 1 == 0, ]

# ── 2. Función auxiliar para construir cada panel ─────────────────────────────
hacer_panel <- function(ind, y_label, mostrar_leyenda = TRUE) {
  
  d_q95       <- bioQ95_plot[bioQ95_plot$indicator == ind, ]
  d_det       <- result_det[result_det$indicator == ind, ]
  d_currentmp <- current_mp[current_mp$indicator == ind, ]   # ← nuevo
  
  p <- ggplot() +
    
    # IC 95%: azul intermedio
    geom_ribbon(
      data  = d_q95,
      aes(x = year, ymin = q025, ymax = q975, fill = "CI 95%"),
      alpha = 0.55
    ) +
    
    # Mediana del OM
    geom_line(
      data = d_q95,
      aes(x = year, y = q50, color = "OM Median"),
      linewidth = 0.9
    ) +
    
    # Línea de referencia en y = 1
    geom_hline(
      yintercept = 1,
      linetype   = "dashed",
      color      = "black",
      linewidth  = 0.8
    ) +
    
    # MP nuevo (SPiCT con CPUE 2025): línea anual
    geom_line(
      data = d_det,
      aes(x = year, y = valor, color = "Current new MP (SPiCT with CPUE 2025)"),
      linewidth = 1
    ) +
    
    # Current MP (desde SA_mpb.csv) ── ← línea nueva
    geom_line(
      data = d_currentmp,
      aes(x = year, y = valor, color = "Current MP (mpb with CPUE 2025)"),
      linewidth = 1,
      linetype  = "solid"
    ) +
    
    # ── Escalas ─────────────────────────────────────────────────────────────
    scale_fill_manual(
      name   = "Confidence interval",
      values = c("CI 95%" = "#4A86C8")
    ) +
    scale_color_manual(
      name   = "Lines",
      values = c(
        "OM Median"                 = "black",
        "Current new MP (SPiCT with CPUE 2025)" = "#D95F02",   # naranja
        "Current MP (mpb with CPUE 2025)"  = "#228B22"     # verde oscuro
      )
    ) +
    
    scale_x_continuous(
      limits = c(NA, 2025),
      breaks = scales::pretty_breaks(n = 6)
    ) +
    
    labs(
      title = ind,
      x     = "Year",
      y     = y_label
    ) +
    
    theme_bw(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 12, hjust = 0.5),
      panel.grid.minor = element_blank(),
      legend.position  = if (mostrar_leyenda) "bottom" else "none",
      legend.box       = "horizontal"
    )
  
  return(p)
}

# ── 3. Crear cada panel ───────────────────────────────────────────────────────
panel_B <- hacer_panel("ssb2Btarget", y_label = "B/Bmsy")
panel_F <- hacer_panel("f2Ftarget",   y_label = "F/Fmsy")

# ── 4. Combinar con patchwork ─────────────────────────────────────────────────
figura_final <- (panel_B | panel_F) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Comparison OM and MP 2025",
    theme = theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  ) &
  theme(
    legend.position  = "bottom",
    legend.text      = element_text(size = 8),    # ← texto más pequeño
    legend.title     = element_text(size = 9),    # ← título más pequeño
    legend.key.size  = unit(0.4, "cm")            # ← icono más pequeño
  )

print(figura_final)

# ── 5. Guardar ────────────────────────────────────────────────────────────────
ggsave("Output/Figures/OM_MP_MPnew_2025cpue_bio.png", plot = figura_final,
       width = 10, height = 5, dpi = 300)
