library(data.table)
library(ggplot2)
library(scales)
library(lubridate)
library(openxlsx2)

load('RData/bg_nimh.RData')

# dt.nimh[TEMP_MIN <= -50, TEMP_MIN := NA_real_]
# dt.nimh[TEMP_MAX >= 50, TEMP_MAX := NA_real_]
# 
# dt.nimh[, TEMP_CH := TEMP - shift(TEMP,1), by = .(STATION_ID)]
# 
# ggplot(dt.nimh[year(ddate)>=2005])+
#     geom_histogram(mapping = aes(x = TEMP_CH), 
#                    binwidth = 0.5, fill = 'orange2', color = 'black')+
#     coord_cartesian(ylim = c(0, 5))+
#     facet_wrap(STATION_ID ~ ., scales = 'free_y')


dt.nimh.d <- dt.nimh[year(ddate) %in% c(2005:2024), 
                     .(tavg = median(TEMP, na.rm=T),
                       tmax = max(TEMP, na.rm=T),
                       tmin = min(TEMP, na.rm=T)), 
                     by = .(STATION_ID,
                            #YR = year(Date),
                            M = month(ddate),
                            D = mday(ddate))] |> 
    _[, ddate_last := as.Date(paste(2025, M, D, sep = '-'))] |>
    setkey(STATION_ID, M, D)



dt.nimh.s <- dt.nimh[year(ddate)>=2005, .(tavg = median(TEMP, na.rm=T)),
                         #max = max(TEMP_MAX, na.rm=T),
                         #min = min(TEMP_MIN, na.rm=T)), 
                     by = .(STATION_ID, STATION_NAME, ddate = as.Date(ddate))] |> 
    _[, ddate_last := as.Date(paste(2025, month(ddate), mday(ddate), sep = '-'))] |>
    setkey(STATION_ID, STATION_NAME, ddate)

colors <- c("median\n2005-2024" = "white", "range\n2005-2024" = "red", "2025" = "firebrick", "2024" = "skyblue2")

ggplot() +
    geom_hline(yintercept = 0)+
    geom_ribbon(mapping = aes(x = ddate_last, ymax = tmax, ymin = tmin, fill = 'range\n2005-2024'), 
                data = dt.nimh.d[STATION_ID == '15614'], alpha = 0.4)+
    geom_line(mapping = aes(x=ddate_last, y = tavg, color = 'median\n2005-2024'), 
              data = dt.nimh.d[STATION_ID == '15614'], linewidth  = 0.8)+
    geom_line(mapping = aes(x = ddate_last |> as.Date(), y = tavg, color = '2024'), 
              data = dt.nimh.s[year(ddate) == 2024 & STATION_ID == '15614'], , linewidth = 0.8)+
    geom_line(mapping = aes(x = ddate |> as.Date(), y = tavg, color = '2025'), 
              data = dt.nimh.s[year(ddate) == 2025 & STATION_ID == '15614' & ddate != Sys.Date()], , linewidth = 0.8)+
    scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(10))+
    scale_x_date(date_breaks = '1 month', expand = expansion(0),
                 labels = scales::label_date_short())+
    scale_color_manual(values = colors, name = '')+
    facet_grid(. ~ paste0(STATION_NAME,' (',STATION_ID,'), ',max(dt.nimh.s$ddate)), scales = 'free_y')+
    theme(axis.text = element_text(colour = 'black'),
          axis.title = element_blank(),
          legend.title = element_blank())

ggsave('figs/nimh_temp_2025.pdf', width = 12, height = 6, device = cairo_pdf)


# Weekly

dt.nimh[, ddate2 := ddate] 
year(dt.nimh$ddate2) <- 2025

date_w_start <- floor_date(Sys.Date(), 'week', week_start = 1) %m-% weeks(1)
week_dates <- c(date_w_start, Sys.Date() + 1) |> as.POSIXct()

dt.nimh.w <- dt.nimh[year(ddate) %in% c(2005:2024), 
                     .(tavg = median(TEMP, na.rm=T),
                       tmax = max(TEMP, na.rm=T),
                       tmin = min(TEMP, na.rm=T)), 
                     by = .(STATION_ID, STATION_NAME, ddate2)] |> 
    setkey(STATION_ID, STATION_NAME, ddate2) |> 
    _[ddate2 %between% week_dates]


ggplot() +
    geom_hline(yintercept = 0)+
    geom_ribbon(mapping = aes(x = ddate2, ymax = tmax, ymin = tmin, fill = 'range\n2005-2024'), 
                data = dt.nimh.w[STATION_ID == '15614'], alpha = 0.4)+
    geom_line(mapping = aes(x=ddate2, y = tavg, color = 'median\n2005-2024'), 
              data = dt.nimh.w[STATION_ID == '15614'], linewidth  = 0.8) +
    geom_line(mapping = aes(x = ddate2, y = TEMP, color = '2024'), 
              data = dt.nimh[year(ddate) == 2024 & STATION_ID == '15614' & ddate2 %between% week_dates], 
              linewidth = 0.8)+
    geom_line(mapping = aes(x = ddate2, y = TEMP, color = '2025'), 
              data = dt.nimh[year(ddate) == 2025 & STATION_ID == '15614' & ddate2 %between% week_dates], 
              linewidth = 0.8)+
    scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(10))+
    scale_x_datetime(date_breaks = '1 day', expand = expansion(0), minor_breaks = NULL,
                 labels = scales::label_date_short())+
    scale_color_manual(values = colors, name = '')+
    facet_grid(. ~ paste0(STATION_NAME,' (',STATION_ID,'), ',max(dt.nimh$ddate)), scales = 'free_y')+
    theme(axis.text = element_text(colour = 'black'),
          axis.title = element_blank(),
          legend.title = element_blank())

ggsave('figs/nimh_temp_2025w.pdf', width = 12, height = 6, device = cairo_pdf)
