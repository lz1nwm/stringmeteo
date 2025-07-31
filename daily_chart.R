library(data.table)
library(ggplot2)
library(scales)
library(lubridate)
library(openxlsx2)
library(lemon)

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

dt.nimh[, OBS := .N, by = .(STATION_ID, ddate |> as.Date())]

dt.nimh.d <- dt.nimh[year(ddate) %in% c(2005:2024), 
                     .(tavg_hist = median(TEMP, na.rm=T),
                       tmax_hist = quantile(TEMP, 1, na.rm=T),
                       tmin_hist = quantile(TEMP, 0, na.rm=T)), 
                     by = .(STATION_ID,
                            #YR = year(Date),
                            M = month(ddate),
                            D = mday(ddate))] |> 
    _[, ddate_last := as.Date(paste(2025, M, D, sep = '-'))] |>
    setkey(STATION_ID, M, D)

include_today <- dt.nimh[as.Date(ddate) == Sys.Date(), any(OBS == 8)]

if(include_today){
    dt.nimh.s <- dt.nimh[year(ddate)>=year(Sys.Date())-1, .(tavg = median(TEMP, na.rm=T)),
                         #max = max(TEMP_MAX, na.rm=T),
                         #min = min(TEMP_MIN, na.rm=T)), 
                     by = .(STATION_ID, STATION_NAME, ddate = as.Date(ddate))] |> 
    _[, ddate_last := as.Date(paste(year(Sys.Date()), month(ddate), mday(ddate), sep = '-'))] |>
    setkey(STATION_ID, STATION_NAME, ddate)
}else{
    dt.nimh.s <- dt.nimh[year(ddate)>=year(Sys.Date())-1 & as.Date(ddate) != Sys.Date(), .(tavg = median(TEMP, na.rm=T)),
                         #max = max(TEMP_MAX, na.rm=T),
                         #min = min(TEMP_MIN, na.rm=T)), 
                         by = .(STATION_ID, STATION_NAME, ddate = as.Date(ddate))] |> 
        _[, ddate_last := as.Date(paste(year(Sys.Date()), month(ddate), mday(ddate), sep = '-'))] |>
        setkey(STATION_ID, STATION_NAME, ddate)
}


dt.nimh.s <- merge(dt.nimh.s, dt.nimh.d, by = c('STATION_ID', 'ddate_last'), all.x = TRUE) 

colors <- c("median\n2005-2024" = "white", "range\n2005-2024" = "red", "2025" = "firebrick", "2024" = "skyblue2")

sts <- unique(dt.nimh.s[,.(STATION_ID, STATION_NAME)]) |> setkey()
sts[, STATION_ID := factor(STATION_ID, levels = c(15614,15712,15552,15655,
                                                  15615,15600,15640,15730,
                                                  15502,15525,15549))]|> setkey()
dt.nimh.s[, STATION_ID := factor(STATION_ID, levels = c(15614,15712,15552,15655,
                                                        15615,15600,15640,15730,
                                                        15502,15525,15549)) ]|> 
    setkey(ddate_last, STATION_ID)

stations <- sts$STATION_ID[sts$STATION_ID != 15600]


dt.nimh[ddate>='2025-07-30' & STATION_ID == '15615']

ggplot(data = dt.nimh.s[STATION_ID %in% stations]) +
    geom_hline(yintercept = 0)+
    geom_ribbon(mapping = aes(x = ddate_last, ymax = tmax_hist, ymin = tmin_hist, fill = 'range\n2005-2024'), 
                alpha = 0.4)+
    geom_line(mapping = aes(x=ddate_last, y = tavg_hist, color = 'median\n2005-2024'), 
              linewidth  = 0.6)+
    geom_line(mapping = aes(x = ddate_last |> as.Date(), y = tavg, color = '2024'), 
              data = dt.nimh.s[year(ddate) == 2024 & STATION_ID %in% stations], 
              linewidth = 0.6)+
    geom_line(mapping = aes(x = ddate |> as.Date(), y = tavg, color = '2025'), 
              data = dt.nimh.s[year(ddate) == 2025 & STATION_ID %in% stations], 
              linewidth = 0.6)+
    scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(10))+
    scale_x_date(date_breaks = '1 month', expand = expansion(0),
                 labels = scales::label_date_short())+
    scale_color_manual(values = colors, name = '')+
    facet_rep_wrap(. ~ paste0(STATION_NAME,' (',STATION_ID,'), ', max(dt.nimh.s[, ddate])), 
               scales = 'free_y',
               repeat.tick.labels = T,
               ncol = 2)+
    theme(axis.text = element_text(colour = 'black'),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom')


ggsave('figs/nimh_temp_2025.pdf', width = 12, height = 6*4, device = cairo_pdf)


# Weekly

dt.nimh[, ddate2 := ddate] 
year(dt.nimh$ddate2) <- year(Sys.Date())

#date_w_start <- floor_date(Sys.Date(), 'week', week_start = 1) %m-% weeks(1)
date_w_start <- Sys.Date() %m-% days(6)
week_dates <- c(date_w_start, Sys.Date() + 1) |> as.POSIXct()

dt.nimh.w <- dt.nimh[year(ddate) %in% c(2005:2024), 
                     .(tavg_hist = median(TEMP, na.rm=T),
                       tmax_hist = max(TEMP, na.rm=T),
                       tmin_hist = min(TEMP, na.rm=T)), 
                     by = .(STATION_ID, STATION_NAME, ddate2)] |> 
    setkey(STATION_ID, STATION_NAME, ddate2) |> 
    _[ddate2 %between% week_dates]


dt.nimh.w <- merge(dt.nimh[year(ddate) >= year(Sys.Date())-1], dt.nimh.w[,.(STATION_ID, ddate2, tavg_hist, tmax_hist, tmin_hist)], by = c('STATION_ID', 'ddate2'), all.x = TRUE) 

ggplot(dt.nimh.w[STATION_ID %in% stations & ddate2 %between% week_dates]) +
    geom_hline(yintercept = 0)+
    geom_ribbon(mapping = aes(x = ddate2, ymax = tmax_hist, ymin = tmin_hist, fill = 'range\n2005-2024'), 
                alpha = 0.4)+
    geom_line(mapping = aes(x=ddate2, y = tavg_hist, color = 'median\n2005-2024'), 
              linewidth  = 0.6) +
    geom_line(mapping = aes(x = ddate2, y = TEMP, color = '2024'), 
              data = dt.nimh.w[year(ddate) == 2024 & STATION_ID %in%  stations & ddate2 %between% week_dates], 
              linewidth = 0.6)+
    geom_line(mapping = aes(x = ddate2, y = TEMP, color = '2025'), 
              data = dt.nimh.w[year(ddate) == 2025 & STATION_ID %in%  stations & ddate2 %between% week_dates], 
              linewidth = 0.6)+
    scale_y_continuous(sec.axis = dup_axis(), breaks = scales::pretty_breaks(10))+
    scale_x_datetime(date_breaks = '1 day', expand = expansion(0), minor_breaks = NULL,
                 labels = scales::label_date_short())+
    scale_color_manual(values = colors, name = '')+
    #facet_wrap(. ~ paste0(STATION_NAME,' (',STATION_ID,'), ',max(dt.nimh$ddate)), scales = 'free_y')+
    facet_rep_wrap(. ~ paste0(STATION_NAME,' (',STATION_ID,'), ', max(dt.nimh$ddate)), 
                          scales = 'free_y',
                          repeat.tick.labels = T,
                          ncol = 2)+
    theme(axis.text = element_text(colour = 'black'),
          axis.title = element_blank(),
          legend.title = element_blank(),
          legend.position = 'bottom')

ggsave('figs/nimh_temp_2025w.pdf', width = 12, height = 6*4, device = cairo_pdf)

