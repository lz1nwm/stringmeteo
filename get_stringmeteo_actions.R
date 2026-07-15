#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(rvest)
    library(data.table)
    library(stringr)
    library(lubridate)
    library(chromote)
    library(cli)
})

# ==============================================================================
# Existing data-processing functions
# ==============================================================================

add_new_dt <- function(dt.old, dt.new) {
    
    # Columns without measurement data
    cols_on <- c(
        "ddate",
        "STATION_ID",
        "STATION_NAME"
    )
    
    cols_i <- names(dt.old)[!(names(dt.old) %in% cols_on)]
    cols_d <- names(dt.old)
    
    # Update existing values
    dt.old[
        dt.new,
        (cols_i) := mget(paste0("i.", cols_i)),
        on = cols_on
    ]
    
    # Keep unmatched old rows and append new rows
    dt.old <- rbindlist(
        list(
            dt.old[!dt.new, on = cols_d],
            dt.new
        ),
        use.names = TRUE
    )
    
    setkeyv(dt.old, cols_on)
    
    dt.old <- unique(dt.old)
}

clean_data <- function(df) {
    
    df[
        ,
        c("TEMP", "TEMP_W") :=
            tstrsplit(
                gsub("\\([^\\)]+\\)", "", X3),
                "ºC"
            )
    ]
    
    df[
        ,
        TEMP_E :=
            regmatches(
                X3,
                gregexpr(
                    "(?<=\\().+?(?=\\))",
                    X3,
                    perl = TRUE
                )
            )
    ]
    
    df[, TEMP_E := gsub("ºC", "", TEMP_E)]
    
    df[, X4 := gsub("---", "ºC", X4)]
    
    df[
        ,
        c("TEMP_MAX", "TEMP_MIN") :=
            tstrsplit(X4, "ºC")
    ]
    
    df[
        ,
        c("TEMP_DEW", "HUMIDITY") :=
            tstrsplit(X5, "ºC")
    ]
    
    df[
        ,
        HUMIDITY :=
            gsub(
                "^.*\\|([0-9]+)%\\|$",
                "\\1",
                HUMIDITY
            )
    ]
    
    df[, WIND_DIRECTION := X6]
    df[, WIND_SPEED := gsub(" м/с", "", X7)]
    
    df[, CLOUDS := X9]
    df[, VISIBILITY := X15]
    
    df[
        ,
        RAIN_3 :=
            str_extract(
                X16,
                "(.*(?=\\())"
            )
    ]
    
    df[
        ,
        RAIN_3 :=
            str_extract(
                RAIN_3,
                "\\d+(.\\d+)?"
            )
    ]
    
    df[
        ,
        RAIN_12 :=
            str_extract(
                X16,
                "(?<=\\().*(?=\\))"
            )
    ]
    
    df[
        ,
        RAIN_12 :=
            str_extract(
                RAIN_12,
                "\\d+(.\\d+)?"
            )
    ]
    
    df[
        ,
        RAIN_24 :=
            str_extract(
                X16,
                "(?<=\\|).*(?=\\|)"
            )
    ]
    
    df[
        ,
        RAIN_24 :=
            str_extract(
                RAIN_24,
                "\\d+(.\\d+)?"
            )
    ]
    
    df[
        ,
        PRESSURE_1 :=
            str_extract(
                X17,
                "(.*(?=\\/))"
            )
    ]
    
    df[
        ,
        PRESSURE_2 :=
            str_extract(
                X17,
                "((?<=\\/).*)"
            )
    ]
    
    cols <- c(
        "TEMP",
        "TEMP_W",
        "TEMP_E",
        "TEMP_MAX",
        "TEMP_MIN",
        "TEMP_DEW",
        "HUMIDITY",
        "WIND_SPEED",
        "RAIN_12",
        "RAIN_24",
        "PRESSURE_1",
        "PRESSURE_2"
    )
    
    df[
        ,
        (cols) := lapply(.SD, as.numeric),
        .SDcols = cols
    ]
    
    df <- df[!is.na(RAIN_24)]
    
    df[
        ,
        names(df)[names(df) %like% "^X"] := NULL
    ]
    
    df <- unique(df)
    
    df
}

# ==============================================================================
# Chromote functions
# ==============================================================================

js_value <- function(browser, expression) {
    
    result <- browser$Runtime$evaluate(
        expression = expression,
        returnByValue = TRUE,
        awaitPromise = TRUE
    )
    
    if (!is.null(result$exceptionDetails)) {
        
        description <-
            result$exceptionDetails$exception$description
        
        if (is.null(description)) {
            description <- result$exceptionDetails$text
        }
        
        stop(
            "JavaScript evaluation failed: ",
            description,
            call. = FALSE
        )
    }
    
    result$result$value
}

get_page_state <- function(browser) {
    
    js_value(
        browser,
        paste0(
            "(() => ({",
            "url: window.location.href,",
            "title: document.title || '',",
            "readyState: document.readyState,",
            "tableCount: document.querySelectorAll('table').length,",
            "bodyText: document.body ? ",
            "document.body.innerText.substring(0, 1500) : ''",
            "}))()"
        )
    )
}

get_page_html <- function(browser) {
    
    js_value(
        browser,
        paste0(
            "document.documentElement ? ",
            "document.documentElement.outerHTML : ''"
        )
    )
}

is_expected_page <- function(current_url, expected_url) {
    
    if (
        is.null(current_url) ||
        length(current_url) != 1L ||
        is.na(current_url) ||
        !nzchar(current_url)
    ) {
        return(FALSE)
    }
    
    if (
        grepl(
            "/__superjs/challenge",
            current_url,
            fixed = TRUE
        )
    ) {
        return(FALSE)
    }
    
    if (
        !grepl(
            "/synop/bg_stday.php",
            current_url,
            fixed = TRUE
        )
    ) {
        return(FALSE)
    }
    
    expected_query <- sub(
        "^[^?]*\\?",
        "",
        expected_url
    )
    
    expected_parts <- strsplit(
        expected_query,
        "&",
        fixed = TRUE
    )[[1]]
    
    all(
        vapply(
            expected_parts,
            function(part) {
                grepl(
                    part,
                    current_url,
                    fixed = TRUE
                )
            },
            logical(1)
        )
    )
}

configure_browser <- function(browser) {
    
    browser_version <- browser$Browser$getVersion()
    
    original_user_agent <- browser_version$userAgent
    
    normal_user_agent <- sub(
        "HeadlessChrome/",
        "Chrome/",
        original_user_agent,
        fixed = TRUE
    )
    
    browser$Network$enable()
    
    browser$Network$setUserAgentOverride(
        userAgent = normal_user_agent,
        acceptLanguage =
            "bg-BG,bg;q=0.9,en-US;q=0.8,en;q=0.7",
        platform = "Linux x86_64"
    )
    
    browser$Network$setExtraHTTPHeaders(
        headers = list(
            Accept = paste0(
                "text/html,application/xhtml+xml,",
                "application/xml;q=0.9,image/avif,",
                "image/webp,image/apng,*/*;q=0.8"
            ),
            `Accept-Language` =
                "bg-BG,bg;q=0.9,en-US;q=0.8,en;q=0.7",
            `Cache-Control` = "no-cache",
            Pragma = "no-cache"
        )
    )
    
    message(
        "Browser: ",
        browser_version$product
    )
    
    invisible(normal_user_agent)
}

create_browser <- function() {
    
    chrome_path <- chromote::find_chrome()
    
    if (
        is.null(chrome_path) ||
        !nzchar(chrome_path)
    ) {
        stop(
            paste(
                "Chromote could not find Chrome.",
                "Check CHROMOTE_CHROME and the Docker image."
            ),
            call. = FALSE
        )
    }
    
    chrome_profile <- Sys.getenv(
        "CHROME_USER_DATA_DIR",
        unset = file.path(
            tempdir(),
            "stringmeteo-chrome-profile"
        )
    )
    
    dir.create(
        chrome_profile,
        recursive = TRUE,
        showWarnings = FALSE
    )
    
    if (!dir.exists(chrome_profile)) {
        stop(
            "Could not create Chrome profile: ",
            chrome_profile,
            call. = FALSE
        )
    }
    
    if (file.access(chrome_profile, mode = 2) != 0) {
        stop(
            "Chrome profile is not writable: ",
            chrome_profile,
            call. = FALSE
        )
    }
    
    chrome_args <- unique(
        c(
            chromote::default_chrome_args(),
            paste0(
                "--user-data-dir=",
                chrome_profile
            )
        )
    )
    
    chromote::set_chrome_args(chrome_args)
    
    message("Chrome executable: ", chrome_path)
    message("Chrome profile: ", chrome_profile)
    
    browser <- ChromoteSession$new(
        width = 1440,
        height = 1000
    )
    
    configure_browser(browser)
    
    browser
}

wait_for_table <- function(
        browser,
        expected_url,
        timeout = 90,
        polling_interval = 1
) {
    
    deadline <- Sys.time() + timeout
    last_state <- NULL
    last_reported_url <- NULL
    
    repeat {
        
        state <- tryCatch(
            get_page_state(browser),
            error = function(e) {
                list(
                    error = conditionMessage(e),
                    url = NA_character_,
                    title = NA_character_,
                    readyState = NA_character_,
                    tableCount = 0,
                    bodyText = ""
                )
            }
        )
        
        last_state <- state
        
        if (
            !is.null(state$url) &&
            length(state$url) == 1L &&
            !is.na(state$url) &&
            !identical(
                state$url,
                last_reported_url
            )
        ) {
            message("Browser URL: ", state$url)
            last_reported_url <- state$url
        }
        
        table_count <- state$tableCount
        
        table_found <-
            !is.null(table_count) &&
            length(table_count) == 1L &&
            !is.na(table_count) &&
            table_count > 0
        
        correct_page <- is_expected_page(
            state$url,
            expected_url
        )
        
        if (table_found && correct_page) {
            return(state)
        }
        
        if (Sys.time() >= deadline) {
            return(last_state)
        }
        
        Sys.sleep(polling_interval)
    }
}

save_failure_diagnostics <- function(
        browser,
        station_id,
        year,
        month
) {
    
    output_dir <- Sys.getenv(
        "STRINGMETEO_OUTPUT_DIR",
        unset = "chromote-debug"
    )
    
    dir.create(
        output_dir,
        recursive = TRUE,
        showWarnings = FALSE
    )
    
    prefix <- sprintf(
        "failure-%s-%04d-%02d",
        station_id,
        year,
        month
    )
    
    screenshot_path <- file.path(
        output_dir,
        paste0(prefix, ".png")
    )
    
    html_path <- file.path(
        output_dir,
        paste0(prefix, ".html")
    )
    
    state_path <- file.path(
        output_dir,
        paste0(prefix, "-state.txt")
    )
    
    try(
        browser$screenshot(
            filename = screenshot_path,
            show = FALSE,
            delay = 1
        ),
        silent = TRUE
    )
    
    html <- tryCatch(
        get_page_html(browser),
        error = function(e) {
            paste(
                "<html><body><pre>",
                conditionMessage(e),
                "</pre></body></html>"
            )
        }
    )
    
    writeLines(
        enc2utf8(html),
        html_path,
        useBytes = TRUE
    )
    
    state <- tryCatch(
        get_page_state(browser),
        error = function(e) {
            list(error = conditionMessage(e))
        }
    )
    
    capture.output(
        str(state),
        file = state_path
    )
    
    message("Failure screenshot: ", screenshot_path)
    message("Failure HTML: ", html_path)
    message("Failure state: ", state_path)
    
    invisible(
        list(
            screenshot = screenshot_path,
            html = html_path,
            state = state_path
        )
    )
}

fetch_stringmeteo_table <- function(
        browser,
        url,
        station_id,
        year,
        month,
        timeout = 90
) {
    
    message(
        sprintf(
            "Downloading station %s, %04d-%02d",
            station_id,
            year,
            month
        )
    )
    
    navigation_error <- NULL
    
    tryCatch(
        browser$go_to(
            url,
            delay = 2,
            timeout_ = 30
        ),
        error = function(e) {
            
            navigation_error <<- conditionMessage(e)
            
            message(
                "Navigation warning: ",
                navigation_error
            )
        }
    )
    
    state <- wait_for_table(
        browser = browser,
        expected_url = url,
        timeout = timeout,
        polling_interval = 1
    )
    
    table_count <- state$tableCount
    
    success <-
        !is.null(table_count) &&
        length(table_count) == 1L &&
        !is.na(table_count) &&
        table_count > 0 &&
        is_expected_page(
            state$url,
            url
        )
    
    if (!success) {
        
        save_failure_diagnostics(
            browser = browser,
            station_id = station_id,
            year = year,
            month = month
        )
        
        body_text <- state$bodyText
        
        if (is.null(body_text)) {
            body_text <- ""
        }
        
        stop(
            paste0(
                "Chromote did not obtain the StringMeteo table.\n",
                "Requested URL: ",
                url,
                "\n",
                "Final URL: ",
                state$url,
                "\n",
                "Page title: ",
                state$title,
                "\n",
                "Page text:\n",
                substr(body_text, 1, 500)
            ),
            call. = FALSE
        )
    }
    
    rendered_html <- get_page_html(browser)
    
    page <- read_html(
        enc2utf8(rendered_html),
        encoding = "UTF-8"
    )
    
    # This matches the old script's html_element("table") behavior.
    table_node <- html_element(
        page,
        "table"
    )
    
    if (inherits(table_node, "xml_missing")) {
        
        save_failure_diagnostics(
            browser = browser,
            station_id = station_id,
            year = year,
            month = month
        )
        
        stop(
            "The rendered page contained no first HTML table.",
            call. = FALSE
        )
    }
    
    # This returns the same kind of object that the old code assigned to `dt`.
    dt <- html_table(table_node)
    
    dt
}

# ==============================================================================
# Main update
# ==============================================================================

main <- function() {
    
    # Avoid the unsupported Windows-style "Bulgarian.utf8" locale in Linux.
    suppressWarnings(
        try(
            Sys.setlocale(
                "LC_CTYPE",
                "C.UTF-8"
            ),
            silent = TRUE
        )
    )
    
    base_url <-
        "https://www.stringmeteo.com/synop/bg_stday.php?"
    
    dt.nimh <- readRDS(
        file = "./RData/bg_nimh.rds"
    )
    
    st <- readRDS(
        file = "./RData/bg_stations.rds"
    )
    
    print(max(dt.nimh$ddate))
    
    dt_ini <- lubridate::floor_date(
        Sys.Date(),
        unit = "month"
    )
    
    dt_ini_1 <- dt_ini %m-% months(1)
    dt_ini_2 <- dt_ini %m-% months(2)
    
    # Update the current month normally.
    # On the first day of a month, also refresh the previous two months.
    if (mday(Sys.Date()) != 1) {
        
        dates <- dt_ini
        
    } else {
        
        dates <- c(
            dt_ini,
            dt_ini_1,
            dt_ini_2
        )
    }
    
    list_dt <- list()
    
    cli_progress_bar(
        total =
            length(dates) *
            length(st[, station_id])
    )
    
    challenge_timeout <- as.numeric(
        Sys.getenv(
            "STRINGMETEO_TIMEOUT",
            unset = "90"
        )
    )
    
    browser <- create_browser()
    
    on.exit(
        {
            try(
                browser$close(),
                silent = TRUE
            )
        },
        add = TRUE
    )
    
    start.time <- Sys.time()
    
    for (cty in st[, station_id]) {
        
        for (i in seq_along(dates)) {
            
            yr <- year(dates[i])
            m <- month(dates[i])
            d <- mday(dates[i])
            
            list_name <- paste(cty, i)
            
            if (!(list_name %in% names(list_dt))) {
                
                params <- paste0(
                    "year=", yr,
                    "&month=", m,
                    "&day=", d,
                    "&city=", cty,
                    "&int=31"
                )
                
                url <- paste0(
                    base_url,
                    params
                )
                
                # ----------------------------------------------------------------------
                # Only this acquisition step differs from get_stringmeteo.R.
                # It returns the same `dt` object as html_table() in the local script.
                # ----------------------------------------------------------------------
                
                dt <- fetch_stringmeteo_table(
                    browser = browser,
                    url = url,
                    station_id = cty,
                    year = yr,
                    month = m,
                    timeout = challenge_timeout
                )
                
                # ----------------------------------------------------------------------
                # Everything below is the existing processing pipeline.
                # ----------------------------------------------------------------------
                
                list_dt[[list_name]] <- dt |>
                    setDT() |>
                    _[, 1:18] |>
                    _[X1 %like% "\\d\\["] |>
                    _[
                        ,
                        ddate := as.POSIXct(
                            paste0(yr, X2),
                            format = "%Y%d.%m.%H%M",
                            tz = "UTC"
                        )
                    ] |>
                    _[, c("X1", "X2") := NULL] |>
                    _[, STATION_ID := cty] |>
                    _[
                        ,
                        STATION_NAME :=
                            st[
                                station_id == cty,
                                station_name
                            ]
                    ]
                
                cli_progress_update()
                
                Sys.sleep(
                    sample(
                        1:2,
                        1
                    )
                )
            }
        }
    }
    
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    
    print(time.taken)
    
    dt.nimh.n <- rbindlist(
        list_dt,
        use.names = TRUE
    )
    
    dt.nimh.n <- clean_data(
        dt.nimh.n
    )
    
    dt.nimh <- readRDS(
        file = "./RData/bg_nimh.rds"
    )
    
    print(nrow(dt.nimh))
    print(max(dt.nimh$ddate))
    
    dt.nimh <- add_new_dt(
        dt.nimh,
        dt.nimh.n
    )
    
    print(nrow(dt.nimh))
    print(max(dt.nimh$ddate))
    
    saveRDS(
        dt.nimh,
        file = "./RData/bg_nimh.rds"
    )
}

main()