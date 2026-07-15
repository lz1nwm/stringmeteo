#!/usr/bin/env Rscript

suppressPackageStartupMessages({
    library(chromote)
    library(rvest)
    library(data.table)
})

# ------------------------------------------------------------------------------
# Configuration
# ------------------------------------------------------------------------------

station_id <- Sys.getenv("STRINGMETEO_STATION", unset = "15614")
year       <- as.integer(Sys.getenv("STRINGMETEO_YEAR", unset = "2026"))
month      <- as.integer(Sys.getenv("STRINGMETEO_MONTH", unset = "7"))
day        <- as.integer(Sys.getenv("STRINGMETEO_DAY", unset = "1"))

challenge_timeout <- as.numeric(
    Sys.getenv("STRINGMETEO_TIMEOUT", unset = "60")
)

output_dir <- Sys.getenv(
    "STRINGMETEO_OUTPUT_DIR",
    unset = "chromote-debug"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

url <- paste0(
    "https://www.stringmeteo.com/synop/bg_stday.php",
    "?year=", year,
    "&month=", month,
    "&day=", day,
    "&city=", station_id,
    "&int=31"
)

# ------------------------------------------------------------------------------
# Utility functions
# ------------------------------------------------------------------------------

js_value <- function(browser, expression) {
    response <- browser$Runtime$evaluate(
        expression = expression,
        returnByValue = TRUE,
        awaitPromise = TRUE
    )
    
    if (!is.null(response$exceptionDetails)) {
        description <- response$exceptionDetails$exception$description
        
        if (is.null(description)) {
            description <- response$exceptionDetails$text
        }
        
        stop(
            "JavaScript evaluation failed: ",
            description,
            call. = FALSE
        )
    }
    
    response$result$value
}

get_page_state <- function(browser) {
    js_value(
        browser,
        paste0(
            "(() => ({",
            "  url: window.location.href,",
            "  title: document.title || '',",
            "  readyState: document.readyState,",
            "  tableCount: document.querySelectorAll('table').length,",
            "  bodyText: document.body ? ",
            "    document.body.innerText.substring(0, 1000) : ''",
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

write_text_file <- function(text, path) {
    con <- file(
        path,
        open = "wb"
    )
    
    on.exit(close(con), add = TRUE)
    
    writeBin(
        charToRaw(enc2utf8(text)),
        con
    )
}

save_diagnostics <- function(browser, prefix = "stringmeteo") {
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
    
    write_text_file(html, html_path)
    
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
    
    message("Diagnostic screenshot: ", screenshot_path)
    message("Diagnostic HTML:       ", html_path)
    message("Diagnostic state:      ", state_path)
    
    invisible(
        list(
            screenshot = screenshot_path,
            html = html_path,
            state = state_path
        )
    )
}

configure_browser <- function(browser) {
    browser_version <- browser$Browser$getVersion()
    
    original_user_agent <- browser_version$userAgent
    
    # Remove the obvious "HeadlessChrome" marker while retaining the actual
    # installed browser version.
    normal_user_agent <- sub(
        "HeadlessChrome/",
        "Chrome/",
        original_user_agent,
        fixed = TRUE
    )
    
    browser$Network$enable()
    
    browser$Network$setUserAgentOverride(
        userAgent = normal_user_agent,
        acceptLanguage = "bg-BG,bg;q=0.9,en-US;q=0.8,en;q=0.7",
        platform = "Linux x86_64"
    )
    
    browser$Network$setExtraHTTPHeaders(
        headers = list(
            Accept = paste0(
                "text/html,application/xhtml+xml,",
                "application/xml;q=0.9,image/avif,",
                "image/webp,image/apng,*/*;q=0.8"
            ),
            `Accept-Language` = "bg-BG,bg;q=0.9,en-US;q=0.8,en;q=0.7",
            `Cache-Control` = "no-cache",
            Pragma = "no-cache"
        )
    )
    
    message("Browser product:    ", browser_version$product)
    message("Original UA:        ", original_user_agent)
    message("Overridden UA:      ", normal_user_agent)
    
    invisible(normal_user_agent)
}

wait_for_table <- function(
        browser,
        timeout = 60,
        polling_interval = 1
) {
    deadline <- Sys.time() + timeout
    last_state <- NULL
    last_url <- NULL
    
    repeat {
        state <- tryCatch(
            get_page_state(browser),
            error = function(e) {
                list(
                    error = conditionMessage(e),
                    url = NA_character_,
                    title = NA_character_,
                    readyState = NA_character_,
                    tableCount = 0
                )
            }
        )
        
        last_state <- state
        
        current_url <- state$url
        
        if (
            !is.null(current_url) &&
            length(current_url) == 1L &&
            !is.na(current_url) &&
            !identical(current_url, last_url)
        ) {
            message("Current URL: ", current_url)
            last_url <- current_url
        }
        
        table_count <- state$tableCount
        
        if (
            !is.null(table_count) &&
            length(table_count) == 1L &&
            !is.na(table_count) &&
            table_count > 0
        ) {
            message(
                "Found ",
                table_count,
                " HTML table(s)."
            )
            
            return(state)
        }
        
        if (Sys.time() >= deadline) {
            return(last_state)
        }
        
        Sys.sleep(polling_interval)
    }
}

extract_first_table <- function(browser) {
    html <- get_page_html(browser)
    
    document <- read_html(
        enc2utf8(html),
        encoding = "UTF-8"
    )
    
    tables <- html_elements(document, "table")
    
    if (length(tables) == 0L) {
        stop(
            "The rendered document contains no HTML tables.",
            call. = FALSE
        )
    }
    
    parsed_tables <- lapply(
        tables,
        function(node) {
            tryCatch(
                html_table(
                    node,
                    fill = TRUE,
                    trim = TRUE,
                    convert = FALSE
                ),
                error = function(e) NULL
            )
        }
    )
    
    parsed_tables <- Filter(
        Negate(is.null),
        parsed_tables
    )
    
    if (length(parsed_tables) == 0L) {
        stop(
            "HTML tables were present, but none could be parsed.",
            call. = FALSE
        )
    }
    
    # Prefer the largest table, because pages may contain small layout tables.
    sizes <- vapply(
        parsed_tables,
        function(x) {
            nrow(x) * max(1L, ncol(x))
        },
        numeric(1)
    )
    
    parsed_tables[[which.max(sizes)]]
}

# ------------------------------------------------------------------------------
# Main test
# ------------------------------------------------------------------------------

main <- function() {
    chrome_path <- chromote::find_chrome()
    
    if (is.null(chrome_path) || !nzchar(chrome_path)) {
        stop(
            paste(
                "Chromote could not find Chrome or Chromium.",
                "Install a Chromium-based browser or set CHROMOTE_CHROME."
            ),
            call. = FALSE
        )
    }
    
    message("Chrome executable: ", chrome_path)
    message("Target URL:        ", url)
    
    chrome_profile <- Sys.getenv(
        "CHROME_USER_DATA_DIR",
        unset = file.path(tempdir(), "chromote-profile")
    )
    
    dir.create(
        chrome_profile,
        recursive = TRUE,
        showWarnings = FALSE
    )
    
    if (!dir.exists(chrome_profile)) {
        stop(
            "Could not create Chrome profile directory: ",
            chrome_profile,
            call. = FALSE
        )
    }
    
    if (file.access(chrome_profile, mode = 2) != 0) {
        stop(
            "Chrome profile directory is not writable: ",
            chrome_profile,
            call. = FALSE
        )
    }
    
    chrome_args <- unique(
        c(
            chromote::default_chrome_args(),
            paste0("--user-data-dir=", chrome_profile)
        )
    )
    
    chromote::set_chrome_args(chrome_args)
    
    message("Chrome profile:    ", chrome_profile)
    message("Chrome arguments:  ", paste(chrome_args, collapse = " "))
    
    browser <- ChromoteSession$new(
        width = 1440,
        height = 1000
    )
    
    on.exit(
        {
            try(browser$close(), silent = TRUE)
        },
        add = TRUE
    )
    
    configure_browser(browser)
    
    message("Navigating to StringMeteo...")
    
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
                "Initial navigation reported an error: ",
                navigation_error
            )
        }
    )
    
    state <- wait_for_table(
        browser,
        timeout = challenge_timeout,
        polling_interval = 1
    )
    
    message("")
    message("Final browser state:")
    message("  URL:         ", state$url)
    message("  Title:       ", state$title)
    message("  Ready state: ", state$readyState)
    message("  Table count: ", state$tableCount)
    
    if (
        is.null(state$tableCount) ||
        is.na(state$tableCount) ||
        state$tableCount == 0
    ) {
        save_diagnostics(
            browser,
            prefix = "stringmeteo-failure"
        )
        
        body_text <- state$bodyText
        
        if (is.null(body_text)) {
            body_text <- ""
        }
        
        stop(
            paste0(
                "Chromote did not reach the meteorological table within ",
                challenge_timeout,
                " seconds.\n",
                "Final URL: ",
                state$url,
                "\n",
                "Page title: ",
                state$title,
                "\n",
                "Page text begins with:\n",
                substr(body_text, 1, 500),
                "\n\n",
                "If the final URL still contains /__superjs/challenge, ",
                "StringMeteo is rejecting or failing to validate the ",
                "headless browser."
            ),
            call. = FALSE
        )
    }
    
    table <- extract_first_table(browser)
    table_dt <- as.data.table(table)
    
    html_path <- file.path(
        output_dir,
        "stringmeteo-success.html"
    )
    
    screenshot_path <- file.path(
        output_dir,
        "stringmeteo-success.png"
    )
    
    csv_path <- file.path(
        output_dir,
        "stringmeteo-table.csv"
    )
    
    rds_path <- file.path(
        output_dir,
        "stringmeteo-table.rds"
    )
    
    write_text_file(
        get_page_html(browser),
        html_path
    )
    
    browser$screenshot(
        filename = screenshot_path,
        show = FALSE,
        delay = 1
    )
    
    fwrite(
        table_dt,
        csv_path,
        bom = TRUE
    )
    
    saveRDS(
        table_dt,
        rds_path
    )
    
    message("")
    message("SUCCESS")
    message("Rows:       ", nrow(table_dt))
    message("Columns:    ", ncol(table_dt))
    message("CSV:        ", csv_path)
    message("RDS:        ", rds_path)
    message("HTML:       ", html_path)
    message("Screenshot: ", screenshot_path)
    message("")
    message("First rows:")
    
    print(
        head(table_dt)
    )
    
    invisible(table_dt)
}

main()