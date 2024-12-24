package main

import "lexer"
import "parser"

import "core:os"
import "core:log"

logger_proc : log.Logger_Proc : proc(data: rawptr, level: log.Level, text: string, options: log.Options, location:=#caller_location) {
}

main :: proc() {
    logger := log.create_console_logger()
    defer log.destroy_console_logger(logger)
    context.logger = logger
    args := os.args

    for arg in args {
        log.info(arg)
    }
}
