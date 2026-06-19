# Helpers to test multiple messages in sequence.
# Usage:
#   expect_messages(out <- f(), "message1", "message2", nomatch("message3"))
# Note:
#   Single-message tests should still use testthat::expect_message.
nomatch <- function(pattern) structure(pattern, excluded = TRUE)

expect_messages <- function(expr, ...) {
    actual_messages <- character()

    result <- withCallingHandlers(
        expr,
        message = function(m) {
            actual_messages <<- c(actual_messages, conditionMessage(m))
            invokeRestart("muffleMessage")
        }
    )

    for (pattern in list(...)) {
        is_excluded <- isTRUE(attr(pattern, "excluded"))
        matched     <- any(grepl(pattern, actual_messages))

        if (is_excluded && matched) {
            fail(sprintf(
                "Message matched excluded pattern '%s'.\nActual messages:\n%s",
                pattern,
                paste("-", encodeString(actual_messages), collapse = "\n")
            ))
        } else if (!is_excluded && !matched) {
            fail(sprintf(
                "No message matched pattern '%s'.\nActual messages:\n%s",
                pattern,
                paste("-", encodeString(actual_messages), collapse = "\n")
            ))
        }
    }

    invisible(result)
}
