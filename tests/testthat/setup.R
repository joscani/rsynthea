# Reset .REC$e between tests so direct process_state() calls don't share state.
withr::defer(.REC$e <- NULL, teardown_env())
