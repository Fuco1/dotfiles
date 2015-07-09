define_browser_object_class(
    "history-url", null,
    function (I, prompt) {
        check_buffer (I.buffer, content_buffer);
        var result = yield I.buffer.window.minibuffer.read_url(
            $prompt = prompt,  $use_webjumps = false, $use_history = true, $use_bookmarks = false);
        yield co_return (result);
    });

interactive("my-find-url-from-history",
            "Find a page from history in the current buffer",
            "find-url",
            $browser_object = browser_object_history_url,
            $prompt = "Find url from history in current buffer");

interactive("my-find-url-from-history-new-buffer",
            "Find a page from history in the current buffer",
            "find-url-new-buffer",
            $browser_object = browser_object_history_url,
            $prompt = "Find url from history in new buffer");

define_key(content_buffer_normal_keymap, "h", "my-find-url-from-history-new-buffer");
define_key(content_buffer_normal_keymap, "H", "my-find-url-from-history");

provide("browse-history");
