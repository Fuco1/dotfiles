/// sessions
require("session.js");
session_auto_save_auto_load = true;

require("browse-history.js");

require("favicon");
read_buffer_show_icons = true;

/// variables
minibuffer_completion_rows = 20;
read_url_handler_list = [read_url_make_default_webjump_handler("g")];

/// minibuffer
// contribute upstream
// TODO: take min minibuffer_completion_rows / current number of items (= go to the end/beginning)
interactive("minibuffer-next-page", null,
    function (I) { minibuffer_complete(I.window, minibuffer_completion_rows); });
interactive("minibuffer-previous-page", null,
    function (I) { minibuffer_complete(I.window, -minibuffer_completion_rows); });

define_key(minibuffer_keymap, "C-v", "minibuffer-next-page");
define_key(minibuffer_keymap, "M-v", "minibuffer-previous-page");

function my_load_url_in_new_buffer(url, ctx) {
    create_buffer_in_current_window(
        buffer_creator(content_buffer,
                       $opener = ctx,
                       $load = url),
        OPEN_NEW_BUFFER, false);
}
url_remoting_fn = my_load_url_in_new_buffer;

/// modifiers
modifiers.M = new modifier(
  function (event) { return event.metaKey; },
  function (event) { event.metaKey = true; });

/// emacs as external editor
editor_shell_command = "emacsclient -c";

/// global settings/switches
mode_line_mode(false);

/// buffers ordered by recency
interactive("switch-to-recent-buffer",
    "Prompt for a buffer and switch to it, displaying the list in last-visited order.",
            function (I) {
        switch_to_buffer(
            I.window,
            (yield I.minibuffer.read_buffer(
                $prompt = "Switch to buffer:",
                $buffers = I.window.buffers.buffer_history,
                $default = (I.window.buffers.count > 1 ?
                            I.window.buffers.buffer_history[1] :
                            I.buffer))));
    });

/// keys
define_key(default_global_keymap, "C-'", "switch-to-recent-buffer");

[content_buffer_normal_keymap, special_buffer_keymap].map(function(map) {
    define_key(map, "j", "cmd_scrollLineDown");
    define_key(map, "J", "cmd_scrollPageDown");
    define_key(map, "k", "cmd_scrollLineUp");
    define_key(map, "K", "cmd_scrollPageUp");
    define_key(map, "u", "cmd_scrollTop");
    define_key(map, "d", "cmd_scrollBottom");
});

define_key(content_buffer_normal_keymap, "v", "follow-new-buffer-background");
define_key(content_buffer_normal_keymap, "V", "follow-new-buffer");

define_key(content_buffer_normal_keymap, "g", "find-alternate-url-new-buffer");
define_key(content_buffer_normal_keymap, "G", "find-alternate-url");

/// DOM/element selections
require("element.js");
define_browser_object_class("list",
                            "Browser object class for selecting a list node.",
                            xpath_browser_object_handler("//ul | //ol"),
                            $hint = "select (un)ordered list");

// how to make default choice of object for a command.
// interactive("follow-yc-links",
//             "follow the news link on yc",
//             "follow",
//             $browser_object = browser_object_yc_links);

define_key(content_buffer_normal_keymap, "* l", "browser-object-list");

/// helpers
interactive("switch-to-other-buffer",
            "Switch to the previously opened buffer",
            function (I) {
                var blist = I.window.buffers.buffer_history;
                if (blist.length > 1)
                  switch_to_buffer(I.window, blist[1]);
            });
define_key(default_global_keymap, "C-x C-a", "switch-to-other-buffer");

/// revive buffers
var kill_buffer_original = kill_buffer_original || kill_buffer;

var killed_buffers = [];

kill_buffer = function (buffer, force) {
    if (buffer.display_uri_string) {
        killed_buffers.push({url: buffer.display_uri_string,
                             title: buffer.title,
                             history: buffer.web_navigation.sessionHistory});
    }

    kill_buffer_original(buffer,force);
};

interactive("restore-killed-buffer-url", "Loads url from a previously killed buffer",
            function restore_killed_buffer_url (I) {
                if (killed_buffers.length !== 0) {
                    var killed_buffer = yield I.minibuffer.read(
                        $prompt = "Restore killed buffer url:",
                        $completer = new all_word_completer($completions = killed_buffers,
                                                            $get_string = function (x) x.url,
                                                            $get_description = function (x) x.title),
                        $default_completion = killed_buffers[killed_buffers.length - 1],
                        $auto_complete = "url",
                        $auto_complete_initial = true,
                        $auto_complete_delay = 0,
                        $require_match = true
                    );

                    load_url_in_new_buffer(killed_buffer.url);

                    var buf = I.window.buffers.current;
                    buf.web_navigation.sessionHistory = killed_buffer.history;
                    var original_index = buf.web_navigation.sessionHistory.index;
                    buf.web_navigation.gotoIndex(original_index);

                } else {
                    I.window.minibuffer.message("No killed buffer urls");
                }
            });
define_key(default_global_keymap, "C-T", "restore-killed-buffer-url");

/// downloads
remove_hook("download_added_hook", open_download_buffer_automatically);

let my_download_path = get_home_directory() + "download";

function update_save_path (info) {
    my_download_path = info.target_file.parent.path;
}

add_hook("download_added_hook", update_save_path);

suggest_save_path_from_file_name = function (filename, buffer) {
    let file = make_file(my_download_path);
    file.append(filename);
    return file.path;
}


/// webjumps
define_opensearch_webjump("g", "google.xml");
define_opensearch_webjump("w", "wikipedia.xml");
define_opensearch_webjump("wg", "wikipedia-de.xml");
define_opensearch_webjump("wi", "wikipedia-it.xml");
define_opensearch_webjump("wcs", "wikipedia-cs.xml");
define_opensearch_webjump("wl", "wikipedia-la.xml");
define_opensearch_webjump("wr", "wikipedia-ru.xml");
define_opensearch_webjump("wd", "wiktionary.xml");
define_opensearch_webjump("y", "youtube.xml");

define_webjump("imdb", "http://www.imdb.com/find?q=%s");
define_webjump("r", "http://reddit.com/r/%s");
define_webjump("wa", "http://www.wolframalpha.com/input/?i=%s");
define_webjump("gi", "http://www.google.com/images?q=%s");
define_webjump("wf", "http://fallout.wikia.com/wiki/index.php?search=%s&fulltext=0");
define_webjump("e", "http://www.etymonline.com/index.php?allowed_in_frame=0&search=%s&searchmode=none");
define_webjump("tpb", "http://thepiratebay.se/s/?q=%s&page=0&orderby=99");
define_webjump("juls", "http://slovnik.juls.savba.sk/?w=%s&s=exact&c=ia8c&d=kssj4&d=psp&ie=utf-8&oe=utf-8");
define_webjump("l", "http://linguax.com/lexica/old.php?searchedLG=%s");
define_webjump("ud", "http://www.urbandictionary.com/define.php?term=%s");
define_webjump("lide", "https://is.muni.cz/auth/lide/?searchid=%s&Hledat=Hledat");
define_webjump("df", "http://df.magmawiki.com/index.php?search=%s&go=Go");
