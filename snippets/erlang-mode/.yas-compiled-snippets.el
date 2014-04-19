;;; Compiled snippets and support files for `erlang-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'erlang-mode
                     '(("erlang" "%% .erlang - should live in the root of erlang project.  when erl is\n%% started with from dir which contains .erlang it will find the\n%% dependencies from deps folder and apps from Apps folder and adds\n%% them to the erlang path\n\nio:format(\"starting Erlang shell, executing .erlang ~n\").\nDir = filename:absname(\".\").\nio:format(\"current dir: ~p ~n\", [Dir]).\n\n%% get current path to figure out if the project incl \"apps\" dir.\nTokens = string:tokens(Dir, \"/\").\n\n%% find all the dir names in \"deps\" folder\nDeps = \ncase filelib:is_dir(\"deps\") of\n    true ->\n        Current = lists:delete(\".\", filename:split(filename:absname(\".\"))),\n        [filename:join(Current ++ [File] ++ [\"ebin\"]) || File <- filelib:wildcard(\"deps/*\"), string:str(File, \".\") =:= 0 ];\n    false -> []\nend.\n\n%% find apps\nApps = \ncase filelib:is_dir(\"apps\") of\n    true ->\n        Current = lists:delete(\".\", filename:split(filename:absname(\".\"))),\n        [filename:join(Current ++ [File] ++ [\"ebin\"]) || File <- filelib:wildcard(\"apps/*\"), string:str(File, \".\") =:= 0 ];\n    false -> []\nend.\n\nio:format(\"found dependencies: ~p ~n\", [Deps ++ Apps]).\n\n%% add paths to all deps and apps\n[code:add_patha(Dir) || Dir <- Deps ++ Apps].\n" "erlang" nil nil nil nil nil nil)))


;;; Do not edit! File generated at Sat Apr 19 08:58:50 2014
