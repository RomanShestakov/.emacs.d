;;; elixir-settings.el --- provide settings for elixir mode.  -*- lexical-binding: t; -*-

;;; Commentary:
;; https://medium.com/@victor.nascimento/elixir-emacs-development-2023-edition-1a6ccc40629

;;; Code:

;; Emacs 30+ ships elixir-ts-mode built in; it still needs the tree-sitter
;; grammars fetched and compiled once per machine. From scratch, after the
;; (add-to-list 'treesit-language-source-alist ...) calls below have run
;; (e.g. by loading this file), either:
;;   M-x treesit-install-language-grammar RET elixir RET
;;   M-x treesit-install-language-grammar RET heex RET
;; or non-interactively:
;;   (treesit-install-language-grammar 'elixir)
;;   (treesit-install-language-grammar 'heex)
;; Installs to ~/.emacs.d/tree-sitter/libtree-sitter-{elixir,heex}.so.
(require 'treesit)
(add-to-list 'treesit-language-source-alist
             '(elixir "https://github.com/elixir-lang/tree-sitter-elixir"))
(add-to-list 'treesit-language-source-alist
             '(heex "https://github.com/phoenixframework/tree-sitter-heex"))

(use-package emacs
  :ensure nil
  :config
  (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode)))

;; project.el's default backend roots on the nearest .git, which is wrong
;; for a mix project nested inside an unrelated outer VCS checkout (e.g.
;; docket/ inside the borg monorepo, which has no .git of its own): eglot
;; then launches ElixirLS rooted at the OUTER checkout, where there's no
;; mix.exs, so it can never build the project or resolve anything in it --
;; not just deps, everything, including the buffer's own modules. Adding a
;; mix.exs-aware backend ahead of project-try-vc (negative depth) fixes
;; `project-current' for any buffer under a mix project, without touching
;; project detection anywhere else. (ElixirLS also has its own
;; `elixirLS.projectDir' initializationOption for this exact case, but
;; that's a single fixed path -- fine for one nested project, wrong the
;; moment there's a second one elsewhere under the same outer checkout;
;; fixing `project-current' itself instead handles any number of them, and
;; also fixes any other tool in this Emacs that calls `project-current'.)
(require 'project)
(defun my/project-try-mix (dir)
  "Find the nearest ancestor of DIR containing mix.exs, as a project root."
  (when-let ((root (locate-dominating-file dir "mix.exs")))
    (cons 'transient root)))
(add-hook 'project-find-functions #'my/project-try-mix -90)

;; requires ElixirLS's language_server.sh. Build from source (the README's
;; "Mix.install based release", the recommended path over the deprecated
;; .ez archives -- its launch script installs the right Elixir/OTP release
;; itself via Mix.install, so there's no local-toolchain version to match):
;;   git clone https://github.com/elixir-lsp/elixir-ls.git /tmp/elixir-ls
;;   cd /tmp/elixir-ls
;;   mix deps.get
;;   MIX_ENV=prod mix compile
;;   MIX_ENV=prod mix elixir_ls.release2 -o ~/.local/share/elixir-ls
;; (task name as of elixir-ls HEAD; older tags may still call it
;; `elixir_ls.release' with no "2" -- run `mix help | grep elixir_ls` in the
;; clone to check if this ever mismatches again).
(use-package eglot
  :ensure nil
  :config
  ;; ElixirLS defaults to MIX_ENV=test (README, "Troubleshooting"), which
  ;; silently hides any `only: :dev'-scoped dep (e.g. phoenix_live_reload)
  ;; from go-to-definition/completion -- force :dev to match what
  ;; `mix phx.server' actually runs under. No project-local config file is
  ;; involved: this initializationOptions plist is the whole mechanism, the
  ;; same one VS Code's settings.json -> "elixirLS.mixEnv" goes through.
  (add-to-list 'eglot-server-programs
               `(elixir-ts-mode
                 . (,(expand-file-name "~/.local/share/elixir-ls/language_server.sh")
                    :initializationOptions (:mixEnv "dev"))))
  (bind-key "M-." 'xref-find-definitions)
  (bind-key "M-," 'pop-tag-mark))

(defun my/elixir-prettify-symbols ()
  "Prettify common Elixir operators as Unicode glyphs."
  (setq prettify-symbols-alist
        (append '((">=" . ?≥)
                  ("<=" . ?≤)
                  ("!=" . ?≠)
                  ("==" . ?⩵)
                  ("=~" . ?≅)
                  ("<-" . ?←)
                  ("->" . ?→)
                  ("|>" . ?▷))
                prettify-symbols-alist)))

(defun my/elixir-format-on-save ()
  "Run `eglot-format' before save, only in this Elixir buffer."
  (add-hook 'before-save-hook #'eglot-format nil t))

;; elixir-ts-mode.el registers auto-mode-alist for .ex/.exs itself, but only
;; as a plain top-level form (no ;;;###autoload cookie), so it takes effect
;; only once the file is actually loaded -- which a plain `:hook' below
;; would never trigger (nothing yet points at elixir-ts-mode to load it).
;; `:mode' here registers auto-mode-alist eagerly, at init time, which is
;; also what makes use-package autoload the file when a matching filename is
;; opened.
(use-package elixir-ts-mode
  :ensure nil
  :mode (("\\.ex\\'" . elixir-ts-mode)
         ("\\.exs\\'" . elixir-ts-mode)
         ("mix\\.lock\\'" . elixir-ts-mode))
  :hook ((elixir-ts-mode . eglot-ensure)
         (elixir-ts-mode . prettify-symbols-mode)
         (elixir-ts-mode . my/elixir-prettify-symbols)
         (elixir-ts-mode . my/elixir-format-on-save)))

(use-package heex-ts-mode
  :ensure nil
  :mode "\\.heex\\'")

;; (use-package inf-elixir
;;   :bind (("C-c i i" . 'inf-elixir)
;;          ("C-c i p" . 'inf-elixir-project)
;;          ("C-c i l" . 'inf-elixir-send-line)
;;          ("C-c i r" . 'inf-elixir-send-region)
;;          ("C-c i b" . 'inf-elixir-send-buffer)
;;          ("C-c i R" . 'inf-elixir-reload-module)))

(provide 'elixir-settings)

;;; elixir-settings.el ends here
