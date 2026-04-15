
;;; Commentary:
;;; claude-settings.el --- provide settings for claude-aig mode.

;;; Code:

(use-package vterm
  :ensure t)

(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :rev :newest)
  :bind ("C-c C-l" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (claude-code-ide-emacs-tools-setup)) ; Optionally enable Emacs MCP tools

(provide 'claude-settings)

;;; claude-settings.el ends here
