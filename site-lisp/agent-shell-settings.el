
;;; Commentary:
;;; agent-shell-settings.el --- provide settings for agent-shell
;;; https://github.com/xenodium/agent-shell
;;; external deps: npm install -g @agentclientprotocol/claude-agent-acp

;; # Install nvm
;; curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.39.7/install.sh | bash

;; # Reload shell
;; source ~/.bashrc

;; # Install and use Node 20
;; nvm install 20
;; nvm use 20
;; nvm alias default 20  # make it permanent

;;; Code:

(use-package agent-shell
    :ensure t
;;    :ensure-system-package
    :config
    ;; Add agent installation configs here
 ;;   ((claude . "apt install claude-code")
    ;;   (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp"))
    ;; use claude-code for all shells
    (setq agent-shell-preferred-agent-config (agent-shell-anthropic-make-claude-code-config))
    ;; use viewport compose promp instead of the standard one
    (setq agent-shell-prefer-viewport-interaction t)
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t)))

(provide 'agent-shell-settings)

;;; agent-shell-settings.el ends here

