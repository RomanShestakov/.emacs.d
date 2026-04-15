
;;; Commentary:
;;; agent-shell-settings.el --- provide settings for agent-shell
;;; https://github.com/xenodium/agent-shell
;;; external deps: npm install -g @agentclientprotocol/claude-agent-acp

;;; Code:

(use-package agent-shell
    :ensure t
;;    :ensure-system-package
    :config
    ;; Add agent installation configs here
 ;;   ((claude . "apt install claude-code")
 ;;   (claude-agent-acp . "npm install -g @agentclientprotocol/claude-agent-acp"))
    (setq agent-shell-anthropic-authentication
          (agent-shell-anthropic-make-authentication :login t)))

(provide 'agent-shell-settings)

;;; agent-shell-settings.el ends here

