(provide 'scala_mode_config)

(add-to-list 'load-path "~/.emacs.d/scala/scala-mode2/")
(add-to-list 'load-path "~/.emacs.d/ensime/ensime_2.10.2-0.9.8.10/elisp")
(setq exec-paths (append exec-path (list "/usr/bin/sbt")))

;; This step causes the ensime-mode to be started whenever
;; scala-mode is started for a buffer. You may have to customize this step
;; if you're not using the standard scala mode.
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'ensime)
(require 'scala-mode2)

