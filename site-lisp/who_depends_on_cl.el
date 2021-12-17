;;https://github.com/hlissner/doom-emacs/issues/3372#issuecomment-643567913
;; code to detect what installed packages depend on depricated cl package
(require 'loadhist)
(file-dependents (feature-file 'cl))
