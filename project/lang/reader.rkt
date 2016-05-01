#lang s-exp syntax/module-reader

literally/project
#:info (λ (key default default-filter)
         (case key
           [(drracket:toolbar-buttons)
            (dynamic-require 'literally/private/ide-buttons 'drracket-buttons)]
           [else
            (default-filter key default)]))
