# emacs-pow

Pow (http://pow.cx) apps management on emacs


## Install

### From Repository

1. Clone to local

    ```
    git clone https://github.com/yukihr/emacs-pow.git
    ```

2. Add following line to your init file (like: `~/.emacs.d/init.el`)

    ```
    (add-to-list 'load-path "path/to/emacs-pow")
    (if (>= (string-to-number (car (split-string emacs-version "\\.")))
            24)
        (require 'pow)
      (require 'pow-core))
    ```

### Package

TODO


## Simple Usage

To regsister your rack project to pow, exec `M-x pow-register-current-app` in your project. `M-x list-pow-apps` lists all registered pow apps (only emacs-version >= 24 is supported). For command reference, try exec `M-x apropos-command RET pow`.


## Listing Apps

**Available on Emacs24**

`M-x list-pow-apps` lists all registered apps on pow. You can use following commands on each items in listing buffer.

Action               | Command Name                   | Keybind
---------------------|--------------------------------|-------------
Open app in browser  | `pow-app-list-open-app`        | `RET`, `C-m`
Open path in emacs   | `pow-app-list-find-app-path`   | `f`
Unmark app           | `pow-app-list-mark-unmark`     | `u`
Unmark all app       | `pow-app-list-mark-unmark-all` | `U`
Mark app as _delete_ | `pow-app-list-mark-delete`     | `d`
Execute mark         | `pow-app-list-execute`         | `x`
Refresh buffer       | `pow-app-list-refresh`         | `r`


## Contributing

**If you are interested in project, pullreqs are wellcome!**

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request
