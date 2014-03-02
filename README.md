# emacs-pow

Pow (http://pow.cx) apps management on emacs


## Install

### Package

TODO


### From Raw Repository

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


## Simple Usage

To regsister your rack project to pow, exec `M-x pow-register-current-app` in your project.

And `M-x pow-open-current-app` to open the project app in browser.

`M-x pow-restart-current-app` will flag `restart` current app.

`M-x list-pow-apps` lists all registered pow apps (only emacs-version >= 24 is supported).


For other command reference, try exec `M-x apropos-command RET pow-`.


## Listing Apps

**Available on Emacs24**

`M-x list-pow-apps` lists all registered apps on pow. You can use following commands on each items in listing buffer.

| Action               | Command Name                   | Keybind                        |
|----------------------|--------------------------------|--------------------------------|
| Open app in browser  | `pow-app-list-open-app`        | <kbd>RET</kbd>, <kbd>C-m</kbd> |
| Open path in emacs   | `pow-app-list-find-app-path`   | <kbd>f</kbd>                   |
| Rename app           | `pow-app-list-rename-app`      | <kbd>n</kbd>                   |
| Unmark app           | `pow-app-list-mark-unmark`     | <kbd>u</kbd>                   |
| Unmark all app       | `pow-app-list-mark-unmark-all` | <kbd>U</kbd>                   |
| Mark app as _delete_ | `pow-app-list-mark-delete`     | <kbd>d</kbd>                   |
| Execute mark         | `pow-app-list-execute`         | <kbd>x</kbd>                   |
| Refresh buffer       | `pow-app-list-refresh`         | <kbd>r</kbd>                   |


## Contributing

**If you are interested in project, pullreqs are welcome!**

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

**Feature request is also welcome! Please put on [issue](https://github.com/yukihr/emacs-pow/issues/new).**
