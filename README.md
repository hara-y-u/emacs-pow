# emacs-pow

Pow (http://pow.cx) apps management on emacs


## Install

**Emacs24 or above is supported.**

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
    (require 'pow)
    ```


## Simple Usage

To regsister your rack project to pow, exec `M-x pow-register-current-app` in your project.

And `M-x pow-open-current-app` to open the project app in browser.

`M-x pow-restart-current-app` will flag `restart` current app.

`M-x list-pow-apps` lists all registered pow apps (only emacs-version >= 24 is supported).


For other command reference, try exec `M-x apropos-command RET pow-`.


## Listing Apps

`M-x list-pow-apps` lists all registered apps on pow. You can use following commands on each items in listing buffer.

| Action               | Command Name                   | Keybind                        |
|----------------------|--------------------------------|--------------------------------|
| Open app in browser  | `pow-app-list-open-app`        | <kbd>RET</kbd>, <kbd>C-m</kbd> |
| Open path in emacs   | `pow-app-list-find-app-path`   | <kbd>f</kbd>                   |
| Rename app           | `pow-app-list-rename-app`      | <kbd>m</kbd>                   |
| Unmark app           | `pow-app-list-mark-unmark`     | <kbd>u</kbd>                   |
| Unmark all app       | `pow-app-list-mark-unmark-all` | <kbd>U</kbd>                   |
| Mark app as _delete_ | `pow-app-list-mark-delete`     | <kbd>d</kbd>                   |
| Execute mark         | `pow-app-list-execute`         | <kbd>x</kbd>                   |
| Refresh buffer       | `pow-app-list-refresh`         | <kbd>r</kbd>                   |


## Contributing

**If you are interested in project, pullreqs are welcome!**

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Check if all test passes (`make`) _make sure you are using emacs>=24 with `make emacs-version`_
4. Commit your changes (`git commit -am 'Add some feature'`)
5. Push to the branch (`git push origin my-new-feature`)
6. Create new Pull Request

**Bug reports, feature requests are also welcome! Please put on [issue](https://github.com/yukihr/emacs-pow/issues/new).**
