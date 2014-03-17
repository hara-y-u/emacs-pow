# emacs-pow

Pow (http://pow.cx) apps management on emacs


## Install

**Emacs24 or above is supported.**

### Package

#### Melpa

Add following lines to your init file (like: `~/.emacs.d/init.el`), and restart Emacs. Just executing these expressions is also fine.

```cl
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(unless (package-installed-p 'pow)
  (package-install 'pow))
```

Or, execute `M-x list-packages` and install `pow` should work too.


### From Raw Repository

1. Clone to local

    ```
    git clone https://github.com/yukihr/emacs-pow.git
    ```

2. Add following line to your init file

    ```
    (add-to-list 'load-path "path/to/emacs-pow")
    (require 'pow)
    ```


## Basic Usage

To regsister your rack project to pow, exec `M-x pow-register-current-app` in your project.

And, `M-x pow-open-current-app` to open the project app in browser.

`M-x pow-restart-current-app` will flag `restart` current app.

`M-x pow-tail-current-log` will open `tail` buffer for app's Pow log.

`M-x list-pow-apps` lists all registered pow apps.


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


## Sample Settings

### Use with [`projectile-rails`](https://github.com/asok/projectile-rails)

```cl
(setq projectile-rails-keymap-prefix (kbd "C-c ;")) ; I prefer rirari keybind
(require 'projectile-rails)
(require 'pow)
(define-key projectile-rails-mode-map (kbd "C-c ; w") 'pow-restart-current-app)
(define-key projectile-rails-mode-map (kbd "C-c ; o") 'pow-open-current-app)
(define-key projectile-rails-mode-map (kbd "C-c ; f l") 'pow-tail-current-log)
(define-key projectile-rails-mode-map (kbd "C-c ; f L") 'pow-tail-current-app-log)
```


## Contributing

**If you are interested in project, pullreqs are welcome!**

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Edit sources, add your features, or fix bugs.
4. Check if all test passes (`make`) _make sure you are using emacs>=24 with `make emacs-version`_
5. Commit your changes (`git commit -am 'Add some feature'`)
6. Push to the branch (`git push origin my-new-feature`)
7. Create new Pull Request

**Bug reports, feature requests are also welcome! Please put on [issue](https://github.com/yukihr/emacs-pow/issues/new).**
