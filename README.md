# Org link archive

This packages provides functionality to replace org-mode link at point with archived link produced by [archive.org](https://archive.org), so it will never be lost, even if orginal website go down.

The replaced link has to be in [org-mode link format](https://orgmode.org/manual/Link-Format.html).

### Installation

#### via package.el:

Put `org-link-archive.el` inside one of the folders defined in `'load-url`.

Add `(require 'org-link-archive.el)` to your Emacs config file.

#### via use-package:
TBA (after [melpa.org](https://melpa.org) accepted)

### Usage

For easy use bind it to keys of your choice. For example:

``` lisp
(define-key org-mode-map (kbd "C-x C-z") 'org-link-archive-at-point)
```

Point cursor on a link and run `org-link-archive-at-point`.

### Roadmap

- [ ] Publish on Melpa

### Contribution

PRs are welcome.
