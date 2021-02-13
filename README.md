# Org Link Archive

This packages provides functionality to replace org-mode link at point with archivised link produced by archive.org. The replaced link has to be in (org-mode link format)[https://orgmode.org/manual/Link-Format.html].

### Installation

Put `org-link-archive.el` inside one of the folders defined in `'load-url`.

Add `(require 'org-link-archive.el)` to your Emacs config file.

### Usage

For easy use bind it to keys of your choice. For example:

``` lisp
(define-key org-mode-map (kbd "C-x C-z") 'org-archive-link)
```

Point cursor on link and run `farynaio/org-archive-link`.

### Roadmap

- [ ] Publish on Melpa

### Contribution

PRs are welcome.