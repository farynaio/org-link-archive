# Org Url Archive

This packages provides functionality to replace all occourances of org-mode URL at point with archivised URL produced by archive.org. The replaced URL has to be in org-mode format - "[[url][optional-description]]".

### Installation

Put `org-url-archive.el` inside one of the folders defined in `'load-url`.

Add `(require 'org-url-archive.el)` to your Emacs config file.

### Usage

For easy use bind it to keys of your choice. For example

``` lisp
(define-key org-mode-map (kbd "C-x C-z") 'farynaio/org-archive-url)
```

### Roadmap

- [ ] Publish on Melpa

### Contribution

PRs are welcome.