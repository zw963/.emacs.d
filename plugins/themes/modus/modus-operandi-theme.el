;; -*- lexical-binding: t; -*-

;;; modus-operandi-theme.el --- Elegant, highly legible theme with a white background -*- lexical-binding:t -*-

;; Copyright (C) 2019-2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/modus-themes
;; Keywords: faces, theme, accessibility

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  Please refer to the official Info manual for
;; further documentation (distributed with the themes, or available
;; at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:



(eval-and-compile
  (unless (and (fboundp 'require-theme)
               load-file-name
               (equal (file-name-directory load-file-name)
                      (expand-file-name "themes/" data-directory))
               (require-theme 'modus-themes t))
    (require 'modus-themes))

;;;###theme-autoload
  (deftheme modus-operandi
    "Elegant, highly legible theme with a white background.
Conforms with the highest legibility standard for color contrast
between background and foreground in any given piece of text,
which corresponds to a minimum contrast in relative luminance of
7:1 (WCAG AAA standard)."
    :background-mode 'light
    :kind 'color-scheme
    :family 'modus)

  (defconst modus-operandi-palette
    '(
;;; Basic values

      (bg-main          "#ffffff")
      (bg-dim           "#f2f2f2")
      (fg-main          "#000000")
      (fg-dim           "#595959")
      (fg-alt           "#193668")
      (bg-active        "#c4c4c4")
      (bg-inactive      "#e0e0e0")
      (border           "#9f9f9f")

;;; Common accent foregrounds

      (red             "#a60000")
      (red-warmer      "#972500")
      (red-cooler      "#a0132f")
      (red-faint       "#7f0000")
      (red-intense     "#d00000")
      (green           "#006800")
      (green-warmer    "#316500")
      (green-cooler    "#00663f")
      (green-faint     "#2a5045")
      (green-intense   "#008900")
      (yellow          "#6f5500")
      (yellow-warmer   "#884900")
      (yellow-cooler   "#7a4f2f")
      (yellow-faint    "#624416")
      (yellow-intense  "#808000")
      (blue            "#0031a9")
      (blue-warmer     "#3548cf")
      (blue-cooler     "#0000b0")
      (blue-faint      "#003497")
      (blue-intense    "#0000ff")
      (magenta         "#721045")
      (magenta-warmer  "#8f0075")
      (magenta-cooler  "#531ab6")
      (magenta-faint   "#7c318f")
      (magenta-intense "#dd22dd")
      (cyan            "#005e8b")
      (cyan-warmer     "#3f578f")
      (cyan-cooler     "#005f5f")
      (cyan-faint      "#005077")
      (cyan-intense    "#008899")

;;; Uncommon accent foregrounds

      (rust       "#8a290f")
      (gold       "#80601f")
      (olive      "#56692d")
      (slate      "#2f3f83")
      (indigo     "#4a3a8a")
      (maroon     "#731c52")
      (pink       "#7b435c")

;;; Common accent backgrounds

      (bg-red-intense     "#ff8f88")
      (bg-green-intense   "#8adf80")
      (bg-yellow-intense  "#f3d000")
      (bg-blue-intense    "#bfc9ff")
      (bg-magenta-intense "#dfa0f0")
      (bg-cyan-intense    "#a4d5f9")

      (bg-red-subtle      "#ffcfbf")
      (bg-green-subtle    "#b3fabf")
      (bg-yellow-subtle   "#fff576")
      (bg-blue-subtle     "#ccdfff")
      (bg-magenta-subtle  "#ffddff")
      (bg-cyan-subtle     "#bfefff")

      (bg-red-nuanced     "#ffe8e8")
      (bg-green-nuanced   "#e0f6e0")
      (bg-yellow-nuanced  "#f8f0d0")
      (bg-blue-nuanced    "#ecedff")
      (bg-magenta-nuanced "#f8e6f5")
      (bg-cyan-nuanced    "#e0f2fa")

;;; Uncommon accent background and foreground pairs

      (bg-clay     "#f1c8b5")
      (fg-clay     "#63192a")

      (bg-ochre    "#f0e3c0")
      (fg-ochre    "#573a30")

      (bg-lavender "#dfcdfa")
      (fg-lavender "#443379")

      (bg-sage     "#c0e7d4")
      (fg-sage     "#124b41")

;;; Graphs

      (bg-graph-red-0     "#ef7969")
      (bg-graph-red-1     "#ffaab4")
      (bg-graph-green-0   "#45c050")
      (bg-graph-green-1   "#75ef30")
      (bg-graph-yellow-0  "#ffcf00")
      (bg-graph-yellow-1  "#f9ff00")
      (bg-graph-blue-0    "#7f90ff")
      (bg-graph-blue-1    "#a6c0ff")
      (bg-graph-magenta-0 "#e07fff")
      (bg-graph-magenta-1 "#fad0ff")
      (bg-graph-cyan-0    "#70d3f0")
      (bg-graph-cyan-1    "#afefff")

;;; Special purpose

      (bg-completion       "#c0deff")
      (bg-hover            "#b2e4dc")
      (bg-hover-secondary  "#f5d0a0")
      (bg-hl-line          "#dae5ec")
      (bg-region           "#bdbdbd")
      (fg-region           "#000000")

      (bg-mode-line-active        "#c8c8c8")
      (fg-mode-line-active        "#000000")
      (border-mode-line-active    "#5a5a5a")
      (bg-mode-line-inactive      "#e6e6e6")
      (fg-mode-line-inactive      "#585858")
      (border-mode-line-inactive  "#a3a3a3")

      (modeline-err     "#7f0000")
      (modeline-warning "#5f0070")
      (modeline-info    "#002580")

      (bg-tab-bar      "#dfdfdf")
      (bg-tab-current  "#ffffff")
      (bg-tab-other    "#c2c2c2")

;;; Diffs

      (bg-added           "#c1f2d1")
      (bg-added-faint     "#d8f8e1")
      (bg-added-refine    "#aee5be")
      (bg-added-fringe    "#6cc06c")
      (fg-added           "#005000")
      (fg-added-intense   "#006700")

      (bg-changed         "#ffdfa9")
      (bg-changed-faint   "#ffefbf")
      (bg-changed-refine  "#fac090")
      (bg-changed-fringe  "#d7c20a")
      (fg-changed         "#553d00")
      (fg-changed-intense "#655000")

      (bg-removed         "#ffd8d5")
      (bg-removed-faint   "#ffe9e9")
      (bg-removed-refine  "#f3b5af")
      (bg-removed-fringe  "#d84a4f")
      (fg-removed         "#8f1313")
      (fg-removed-intense "#aa2222")

      (bg-diff-context    "#f3f3f3")

;;; Paren match

      (bg-paren-match        "#5fcfff")
      (fg-paren-match        fg-main)
      (bg-paren-expression   "#efd3f5")
      (underline-paren-match unspecified)

;;; Mappings

;;;; General mappings

      (fringe bg-dim)
      (cursor fg-main)

      (keybind blue-cooler)
      (name magenta)
      (identifier yellow-cooler)

      (err red)
      (warning yellow-warmer)
      (info cyan-cooler)

      (underline-err red-intense)
      (underline-warning yellow-intense)
      (underline-note cyan-intense)

      (bg-prominent-err bg-red-intense)
      (fg-prominent-err fg-main)
      (bg-prominent-warning bg-yellow-intense)
      (fg-prominent-warning fg-main)
      (bg-prominent-note bg-cyan-intense)
      (fg-prominent-note fg-main)

      (bg-active-argument bg-yellow-nuanced)
      (fg-active-argument yellow-warmer)
      (bg-active-value bg-cyan-nuanced)
      (fg-active-value cyan-warmer)

;;;; Code mappings

      (bracket fg-main)
      (builtin magenta-warmer)
      (comment fg-dim)
      (constant blue-cooler)
      (delimiter fg-main)
      (docmarkup magenta-faint)
      (docstring green-faint)
      (fnname magenta)
      (keyword magenta-cooler)
      (number fg-main)
      (operator fg-main)
      (preprocessor red-cooler)
      (property cyan)
      (punctuation fg-main)
      (rx-backslash magenta)
      (rx-construct green-cooler)
      (string blue-warmer)
      (type cyan-cooler)
      (variable cyan)

;;;; Accent mappings

      (accent-0 blue)
      (accent-1 magenta-warmer)
      (accent-2 cyan)
      (accent-3 red)

;;;; Button mappings

      (fg-button-active fg-main)
      (fg-button-inactive fg-dim)
      (bg-button-active bg-active)
      (bg-button-inactive bg-dim)

;;;; Completion mappings

      (fg-completion-match-0 blue)
      (fg-completion-match-1 magenta-warmer)
      (fg-completion-match-2 cyan)
      (fg-completion-match-3 red)
      (bg-completion-match-0 unspecified)
      (bg-completion-match-1 unspecified)
      (bg-completion-match-2 unspecified)
      (bg-completion-match-3 unspecified)

;;;; Date mappings

      (date-common cyan)
      (date-deadline red-cooler)
      (date-deadline-subtle red-faint)
      (date-event fg-alt)
      (date-holiday red)
      (date-holiday-other blue)
      (date-now fg-main)
      (date-range fg-alt)
      (date-scheduled yellow)
      (date-scheduled-subtle yellow-faint)
      (date-weekday cyan)
      (date-weekend magenta)

;;;; Line number mappings

      (fg-line-number-inactive fg-dim)
      (fg-line-number-active fg-main)
      (bg-line-number-inactive bg-dim)
      (bg-line-number-active bg-active)

;;;; Link mappings

      (fg-link blue-warmer)
      (bg-link unspecified)
      (underline-link blue-warmer)

      (fg-link-symbolic cyan)
      (bg-link-symbolic unspecified)
      (underline-link-symbolic cyan)

      (fg-link-visited magenta)
      (bg-link-visited unspecified)
      (underline-link-visited magenta)

;;;; Mail mappings

      (mail-cite-0 blue-faint)
      (mail-cite-1 yellow-warmer)
      (mail-cite-2 cyan-cooler)
      (mail-cite-3 red-cooler)
      (mail-part cyan)
      (mail-recipient magenta-cooler)
      (mail-subject magenta-warmer)
      (mail-other magenta-faint)

;;;; Mark mappings

      (bg-mark-delete bg-red-subtle)
      (fg-mark-delete red)
      (bg-mark-select bg-cyan-subtle)
      (fg-mark-select cyan)
      (bg-mark-other bg-yellow-subtle)
      (fg-mark-other yellow)

;;;; Prompt mappings

      (fg-prompt cyan-cooler)
      (bg-prompt unspecified)

;;;; Prose mappings

      (bg-prose-block-delimiter bg-dim)
      (fg-prose-block-delimiter fg-dim)
      (bg-prose-block-contents bg-dim)

      (bg-prose-code unspecified)
      (fg-prose-code cyan-cooler)

      (bg-prose-macro unspecified)
      (fg-prose-macro magenta-cooler)

      (bg-prose-verbatim unspecified)
      (fg-prose-verbatim magenta-warmer)

      (prose-done green)
      (prose-todo red)

      (prose-metadata fg-dim)
      (prose-metadata-value fg-alt)

      (prose-table fg-alt)
      (prose-table-formula magenta-warmer)

      (prose-tag magenta-faint)

;;;; Rainbow mappings

      (rainbow-0 fg-main)
      (rainbow-1 magenta-intense)
      (rainbow-2 cyan-intense)
      (rainbow-3 red-warmer)
      (rainbow-4 yellow-intense)
      (rainbow-5 magenta-cooler)
      (rainbow-6 green-intense)
      (rainbow-7 blue-warmer)
      (rainbow-8 magenta-warmer)

;;;; Search mappings

      (bg-search-current bg-yellow-intense)
      (bg-search-lazy bg-cyan-intense)
      (bg-search-replace bg-red-intense)

      (bg-search-rx-group-0 bg-blue-intense)
      (bg-search-rx-group-1 bg-green-intense)
      (bg-search-rx-group-2 bg-red-subtle)
      (bg-search-rx-group-3 bg-magenta-subtle)

;;;; Space mappings

      (bg-space unspecified)
      (fg-space border)
      (bg-space-err bg-red-intense)

;;;; Terminal mappings

      (bg-term-black           "#000000")
      (fg-term-black           "#000000")
      (bg-term-black-bright    "#595959")
      (fg-term-black-bright    "#595959")

      (bg-term-red             red)
      (fg-term-red             red)
      (bg-term-red-bright      red-warmer)
      (fg-term-red-bright      red-warmer)

      (bg-term-green           green)
      (fg-term-green           green)
      (bg-term-green-bright    green-cooler)
      (fg-term-green-bright    green-cooler)

      (bg-term-yellow          yellow)
      (fg-term-yellow          yellow)
      (bg-term-yellow-bright   yellow-warmer)
      (fg-term-yellow-bright   yellow-warmer)

      (bg-term-blue            blue)
      (fg-term-blue            blue)
      (bg-term-blue-bright     blue-warmer)
      (fg-term-blue-bright     blue-warmer)

      (bg-term-magenta         magenta)
      (fg-term-magenta         magenta)
      (bg-term-magenta-bright  magenta-cooler)
      (fg-term-magenta-bright  magenta-cooler)

      (bg-term-cyan            cyan)
      (fg-term-cyan            cyan)
      (bg-term-cyan-bright     cyan-cooler)
      (fg-term-cyan-bright     cyan-cooler)

      (bg-term-white           "#a6a6a6")
      (fg-term-white           "#a6a6a6")
      (bg-term-white-bright    "#ffffff")
      (fg-term-white-bright    "#ffffff")

;;;; Heading mappings

      (fg-heading-0 cyan-cooler)
      (fg-heading-1 fg-main)
      (fg-heading-2 yellow-faint)
      (fg-heading-3 fg-alt)
      (fg-heading-4 magenta)
      (fg-heading-5 green-faint)
      (fg-heading-6 red-faint)
      (fg-heading-7 cyan-warmer)
      (fg-heading-8 fg-dim)

      (bg-heading-0 unspecified)
      (bg-heading-1 unspecified)
      (bg-heading-2 unspecified)
      (bg-heading-3 unspecified)
      (bg-heading-4 unspecified)
      (bg-heading-5 unspecified)
      (bg-heading-6 unspecified)
      (bg-heading-7 unspecified)
      (bg-heading-8 unspecified)

      (overline-heading-0 unspecified)
      (overline-heading-1 unspecified)
      (overline-heading-2 unspecified)
      (overline-heading-3 unspecified)
      (overline-heading-4 unspecified)
      (overline-heading-5 unspecified)
      (overline-heading-6 unspecified)
      (overline-heading-7 unspecified)
      (overline-heading-8 unspecified))
    "The entire palette of the `modus-operandi' theme.

Named colors have the form (COLOR-NAME HEX-VALUE) with the former
as a symbol and the latter as a string.

Semantic color mappings have the form (MAPPING-NAME COLOR-NAME)
with both as symbols.  The latter is a named color that already
exists in the palette and is associated with a HEX-VALUE.")

  (defcustom modus-operandi-palette-user nil
    "Like the `modus-operandi-palette' for user-defined entries.
This is meant to extend the palette with custom named colors and/or
semantic palette mappings.  Those may then be used in combination with
palette overrides (also see `modus-themes-common-palette-overrides' and
`modus-operandi-palette-overrides')."
    :group 'modus-themes
    :package-version '(modus-themes . "4.5.0")
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Option to extend the palette for use with overrides"))

  (defcustom modus-operandi-palette-overrides nil
    "Overrides for `modus-operandi-palette'.

Mirror the elements of the aforementioned palette, overriding
their value.

For overrides that are shared across all of the Modus themes,
refer to `modus-themes-common-palette-overrides'.

Theme-specific overrides take precedence over shared overrides.
The idea of common overrides is to change semantic color
mappings, such as to make the cursor red.  Wherea theme-specific
overrides can also be used to change the value of a named color,
such as what hexadecimal RGB value the red-warmer symbol
represents."
    :group 'modus-themes
    :package-version '(modus-themes . "4.0.0")
    :version "30.1"
    :type '(repeat (list symbol (choice symbol string)))
    :set #'modus-themes--set-option
    :initialize #'custom-initialize-default
    :link '(info-link "(modus-themes) Palette overrides"))

  (modus-themes-theme modus-operandi
                      modus-operandi-palette
                      modus-operandi-palette-overrides)

  (provide-theme 'modus-operandi))

;;; modus-operandi-theme.el ends here
