;;; -*- Mode: Lisp; Package: COMMON-LISP-USER -*-

;;;  (c) copyright 2004-2005 by
;;;           Robert Strandh (strandh@labri.fr)
;;;  (c) copyright 2005 by
;;;           Matthieu Villeneuve (matthieu.villeneuve@free.fr)
;;;  (c) copyright 2006 by
;;;           Troels Henriksen (athas@sigkill.dk)

;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

;;; Package definitions for the DREI editing component.

(in-package :cl-user)

(defpackage :drei-buffer
  (:use :clim-lisp :flexichain :binseq :esa-utils)
  ;; Kludge to remove symbol conflicts.

  (:import-from :esa-io :buffer)
  (:import-from :esa :esa-command-table)
  (:export #:buffer #:standard-buffer
           #:mark #:left-sticky-mark #:right-sticky-mark
           #:standard-left-sticky-mark #:standard-right-sticky-mark
           #:make-buffer-mark #:clone-mark
           #:condition-offset
           #:no-such-offset #:offset-before-beginning #:offset-after-end
           #:invalid-motion #:motion-before-beginning #:motion-after-end
           #:size #:number-of-lines
           #:offset #:mark< #:mark<= #:mark= #:mark> #:mark>=
           #:forward-object
           #:backward-object
           #:forward-line-start #:backward-line-start
           #:forward-line-end #:backward-line-end
           #:beginning-of-buffer #:end-of-buffer
           #:beginning-of-buffer-p #:end-of-buffer-p
           #:beginning-of-line #:end-of-line
           #:beginning-of-line-p #:end-of-line-p
           #:buffer-line-number #:buffer-column-number
           #:line-number #:column-number
           #:insert-buffer-object #:insert-buffer-sequence
           #:buffer-substring
           #:insert-object #:insert-sequence
           #:delete-buffer-range #:delete-range
           #:delete-region
           #:buffer-object #:buffer-sequence
           #:object-before #:object-after #:region-to-sequence #:region-to-string
           #:observable-buffer-mixin
           #:binseq-buffer #:obinseq-buffer #:binseq2-buffer
           #:persistent-left-sticky-mark #:persistent-right-sticky-mark
           #:persistent-left-sticky-line-mark #:persistent-right-sticky-line-mark
           #:p-line-mark-mixin #:buffer-line-offset
           #:delegating-buffer #:implementation
           #:delegating-left-sticky-mark #:delegating-right-sticky-mark)
  (:documentation "An implementation of the buffer protocol. This
package is quite low-level, not syntax-aware, not CLIM-aware and
not user-oriented at all."))

(defpackage :drei-undo
  (:use :clim-lisp :drei-buffer :flexichain)
  (:export #:no-more-undo
           #:undo-tree #:standard-undo-tree
           #:undo-record #:standard-undo-record
           #:add-undo #:flip-undo-record #:undo #:redo))

(defpackage :drei-kill-ring
  (:use :clim-lisp :flexichain)
  (:export #:kill-ring #:kill-ring-chain #:kill-ring-cursor
           #:empty-kill-ring
           #:kill-ring-length #:kill-ring-max-size
           #:append-next-p
           #:reset-yank-position #:rotate-yank-position #:kill-ring-yank
           #:kill-ring-standard-push #:kill-ring-concatenating-push
           #:kill-ring-reverse-concatenating-push
           #:*kill-ring*)
  (:documentation "An implementation of a kill ring."))

(defpackage :drei-base
  (:use :clim-lisp :drei-buffer :drei-kill-ring :esa-buffer :esa-utils)
  (:export #:as-region
           #:as-full-region
           #:as-offsets
           #:do-buffer-region
           #:do-buffer-region-lines
           #:previous-line #:next-line
           #:open-line
           #:delete-line
           #:extract-line
           #:lines-in-region
           #:extract-lines-in-region
           #:empty-line-p
           #:line-indentation
           #:buffer-display-column
           #:number-of-lines-in-region
           #:constituentp
           #:just-n-spaces
           #:move-to-column
           #:kill-region
           #:in-place-buffer-substring
           #:fill-string-from-buffer
           #:buffer-find-nonchar
           #:offset-beginning-of-line-p
           #:offset-end-of-line-p
           #:end-of-line-offset
           #:extract-region
           #:buffer-whitespacep
           #:buffer-region-case
           #:buffer-looking-at #:looking-at
           #:buffer-search-forward #:buffer-search-backward
           #:buffer-re-search-forward #:buffer-re-search-backward
           #:search-forward #:search-backward
           #:re-search-forward #:re-search-backward
           #:buffer-search-word-forward #:search-word-forward
           #:buffer-search-word-backward #:search-word-backward
           #:downcase-buffer-region #:downcase-region
           #:upcase-buffer-region #:upcase-region
           #:capitalize-buffer-region #:capitalize-region
           #:tabify-region #:untabify-region
           #:narrowed-mark-mixin #:narrowed-left-sticky-mark #:narrowed-right-sticky-mark
           #:make-narrowed-mark #:make-backward-narrowed-mark #:make-forward-narrowed-mark
           #:narrow-mark #:unnarrow-mark)
  (:documentation "Basic functionality built on top of the buffer
protocol. Here is where we define slightly higher level
functions, that can be directly implemented in terms of the
buffer protocol, but that are not, strictly speaking, part of
that protocol. The functions in this package are not
syntax-aware, and are thus limited in what they can do. They
percieve the buffer as little more than a sequence of
characters."))

(defpackage :drei-abbrev
  (:use :clim-lisp :clim :drei-buffer :drei-base)
  (:export #:abbrev-expander #:dictionary-abbrev-expander #:dictionary
           #:expand-abbrev #:abbrev-mixin #:possibly-expand-abbrev
           #:add-abbrev))

(defpackage :drei-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base :flexichain :esa-utils)
  (:export #:syntax #:syntax-command-tables #:updater-fns #:update-parse #:syntaxp
           #:define-syntax #:*default-syntax*
           #:syntax-command-table #:additional-command-tables #:define-syntax-command-table
           #:eval-option
           #:define-option-for-syntax
           #:current-attributes-for-syntax
           #:make-attribute-line
           #:syntax-from-name
           #:update-syntax
           #:grammar #:grammar-rule #:add-rule
           #:parser #:initial-state
           #:advance-parse
           #:parse-tree #:start-offset #:end-offset
           #:lexer #:nb-lexemes #:lexeme #:insert-lexeme
           #:incremental-lexer #:next-lexeme
           #:delete-invalid-lexemes #:inter-lexeme-object-p
           #:skip-inter-lexeme-objects #:update-lex
           #:parse-stack-top #:target-parse-tree #:parse-state-empty-p
           #:parse-stack-next #:parse-stack-symbol
           #:parse-stack-parse-trees #:map-over-parse-trees
           #:no-such-operation
           #:name-for-info-pane
           #:display-syntax-name
           #:syntax-line-indentation
           #:eval-defun
           #:syntax-line-comment-string
           #:line-comment-region #:comment-region
           #:line-uncomment-region #:uncomment-region
           #:word-constituentp
           #:whitespacep
           #:page-delimiter
           #:paragraph-delimiter)
  (:documentation "The syntax protocol. Contains functions that
  can be used to implement higher-level operations on buffer
  contents."))

(defpackage :drei
  (:use :clim-lisp :clim-sys :clim :drei-buffer :drei-base :drei-abbrev
        :drei-syntax :flexichain :drei-undo :esa-buffer :esa-io :esa
        :esa-utils :drei-kill-ring)
  (:import-from :climi :change-stream-space-requirements)
  (:export #:drei-buffer #:needs-saving
           #:filepath #:file-saved-p #:file-write-time
           #:read-only-p #:buffer-read-only
           
           #:display-drei #:display-drei-pane #:display-drei-area #:full-redisplay
           #:offset-to-screen-position
           #:page-down #:page-up
           #:isearch-state #:search-string #:search-mark
           #:search-forward-p #:search-success-p
           #:query-replace-state #:string1 #:string2 #:targets #:occurrences

           ;; Undo.
           #:undo-mixin #:undo-tree #:undo-accumulate #:performing-undo
           #:drei-undo-record
           #:simple-undo-record
           #:insert-record
           #:delete-record
           #:compound-record
           #:with-undo #:clear-undo-history
           
           #:drei-buffer

           ;; Signals and conditions.
           #:user-condition-mixin
           #:buffer-read-only
           #:buffer-single-line
           #:no-available-minibuffer

           ;; Views and their facilities.
           #:drei-view #:modified-p #:no-cursors
           
           #:drei-buffer-view #:buffer #:top #:bot #:buffer-view-p
           #:lines #:single-line-mixin
           #:buffer-line #:start-mark #:end-mark #:line-length #:chunks
           #:line-containing-offset #:offset-in-line-p
           #:buffer-view-pump-state-for-offset
           #:buffer-view-stroke-pump
           
           #:drei-syntax-view #:syntax #:syntax-view-p
           #:pump-state-for-offset-with-syntax
           #:stroke-pump-with-syntax
           
           #:point-mark-view #:point-mark-view-p
           
           #:textual-drei-syntax-view
           #:tab-space-count #:space-width #:tab-width #:use-tabs
           #:auto-fill-mode #:auto-fill-column
           #:isearch-mode #:isearch-states #:isearch-previous-string
           #:query-replace-mode
           #:region-visible-p
           #:dabbrev-expansion-mark
           #:original-prefix
           #:prefix-start-offset
           #:overwrite-mode
           #:goal-column

           #:invalidate-strokes

           #:view-command-tables
           #:use-editor-commands-p
           #:synchronize-view
           #:create-view-cursors
           #:clear-redisplay-information
           #:clone-view
           #:make-syntax-for-view
           
           ;; DREI command tables.
           #:comment-table #:deletion-table #:editing-table
           #:fill-table #:indent-table #:marking-table #:case-table
           #:movement-table #:search-table #:info-table #:self-insert-table
           #:view-table
           #:editor-table #:exclusive-gadget-table #:exclusive-input-editor-table

           #:minibuffer

           #:drei #:editor-pane
           #:drei-pane #:drei-gadget-pane #:drei-area
           #:handling-drei-conditions #:handle-drei-condition
           #:execute-drei-command

           ;; Redisplay engine.
           #:display-drei-view-contents #:display-drei-view-cursor
           #:handle-redisplay
           #:face #:make-face #:face-ink #:face-style
           #:drawing-options #:make-drawing-options
           #:drawing-options-face #:drawing-options-function
           #:drawing-options-equal #:+default-drawing-options+
           #:stroke-start-offset #:stroke-end-offset
           #:stroke-drawing-options
           
           #:pump-state-for-offset #:stroke-pump
           #:object-drawer #:*maximum-chunk-size*

           #:+roman-face+ #:+roman-face-drawing-options+
           #:+italic-face+ #:+italic-face-drawing-options+
           #:+bold-face+ #:+bold-face-drawing-options+
           #:+bold-italic-face+ #:+bold-italic-drawing-options+

           #:*keyword-drawing-options*
           #:*special-operator-drawing-options*
           #:*special-variable-drawing-options*
           #:*string-drawing-options*
           #:*comment-drawing-options*
           #:*error-drawing-options*

           #:*highlight-strokes*
           #:*stroke-boundary-ink*
           #:*stroke-baseline-ink*

           ;; DREI program interface stuff.
           #:with-drei-options
           #:performing-drei-operations #:invoke-performing-drei-operations
           #:with-bound-drei-special-variables
           #:accepting-from-user #:invoke-accepting-from-user
           #:require-minibuffer

           ;; Gadget interface stuff.
           #:handle-gesture

           ;; Input-editor interface stuff.
           #:drei-input-editing-mixin #:drei-instance
           #:object #:result-type

           ;; Drei cursors.
           #:drei-cursor
           #:mark-cursor #:active #:mark
           #:active-ink #:inactive-ink #:ink

           
           #:in-focus-p

           ;; Info functions.
           #:point #:point-of
           #:mark #:mark-of
           #:current-syntax
           #:current-view
           #:drei-instance #:drei-instance-of

           ;; Configuration.
           #:*foreground-color*
           #:*background-color*
           #:*show-mark*
           #:*use-tabs-for-indentation*

           #:view-mode #:syntax-mode
           #:applicable-modes
           #:define-mode #:define-view-mode #:define-syntax-mode
           #:define-mode-toggle-commands))

(defpackage :drei-motion
  (:use :clim-lisp :drei-base :drei-buffer :drei-syntax)
  (:export #:forward-to-word-boundary #:backward-to-word-boundary
           #:define-motion-fns
           #:beep-limit-action #:revert-limit-action #:error-limit-action
           #:motion-limit-error
           #:make-diligent-motor

           ;; Lines
           #:forward-one-line
           #:backward-one-line
           #:forward-line
           #:backward-line

           ;; Words
           #:forward-one-word
           #:backward-one-word
           #:forward-word
           #:backward-word

           ;; Pages
           #:forward-one-page
           #:backward-one-page
           #:forward-page
           #:backward-page

           ;; Expressions
           #:forward-one-expression
           #:backward-one-expression
           #:forward-expression
           #:backward-expression

           ;; Definitions
           #:forward-one-definition
           #:backward-one-definition
           #:forward-definition
           #:backward-definition

           ;; Up
           #:forward-one-up
           #:backward-one-up
           #:forward-up
           #:backward-up

           ;; Down
           #:forward-one-down
           #:backward-one-down
           #:forward-down
           #:backward-down

           ;; Paragraphs
           #:forward-one-paragraph
           #:backward-one-paragraph
           #:forward-paragraph
           #:backward-paragraph

           ;; Sentences
           #:forward-one-sentence
           #:backward-one-sentence
           #:forward-sentence
           #:backward-sentence

           ;; Lists
           #:forward-one-list
           #:backward-one-list
           #:forward-list
           #:backward-list)
  (:documentation "Functions and facilities for moving a mark
around by syntactical elements. The functions in this package are
syntax-aware, and their behavior is based on the semantics
defined by the syntax of the buffer, that the mark they are
manipulating belong to. These functions are also directly used to
implement the motion commands."))

(defpackage :drei-editing
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring)
  (:export #:forward-delete-object
           #:backward-delete-object
           #:forward-kill-object
           #:backward-kill-object
           #:transpose-objects
           
           ;; Lines
           #:forward-delete-line #:backward-delete-line
           #:forward-kill-line #:backward-kill-line
           #:transpose-lines
           #:forward-delete-line-start #:backward-delete-line-start
           #:forward-kill-line-start #:backward-kill-line-start
           #:transpose-line-starts
           
           ;; Words
           #:forward-delete-word #:backward-delete-word
           #:forward-kill-word #:backward-kill-word
           #:transpose-words

           ;; Pages
           #:forward-delete-page #:backward-delete-page
           #:forward-kill-page #:backward-kill-page
           #:transpose-pages
           
           ;; Expressions
           #:forward-delete-expression #:backward-delete-expression
           #:forward-kill-expression #:backward-kill-expression
           #:transpose-expressions

           ;; Definitions
           #:forward-delete-definition #:backward-delete-definition
           #:forward-kill-definition #:backward-kill-definition
           #:transpose-definitions

           ;; Paragraphs
           #:forward-delete-paragraph #:backward-delete-paragraph
           #:forward-kill-paragraph #:backward-kill-paragraph
           #:transpose-paragraphs

           ;; Sentences
           #:forward-delete-sentence #:backward-delete-sentence
           #:forward-kill-sentence #:backward-kill-sentence
           #:transpose-sentences

           ;; Lists
           #:forward-delete-list #:backward-delete-list
           #:forward-kill-list #:backward-kill-list
           #:transpose-list)
  (:documentation "Functions and facilities for changing the
buffer contents by syntactical elements. The functions in this
package are syntax-aware, and their behavior is based on the
semantics defined by the syntax of the buffer, that the mark they
are manipulating belong to. These functions are also directly
used to implement the editing commands."))

(defpackage :drei-core
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring
        :drei-editing :clim :drei-abbrev :esa :esa-buffer :esa-io
        :esa-utils :drei-undo)
  (:export #:proper-line-indentation
           #:goto-position
           #:goto-line
           #:replace-one-string
           #:possibly-fill-line
           #:back-to-indentation
           #:insert-character
           #:delete-horizontal-space
           #:indent-current-line
           #:insert-pair
           #:move-past-close-and-reindent
           #:downcase-word #:upcase-word #:capitalize-word
           #:indent-region
           #:fill-line #:fill-region
           #:indent-line #:delete-indentation
           #:join-line
           #:set-syntax

           #:*killed-rectangle*
           #:map-rectangle-lines
           #:extract-and-delete-rectangle-line
           #:insert-rectangle-at-mark
           #:clear-rectangle-line
           #:open-rectangle-line
           #:replace-rectangle-line
           #:insert-in-rectangle-line
           #:delete-rectangle-line-whitespace
           #:with-narrowed-buffer

           #:start-mark #:end-mark
           #:make-buffer-stream

           #:target-specification
           #:activate-target-specification
           #:deactivate-target-specification
           #:subsequent-targets-p #:preceding-targets-p
           #:next-target #:previous-target
           #:previous-target
           #:no-more-targets
           #:*default-target-creator*
           #:view-list-target-specification #:views)
  (:documentation "Implementation of much syntax-aware, yet no
syntax-specific, core functionality of Drei."))

(defpackage :drei-fundamental-syntax
  (:use :clim-lisp :clim :drei-buffer :drei-base 
        :drei-syntax :flexichain :drei :drei-core :esa-utils)
  (:export #:fundamental-syntax)
  (:documentation "Implementation of the basic syntax module for
editing plain text."))

(defpackage :drei-lr-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base
        :drei-syntax :drei :drei-core :drei-fundamental-syntax
        :esa-utils)
  (:export #:lr-syntax-mixin #:stack-top #:initial-state
	   #:skip-inter #:lex #:define-lexer-state
	   #:lexer-toplevel-state #:lexer-error-state
	   #:parser-symbol #:parent #:children
	   #:start-offset #:end-offset #:parser-state
	   #:preceding-parse-tree
           #:literal-object-mixin
	   #:define-parser-state
	   #:lexeme #:nonterminal
	   #:action #:new-state #:done
	   #:reduce-fixed-number #:reduce-until-type #:reduce-all 
	   #:error-state #:error-reduce-state
           #:do-parse-symbols-forward
           #:parser-symbol-containing-offset
           #:define-syntax-highlighting-rules
           #:syntax-highlighting-rules)
  (:documentation "Underlying LR parsing functionality."))

(defpackage :drei-lisp-syntax
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base 
        :drei-syntax :drei-fundamental-syntax :flexichain :drei
        :drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io
	:drei-lr-syntax :drei-kill-ring)
  (:export #:lisp-syntax #:lisp-table
           #:lisp-string
           #:edit-definition
           #:form
           #:form-to-object #:form-equal

           ;; Selecting forms based on mark
           #:form-around #:form-before #:form-after
           #:find-list-parent
           #:expression-at-mark
           #:definition-at-mark
           #:form-of-type-at-mark
           #:list-at-mark
           #:symbol-at-mark
           #:fully-quoted-form
           #:fully-unquoted-form
           #:this-form

           ;; Querying forms
           #:formp #:form-list-p
           #:form-incomplete-p #:form-complete-p
           #:form-token-p #:form-string-p
           #:form-quoted-p
           #:form-comma-p #:form-comma-at-p #:form-comma-dot-p
           #:form-character-p
           #:form-simple-vector-p
           #:comment-p #:line-comment-p #:long-comment-p
           #:form-at-top-level-p

           ;; Querying form data
           #:form-children
           #:form-operator #:form-operands
           #:form-toplevel
           #:form-operator-p

           ;; Querying about state at mark
           #:in-string-p
           #:in-comment-p
           #:in-line-comment-p
           #:in-long-comment-p
           #:in-character-p
           #:location-at-beginning-of-form
           #:location-at-end-of-form
           #:at-beginning-of-list-p
           #:at-end-of-list-p
           #:at-beginning-of-string-p
           #:at-end-of-string-p
           #:at-beginning-of-children-p
           #:at-end-of-children-p
           #:comment-at-mark

           ;; Lambda list classes.
           #:lambda-list
           #:semiordinary-lambda-list
           #:ordinary-lambda-list
           #:macro-lambda-list
           #:destructuring-lambda-list

           ;; Lambda list constructors.
           #:make-lambda-list
           #:make-required-parameter
           #:make-&optional-parameter
           #:make-&key-parameter
           #:make-&body-parameter
           #:make-&rest-parameter
           #:parse-lambda-list

           ;; Lambda list readers.
           #:required-parameters
           #:optional-parameters
           #:keyword-parameters
           #:allow-other-keys-p
           #:rest-parameter
           #:body-parameter

           ;; Parameter classes.
           #:parameter
           #:named-parameter
           #:destructuring-parameter
           #:required-parameter #:destructuring-required-parameter #:named-required-parameter
           #:optional-parameter #:destructuring-optional-parameter #:named-optional-parameter
           #:keyword-parameter #:destructuring-keyword-parameter
           #:rest-parameter
           #:body-parameter


           ;; Parameter object readers.
           #:min-arg-index
           #:name
           #:inner-lambda-list
           #:init-form
           #:keyword-name

           ;; Conditions.
           #:form-conversion-error
           #:invalid-lambda-list

           ;; Configuration
           #:*syntax-highlighting-rules*
           #:emacs-style-highlighting
           #:retro-highlighting)
  (:shadow clim:form)
  (:documentation "Implementation of the syntax module used for
editing Common Lisp code."))

(defpackage :drei-commands
  (:use :clim-lisp :drei-base :drei-buffer
        :drei-syntax :drei-motion :drei :drei-kill-ring
        :drei-editing :clim :drei-abbrev :esa :esa-buffer :esa-io
        :esa-utils :drei-core :drei-undo)
  (:export #:define-motion-commands
           #:define-deletion-commands
           #:define-editing-commands)
  (:documentation "Command definitions that are not tied to
specific syntaxes."))

(defpackage :drei-user
  (:use :clim-lisp :clim :clim-extensions :drei-buffer :drei-base 
        :drei-syntax :drei-fundamental-syntax :flexichain :drei
        :drei-motion :drei-editing :esa-utils :esa :drei-core :esa-io
        :drei-commands)
  (:documentation "The package intended for user-made
customizations and extensions."))
