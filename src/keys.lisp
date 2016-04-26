(in-package :selenium)

;; TODO: make constant?
(defvar *keymap*
  '(:null #\ue000
    :cancel #\ue001
    :help #\ue002
    :backspace #\ue003
    :tab #\ue004
    :clear #\ue005
    :return #\ue006
    :enter #\ue007
    :shift #\ue008
    :control #\ue009
    :alt #\ue00a
    :pause #\ue00b
    :escape #\ue00c
    :space #\ue00d
    :page-up #\ue00e
    :page-down #\ue00f
    :end #\ue010
    :home #\ue011
    :left-arrow #\ue012
    :up-arrow #\ue013
    :right-arrow #\ue014
    :down-arrow #\ue015
    :insert #\ue016
    :delete #\ue017
    :semicolon #\ue018
    :equals #\ue019
    :numpad-0 #\ue01a
    :numpad-1 #\ue01b
    :numpad-2 #\ue01c
    :numpad-3 #\ue01d
    :numpad-4 #\ue01e
    :numpad-5 #\ue01f
    :numpad-6 #\ue020
    :numpad-7 #\ue021
    :numpad-8 #\ue022
    :numpad-9 #\ue023
    :multiply #\ue024
    :add #\ue025
    :separator #\ue026
    :substract #\ue027
    :decimal #\ue028
    :divide #\ue029
    :f1 #\ue031
    :f2 #\ue032
    :f3 #\ue033
    :f4 #\ue034
    :f5 #\ue035
    :f6 #\ue036
    :f7 #\ue037
    :f8 #\ue038
    :f9 #\ue039
    :f10 #\ue03a
    :f11 #\ue03b
    :f12 #\ue03c
    :meta #\ue03d))

;; TODO: prepare all keys on compile
(defun key (key)
  (make-string 1 :initial-element (getf *keymap* key)))
