(ns ui.samak.stdlib)

(defmacro link [from to]
  `(link* ~from '~from ~to '~to))
