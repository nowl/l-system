(asdf:defsystem :l-system
    :description "An L-system expander."
    :version "0.1"
    :author "Nowl <esologic@gmail.com>"
    :license "GNU Public License"
    :depends-on ("turtle-graphics")
    :components ((:file "package")
                 (:file "expander"))
    :serial t)
