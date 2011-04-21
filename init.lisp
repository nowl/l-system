(load "~/quicklisp/setup.lisp")

(push #p"../turtle-graphics/" asdf:*central-registry*)

(asdf:oos 'asdf:load-op 'l-system)