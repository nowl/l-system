(in-package :l-system)

;; examples from wikipedia

(defparameter *algae-rules*
  '("a" ((#\a . "ab") (#\b . "a"))))

(defparameter *fibonacci-rules*
  '("a" ((#\a . "b") (#\b . "ab"))))

(defparameter *cantor-dust-rules*
  '("a" ((#\a . "aba") (#\b . "bbb"))))

(defparameter *koch-curve-rules*
  '("f" ((#\f . "f+f-f-f+f") 
         (#\+ . "+")
         (#\- . "-"))))

(defparameter *sierpinski-triangle-rules*
  '("a" ((#\a . "b-a-b")
         (#\b . "a+b+a")
         (#\+ . "+")
         (#\- . "-"))
    :angle 60))

(defparameter *dragon-curve-rules*
  '("fx" ((#\x . "x+yf")
          (#\y . "fx-y")
          (#\f . "f")
          (#\+ . "+")
          (#\- . "-"))
    :angle 90))

(defparameter *fractal-plant-rules*
  '("x" ((#\x . "f-[[x]+x]+f[+fx]-x")
         (#\f . "ff")
         (#\+ . "+")
         (#\- . "-"))
    :angle 25))

(defun single-expand (input rules)
  (let ((acc ""))
    (loop for char across input do
         (let ((prod (assoc char rules)))
           (when prod
             (setf acc (concatenate 'string acc (cdr prod))))))
    acc))

(defun expander (rules iterations)
  (let ((acc (first rules)))
    (dotimes (i iterations)
      (setf acc (single-expand acc (second rules))))
    acc))