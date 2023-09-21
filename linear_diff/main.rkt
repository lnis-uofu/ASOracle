#lang racket/base
(require "verilog-output.rkt" "spice-output.rkt")
(module reader racket
  (provide read-syntax)
  (require "reader.rkt" "dsl.rkt"))
