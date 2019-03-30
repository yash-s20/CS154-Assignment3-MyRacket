import os

my_interpreter = """
#lang racket
(require racket/struct)
(require "defs.rkt")
(require "examples.rkt")
(require "my-interpreter.rkt")

(eval-program prog{0})
"""

try_file = """
#lang racket
(require racket/struct)
(require "defs.rkt")
(require "examples.rkt")
(require "model-interpreter.rkt")

(eval-program prog{0})
"""
for i in range(1, 13):
	if i == 4:
		continue
	os.system("echo '" + my_interpreter.format(str(i)) + "' > testmy{0}.rkt".format(str(i)))
	os.system("echo '" + try_file.format(str(i)) + "' > testtry{0}.rkt".format(str(i)))
	os.system("gtimeout 5 racket testmy{0}.rkt > out1{0}.txt".format(str(i)))
	os.system("gtimeout 5 racket testtry{0}.rkt > out2{0}.txt && diff out1{0}.txt out2{0}.txt && echo $?".format(str(i)))

os.system("rm out*")
os.system("rm testmy* && testtry*")