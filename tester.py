import os
import sys

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
# for i in range(1, 13):
# 	os.system("echo '" + my_interpreter.format(str(i)) + "' > testmy{0}.rkt".format(str(i)))
# 	os.system("echo '" + try_file.format(str(i)) + "' > testtry{0}.rkt".format(str(i)))
# 	os.system("gtimeout 5 racket testmy{0}.rkt > out1{0}.txt 2>err1{0}.txt".format(str(i)))
# 	os.system("gtimeout 5 racket testtry{0}.rkt > out2{0}.txt 2>err2{0}.txt; diff out1{0}.txt out2{0}.txt >/dev/null || [ -s err2{0}.txt ] && echo {0} pass || echo {0} fail".format(str(i)))

# os.system("rm out* err*")
# os.system("rm testmy* testtry*")
print(sys.platform)