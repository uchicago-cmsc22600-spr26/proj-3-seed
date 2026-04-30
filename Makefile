# Makefile for Project 3
#
# CMSC 22600 --- Compilers for Computer Languages
# Spring 2026
# University of Chicago
#
# COPYRIGHT (c) 2026 John Reppy (https://cs.uchicago.edu/~jhr)
# All rights reserved.
#
# targets:
#	make soolc	-- Build SooL compiler
#	make sooli	-- Build SooL interpreter
#	make all	-- Build all of the above
#

SHELL =         /bin/sh
OS =            $(shell uname -s)

# we assume that sml (and thus ml-build is in the path)
#
ML_BUILD =	ml-build

HEAP_SUFFIX =	$(shell sml @SMLsuffix)

CM_FILES =	$(wildcard */sources.cm)

COMMON_SRCS =	ast/ast-pp.sml \
		ast/ast.sml \
		ast/basis.sml \
		ast/class.sml \
		ast/interface.sml \
		ast/local-var.sml \
		ast/member-fun.sml \
		ast/member-var.sml \
		ast/type-of.sml \
		ast/types.sml \
		ast/util.sml \
		common/binop.sml \
		common/error.sml \
		common/stamp.sml \
		normalize/analyze-ast.sml \
		normalize/basis-members.sml \
		normalize/coercion-graph.sml \
		normalize/coercions.sml \
		normalize/env.sml \
		normalize/normalize.sml \
		normalize/util.sml \
		parse-tree/parse-tree.sml \
		parse-tree/print-parse-tree.sml \
		parser/parser.sml \
		parser/sool.grm \
		parser/sool.lex \
		soir/prim-op.sml \
		soir/prog-pt.sml \
		soir/soir-basis.sml \
		soir/soir-func.sml \
		soir/soir-index.sml \
		soir/soir-pp.sml \
		soir/soir-type.sml \
		soir/soir-util.sml \
		soir/soir-var.sml \
		soir/soir.sml \
		type-checker/basis-env.sml \
		type-checker/check-block.sml \
		type-checker/check-exp.sml \
		type-checker/check-params.sml \
		type-checker/check-type.sml \
		type-checker/define-member.sml \
		type-checker/env.sml \
		type-checker/type-error.sml \
		type-checker/type-util.sml \
		type-checker/typechecker.sml

SOOLC_SRCS =	$(COMMON_SRCS) \
		driver/main.sml

SOOLI_SRCS =	$(COMMON_SRCS) \
		soir-interp/dynamic-env.sml \
		soir-interp/eval.sml \
		soir-interp/interp.sml \
		soir-interp/runtime.sml \
		soir-interp/util.sml \
		soir-interp/value.sml \
		soir-interp/varmap.sml

.PHONY:		soolc
soolc:		bin/soolc.$(HEAP_SUFFIX)

.PHONY:		sooli
sooli:		bin/sooli.$(HEAP_SUFFIX)

.PHONY:		all
all:		bin/soolc.$(HEAP_SUFFIX) bin/sooli.$(HEAP_SUFFIX)

# build rule for compiler
bin/soolc.$(HEAP_SUFFIX):	$(CM_FILES) $(SOOLC_SRCS)
	$(ML_BUILD) driver/sources.cm Main.main soolc
	mv soolc.$(HEAP_SUFFIX) bin

# build rule for interpreter
bin/sooli.$(HEAP_SUFFIX):	$(CM_FILES) $(SOOLI_SRCS)
	$(ML_BUILD) soir-interp/sources.cm Interp.main sooli
	mv sooli.$(HEAP_SUFFIX) bin

.PHONY:		clean
clean:
		rm -rf bin/*.$(HEAP_SUFFIX)
		rm -rf .cm */.cm
		rm -f parser/sool.grm.sml parser/sool.lex.sml
