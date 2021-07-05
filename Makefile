LISP ?= sbcl
APP = cl-github

# build:
# 	$(LISP) --non-interactive \
# 		--load $(APP).asd \
# 		--eval '(ql:quickload :$(APP))' \
# 		--eval '(asdf:make :$(APP))'

build:
	$(LISP) \
    	--eval '(load "$(APP).asd")' \
		--eval '(ql:quickload :$(APP))' \
		--eval "(sb-ext:save-lisp-and-die #p\"${APP}\" :toplevel #'${APP}::main \
		:executable t :purify t :save-runtime-options t :compression t)" \
		--eval '(quit)'

