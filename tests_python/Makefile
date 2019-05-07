TYPECHECK=mypy
# pytest causes redefined-outer-name and no-self-use
LINT=pylint -d redefined-outer-name,no-self-use,missing-docstring,too-many-arguments,too-few-public-methods,fixme,too-many-locals,too-many-public-methods,too-many-instance-attributes,R0801
LINT2=pycodestyle
PACKAGES=daemons launchers client tools scripts tests examples tests/multibranch
LOG_DIR=tmp

some:
	pytest -m "not slow"

all:
	mkdir -p $(LOG_DIR)
	pytest --log-dir=tmp --tb=no

injection:
	pytest -s tests/test_injection.py


lint_all: typecheck lint lint2

typecheck:
	$(TYPECHECK) $(PACKAGES)

lint:
	$(LINT) $(PACKAGES)

lint2:
	$(LINT2) $(PACKAGES)

test:
	pytest tests_simple -sv

clean:
	rm -rf tmp/*  __pycache__ *.pyc */__pycache__ */*.pyc .mypy_cache .pytest_cache .pytype
