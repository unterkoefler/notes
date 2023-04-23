.PHONY: serve
serve: venv
	$(VENV)/python manage.py runserver 0.0.0.0:8001

include Makefile.venv
