SHELL:=/bin/bash
CONF=config.sh
#######################################
install: config
	./$(CONF) --install

#######################################
uninstall: clean

#######################################
clean: config
	./$(CONF) --clean
	chmod -x $(CONF)

#######################################
config:
	chmod +x $(CONF)

#######################################
