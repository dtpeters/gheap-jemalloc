
FILES=gheap-jemalloc.scm
PREFIX="${HOME}/.local/share/gheap/"
SYSTEM_PREFIX="/usr/share/gdb/gheap/"

install: ${FILES}
	mkdir -p ${PREFIX}
	cp ${FILES} ${PREFIX}

uninstall:
	rm -fr ${PREFIX}
	rm -fr ${SYSTEM_PREFIX}

sysinstall:
	mkdir -p ${SYSTEM_PREFIX}
	cp ${FILES} ${SYSTEM_PREFIX}
