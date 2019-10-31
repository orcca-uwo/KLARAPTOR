


all: polylib

polylib : objects
	ar rcs libnumpolysupport.a src/numpolysupport.o	

objects : install-interp-packages
	(cd src; make;)

install-interp-packages:
	(cd install_interp_packages; ls; make;)
	chmod u+x get_numpolysupport_libargs.sh get_libpath.sh

test : exec-test
exec-test : objects
	(cd test; make;)

##### clean

clean : 
	(cd src; make clean;)
	(cd test; make clean;)
	(rm -f libnumpolysupport.a)
	(cd install_interp_packages; make clean;)
	(chmod -x get_numpolysupport_libargs.sh	get_libpath.sh;)
