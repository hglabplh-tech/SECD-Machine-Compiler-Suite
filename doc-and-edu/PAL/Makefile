# This makefile is used to create .tgz and .zip versions
# of the PAL distribution.

PUB = /homes/mr/public_html

# Public HTML directory if not mountable on this machine
# and the shared drive is called E: (/dose on Linux).
# Remember to call ssh-add before calling make sshpub.
SSHPUB = sandy.cl.cam.ac.uk:public_html

help:
	@echo
	@echo "make all      Construct the files: pal.tgz and pal.zip"
	@echo "make dosd     Put them in my D drive (/dosd)"
	@echo "make dose     Put them in my E drive (/dose)"
	@echo "make pub      Put them also in my home page"
	@echo "make sshpubd  Put them in /dosd and my home page using scp"
	@echo "make sshpube  Put them in /dose and my home page using scp"
	@echo

all:	
	rm -f *~ */*~
	echo >TGZDATE
	echo -n "Distributed from machine: " >>TGZDATE
	hostname >>TGZDATE
	date >>TGZDATE
	rm -f FILES
	(cd pal; make clean)
	(cd ..; tar cvzf pal.tgz PAL)
	rm -f ../pal.zip
	(cd ..; zip -rv9 pal.zip PAL)
	cp TGZDATE FILES
	ls -l ../pal.tgz ../pal.zip>>FILES

pub:	dosd
	cp README FILES ../pal.tgz ../pal.zip $(PUB)/PAL
	cat FILES

sshpubd:	dosd
	scp README FILES ../pal.tgz ../pal.zip $(SSHPUB)/PAL
	cat FILES

sshpube:	dose
	scp README FILES ../pal.tgz ../pal.zip $(SSHPUB)/PAL
	cat FILES

dosd:	all
	cp ../pal.tgz ../pal.zip /dosd

dose:	all
	cp ../pal.tgz ../pal.zip /dose

