CFLAGS=-Q -Gd- -Se -Re -Ss -Ms -Gm+ -c -Ge- -Fo
LFLAGS=/NOFREE /NOE /EXEP:2 /DLL
LLIBLS=rexx

.c.obj:
    icc $(CFLAGS) $<

..\bin\awget.dll: awget.obj url.obj awget.def awget.res
    ilink $(LFLAGS) awget+url,$@,awget,$(LLIBLS),awget.def
    rc -n awget.res $*.dll

awget.obj: awget.c awget.h url.h
url.obj:   url.c url.h
awget.res: awget.rc awget.h
    rc -n -r $*.rc $*.res

clean:
  -@erase *.obj
  -@erase *.res