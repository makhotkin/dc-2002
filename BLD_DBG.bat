ren dc2002.exe	dc2002_.exe
del dc2002.exe
tasm.exe /zi *.asm > errlog.txt
tlink.exe /v dc2002.obj decode.obj user.obj disk.obj view.obj base.obj view_hex.obj view_dec.obj >> errlog.txt

If EXIST dc2002.exe GOTO debug
\nc\wpview.exe errlog.txt
GOTO done

:debug
td dc2002.exe

:done