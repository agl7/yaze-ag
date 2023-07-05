; MAKE.C und MAKEFILE  (c) Copyright  11/1988 by Michael Schewe
;                                                  3119 Gollern Nr.17

projekt ds
begin
  let #asm = ""
  let #era = ""
  if_bedingung ( disk1.rel disk1.c stdio.h disk.h )
    begin
      do "ccz /08sd disk1\n"
      do "era disk1.z80\n"
      do "ren disk1.z80=disk1.asm\n"
      let #asm = ( #asm + "disk1/m6," )
      let #era = ( #era + "era disk1.z80\n" )
    end
  if_bedingung ( disk2.rel disk2.c stdio.h disk.h )
    begin
      do "ccz /08sd disk2\n"
      do "era disk2.z80\n"
      do "ren disk2.z80=disk2.asm\n"
      let #asm = ( #asm + "disk2/m6," )
      let #era = ( #era + "era disk2.z80\n" )
    end
  if_bedingung ( disk3.rel disk3.c stdio.h disk.h )
    begin
      do "ccz /08sd disk3\n"
      do "era disk3.z80\n"
      do "ren disk3.z80=disk3.asm\n"
      let #asm = ( #asm + "disk3/m6," )
      let #era = ( #era + "era disk3.z80\n" )
    end
  if_bedingung ( disk4.rel disk4.c stdio.h disk.h )
    begin
      do "ccz /08sd disk4\n"
      do "era disk4.z80\n"
      do "ren disk4.z80=disk4.asm\n"
      let #asm = ( #asm + "disk4/m6," )
      let #era = ( #era + "era disk4.z80\n" )
    end
  if_bedingung ( disksort.rel disksort.c stdio.h disk.h )
    begin
      do "ccz /08sd disksort\n"
      do "era disksort.z80\n"
      do "ren disksort.z80=disksort.asm\n"
      let #asm = ( #asm + "disksort/m6," )
      let #era = ( #era + "era disksort.z80\n" )
    end
  if #asm
    begin
       do ( "z80 " + #asm + "\n" )
       do #era
       do "mera\n"
       do "l80 xxxmain,disksort,disk1,disk2,disk3,disk4,mylib/s,disksort/n/y/e\n"
       do "crc disksort.com\n"
    end
end


projekt make
begin
  let #asm = ""
  let #era = ""
  if_bedingung ( make.rel make.c stdio.h )
    begin
      do "ccz /08sd make\n"
      do "era make.z80\n"
      do "ren make.z80=make.asm\n"
      let #asm = ( #asm + "make/m6," )
      let #era = ( #era + "era make.z80\n" )
    end
  if_bedingung ( makeinc.rel makeinc.c stdio.h )
    begin
      do "ccz /08sd makeinc\n"
      do "era makeinc.z80\n"
      do "ren makeinc.z80=makeinc.asm\n"
      let #asm = ( #asm + "makeinc/m6," )
      let #era = ( #era + "era makeinc.z80\n" )
    end
  if #asm
    begin
       do ( "z80 " + #asm + "\n" )
       do #era
       do "mera\n"
       do "l80 make/n,xxxmain,make,makeinc,mylib/s/e/y\n"
       do "crc make.com\n"
    end
end


projekt t
begin
  if_bedingung ( tnew.com tnew.c )
    begin
       do "ccz /C08sd t\n"
       do "era t.z80\n"
       do "ren t.z80=t.asm\n"
       do "z80 t/m6\n"
       do "l80 t/n/y,xxxmain,t,mylib/s/e\n"
    end
end
