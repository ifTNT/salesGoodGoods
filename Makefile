all:clean main.exe run

run:
	@echo [Running]
	@dosbox -c "mount C $(shell pwd)" \
		   -c "C:" \
		   -c "main.exe" \
		   -c "exit"

%.exe:%.obj
	dosbox -c "mount C $(shell pwd)" \
			-c "C:" \
			-c "TLINK /s $(subst /,\\,$^), $(subst /,\\,$@) > __LINKER.TXT" \
			-c "exit" > /dev/null
	@echo [Linking]
	@cat __LINKER.TXT
	@rm -f __LINKER.TXT

%.obj:%.asm 
	@echo [Assembling]
	nasm -f obj -Ilib/ -o $@ $^

clean:
	@echo Cleaning up...
	rm -f MAIN.EXE main.obj MAIN.MAP
