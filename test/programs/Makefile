
SRCS := $(shell find . -path ./lib -prune -o -name '*.may' -print)
OBJS := $(SRCS:%.may=%.exe)

SRCS_O := $(shell find . -path ./lib -prune -o -name '*.mayo' -print)
OBJS_O := $(SRCS_O:%.mayo=%.exe)

%.exe: %.s
	$(CC) $< -o $@ -L../../_build/default/runtime -lmayruntime 

%.s: %.qbe
	qbe $< -o $@

%.qbe: %.may ../../may.exe
	../../may.exe $< -out $@

%.qbe: %.mayo ../../may.exe
	../../may.exe -ownership $< -out $@

.PHONY: all clean

all: $(OBJS) $(OBJS_O)

clean:
	rm *.exe
