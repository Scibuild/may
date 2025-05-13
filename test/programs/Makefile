
SRCS := $(shell find . -name '*.may')
OBJS := $(SRCS:%.may=%.exe)

%.exe: %.s
	$(CC) $< -o $@ -L../../_build/default/runtime -lmayruntime 

%.s: %.qbe
	qbe $< -o $@

%.qbe: %.may
	../../_build/default/bin/main.exe $< -o $@

.PHONY: all clean

all: $(OBJS)

clean:
	rm *.exe