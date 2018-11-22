SRCS=??/*.hs
OBJS=$(subst .hs,.o,$(SRCS))
INTERFACES=$(subst .hs,.hi,$(SRCS))

all: $(SRCS)
	@ghc $(SRCS)

tags: $(SRC)
	@echo ":ctags" | ghci -v0 $(SRCS)

.PHONY: clean
clean:
	-@$(RM) $(OBJS) $(INTERFACES)
