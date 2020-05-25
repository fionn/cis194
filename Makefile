SRCS=??/*.hs
OBJS=$(subst .hs,.o,$(SRCS))
INTERFACES=$(subst .hs,.hi,$(SRCS))

.PHONY: all
all: $(SRCS)
	@ghc $(SRCS)

tags: $(SRC)
	@echo ":ctags" | ghci -v0 $(SRCS)

.PHONY: lint
lint: $(SRC)
	@hlint .

.PHONY: clean
clean:
	-@$(RM) $(OBJS) $(INTERFACES)
