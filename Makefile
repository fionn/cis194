SRCS=??/*.hs
OBJS=$(subst .hs,.o,$(SRCS))
INTERFACES=$(subst .hs,.hi,$(SRCS))

.PHONY: all
all: $(SRCS)
	@ghc $(SRCS)

tags: $(SRCS)
	@echo ":ctags" | ghci -v0 $(SRCS)

.PHONY: lint
lint: $(SRCS)
	@hlint $(SRCS)

.PHONY: clean
clean:
	-@$(RM) $(OBJS) $(INTERFACES)
