%.so : %.c
	cc -shared -o $@ $^
