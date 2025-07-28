.PHONY: all

all: input/25M_circle.dat input/25M_rectangle.dat input/25M_quadratic.dat
	cd input && md5sum -c MD5SUMS

gen_points: gen_points.c
	cc -o gen_points gen_points.c -O -Wall -Wextra -fsanitize=undefined

input/25M_circle.dat: ./gen_points
	./gen_points 2000000000 2000000000 25000000 c > $@

input/25M_rectangle.dat: ./gen_points
	./gen_points 2000000000 2000000000 25000000 r > $@

input/25M_quadratic.dat: ./gen_points
	./gen_points 2000000000 2000000000 25000000 q > $@
