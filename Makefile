CC = cobc
FLAGS  = -x -free -Wall

main:
	clear
	$(CC) $(FLAGS) statnew.cob
run: main
	clear
	./statnew
clean:
	-rm statnew
	-rm NOUT.TXT
	-rm statd
	clear
generate:
	-rm *.txt
	clear
	gcc -Wall -std=c99 randomNumbers.c -o random
	./random
old:
	clear
	$(CC) $(FLAGS) statd.cob
	./statd
