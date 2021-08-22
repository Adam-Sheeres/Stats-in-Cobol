#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

int main() {
    FILE        *fp;
    int         i;
    time_t      t;
    int         n;
    char        fName[50];

    i = n = 0;

    printf("Enter number: ");
    scanf("%d", &n);
    printf("Enter file name: ");
    scanf("%s", fName);

    fp = fopen(fName, "w");
    if (fp == NULL) {
        printf("UNABLE TO OPEN FILE\n");
        exit(1);
    } 

    srand((unsigned) time(&t));

    for (i = 0 ; i < n ; i++) {
        fprintf(fp, "        %7lld\n", rand() % 10000000000);
    }

    fclose(fp);


    return 0;
}