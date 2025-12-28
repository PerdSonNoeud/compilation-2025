#include <stdio.h>
#include <stdlib.h>

long add_eight_int(long a1, long a2, long a3, long a4, long a5, long a6, long a7, long a8) {
    return a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8;
}

void observe_int(long n) {
    printf("Observation: %ld\n", n);
    exit((int)n);
}
