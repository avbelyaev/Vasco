//
// Created by anthony on 2020-01-01.
//


#include <stdio.h>
#include <stdlib.h>
#include <string.h>


int sum(int a, int b)
{
    return a + b;
}

int main(int argc, char* argv[])
{
    char* progName = argv[0];
    printf("prog name: %s\n", progName);

    int a = atoi(argv[1]);
    int b = atoi(argv[2]);

    int c = sum(a, b);
    printf("result: %d\n", c);
}