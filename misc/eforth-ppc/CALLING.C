#include <stdio.h>
#include <stdlib.h> 
#include <string.h> 
#include <time.h> 

	char *gHelloString[120];

int main()
{   
	int n;
    printf("Boot eForth from C\n");
	n=fflush(stdout);
    printf("C. H. Ting, 1997\n");
	n=fflush(stdout); 
	
	eforth();
	return 0;
}



