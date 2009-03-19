#include <stdio.h>

int main(int argc, char* argv[])
{
    int c1, c2, c3;
    while( (c1 = getchar()) != EOF )
    {
	if (c1 != '&')
	{
	    printf("%c", c1);
	    continue;
	}
	c2 = getchar();
	if (c2 == EOF)
	    continue;
	c3 = getchar();
	if (c3 == EOF)
	    continue;
	if (c2 == '\n' && c3 == '&')
	{
	    continue; /* c1, c2 and c3 ignored so lines joined */
	}
	else
	{
	    printf("%c%c%c", c1, c2, c3);
	}
    }
    return 0;
}
