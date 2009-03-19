#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[])
{
    static unsigned *buffer, *new_data;
    int data_start, tcb_start, n, nsp1, ntc1, i;
    struct stat s;
    FILE* f;
    stat("mar09948.raw",&s);
    buffer = (unsigned*)malloc(s.st_size);
    f = fopen("mar09948.raw","rb");
    fread(buffer, 1, s.st_size, f);
    fclose(f);
    data_start = buffer[27] - 1;
    tcb_start = buffer[25] - 1;
    nsp1 = buffer[tcb_start + 260]; /* number of spectra */
    ntc1 = buffer[tcb_start + 261]; /* number of time channels */
    printf("%d %d\n", nsp1, ntc1);
    buffer[data_start] = 1;   /* set data type to uncompressed */
    n = (nsp1+1) * (ntc1+1); /* +1 as spectrum 0 and time channel 0 */
    new_data = malloc(4*n);
    for(i=0; i<n; i++)
    {
        new_data[i] = i;
    }
    f = fopen("newfile.raw","wb");
    /* write out old file and new compression flag */
    fwrite(buffer, 4, data_start+1, f);
    /* write out new data */
    fwrite(new_data, 4, n, f);
    fclose(f);
    return 0;
}
   
