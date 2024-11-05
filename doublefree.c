#include <stdio.h>
#include <stdlib.h>

int main() {
    // Step 1: Allocate memory
    char *ptr = (char *)malloc(100 * sizeof(char));
    if (ptr == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        return 1;
    }

    // Step 2: Free the allocated memory
    free(ptr);

    // Step 3: Accidental second free of the same memory block
    free(ptr);  // Double free bug

    printf("Double free occurred. Undefined behavior may follow.\n");

    return 0;
}
