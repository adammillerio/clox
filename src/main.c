#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "chunk.h"   // IWYU pragma: keep
#include "common.h"  // IWYU pragma: keep
#include "debug.h"   // IWYU pragma: keep
#include "vm.h"

/* Start the Lox REPL.
 *
 * This will continually read lines from stdin, interpret them, and print the
 * result.
 */
static void repl() {
    char line[1024];
    for (;;) {
        printf("> ");

        // Read input into line with fgets from stdin, if there is no input,
        // then exit
        if (!fgets(line, sizeof(line), stdin)) {
            printf("\n");
            break;
        }

        interpret(line);
    }
}

/* Read a Lox source file.
 *
 * @param path the path to the file on the system
 *
 * @return the Lox source file loaded as a string
 */
static char* readFile(const char* path) {
    // Open the file in binary read mode
    FILE* file = fopen(path, "rb");
    if (file == NULL) {
        fprintf(stderr, "Could not open file \"%s\".\n", path);
        exit(74);
    }

    // Seek to the end of the file
    fseek(file, 0L, SEEK_END);
    // Tell in order to get # of bytes in the file, since it is at the end
    size_t fileSize = ftell(file);
    // Return to the start of the file
    rewind(file);

    // Allocate a string of fileSize + 1 (for null char)
    char* buffer = (char*)malloc(fileSize + 1);
    if (buffer == NULL) {
        fprintf(stderr, "Not enough memory to read \"%s\".\n", path);
        exit(74);
    }

    // Read file into the string
    size_t bytesRead = fread(buffer, sizeof(char), fileSize, file);
    if (bytesRead < fileSize) {
        fprintf(stderr, "Could not read file \"%s\".\n", path);
        exit(74);
    }

    // Add the null char
    buffer[bytesRead] = '\0';

    // Close file and return loaded string
    fclose(file);
    return buffer;
}

/* Run a Lox source file.
 *
 * Given a path, this will load the Lox source at this path and execute it.
 *
 * @param path the path to the file on the system
 */
static void runFile(const char* path) {
    // All scanned Tokens store pointers to locations within this source string
    // to represent the lexeme, so it needs to live as long as the execution
    // is happening.
    char* source = readFile(path);
    InterpretResult result = interpret(source);

    free(source);

    if (result == INTERPRET_COMPILE_ERROR) exit(65);
    if (result == INTERPRET_RUNTIME_ERROR) exit(70);
}

int main(int argc, const char* argv[]) {
    // Start Lox Virtual Machine
    initVM();

    // argv[0] is always the executable name
    if (argc == 1) {
        // No args, run interactive REPL
        repl();
    } else if (argc == 2) {
        // Run the provided Lox source file
        runFile(argv[1]);
    } else {
        fprintf(stderr, "Usage: clox [path]\n");
        exit(64);
    }

    // Free the Lox Virtual Machine
    freeVM();

    return 0;
}
