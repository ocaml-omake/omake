/*
 * Read input from the terminal.
 *
 * ----------------------------------------------------------------
 *
 * @begin[license]
 * Copyright (C) 2004-2007 Mojave Group, Caltech and HRL Laboratories, LLC
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; version 2
 * of the License.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 * 
 * Additional permission is given to link this library with the
 * with the Objective Caml runtime, and to redistribute the
 * linked executables.  See the file LICENSE.OMake for more details.
 *
 * Author: Jason Hickey @email{jyh@cs.caltech.edu}
 * Modified by: Aleksey Nogin @email{nogin@metaprl.org}, @email{anogin@hrl.com}
 * @end[license]
 */
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/custom.h>
#include <caml/callback.h>

/*
 * XXX: HACK (nogin 02/28/07):
 * CAMLreturn with non-value types is wrong in 3.09.3 and later; CAMLreturnT was added in 3.09.4 and
 * 3.10 to address this, but in 3.09.3 we are out of luck!
 */
#ifndef CAMLreturnT
#define CAMLreturnT(type, result) do{ \
   type caml__temp_result = (result); \
   caml_local_roots = caml__frame; \
   return (caml__temp_result); \
}while(0)
#endif

#ifdef WIN32
#  include <caml/signals.h>
#  include <windows.h>
   /* Disable some of the warnings */
#  pragma warning( disable : 4100 4127 4505 4702 4996 4189)
#else
#  include <unistd.h>
#  include <string.h>
#  include <sys/types.h>
#  include <limits.h>

#  ifdef READLINE_ENABLED
#    include <readline/readline.h>
#    include <readline/history.h>
#  endif
#endif /* !WIN32 */

/*
 * Maximum number of characters in an input line. Defined by limits.h on Unix.
 */
#ifndef LINE_MAX
#define LINE_MAX                2048
#endif

/*
 * Prompts shouldn't be too wild.
 * In particular, they should be less than an average line length.
 */
#define MAX_PROMPT_LENGTH       70

/*
 * Maximum length of the history filename. Defined by limits.h on Unix.
 */
#ifndef PATH_MAX
#define PATH_MAX                2048
#endif

/*
 * Space for completions array.
 */
#define COMPLETION_LENGTH       10

/*
 * Name of the command completion callback.
 */
static char omake_filename_completion[] = "omake_filename_completion";
static char omake_command_completion[]  = "omake_command_completion";

/************************************************************************
 * Completions.
 */

/*
 * Linked list of filename completions.
 */
typedef struct _completion_info {
    /* Current directory (needed for filename completion) */
    char dir[PATH_MAX];

    /* List of completions */
    char **completions;
} CompletionInfo;

/*
 * Command completions use a callback.
 */
static char **readline_completion(char *omake_completion, const char *text)
{
    CAMLparam0();
    CAMLlocal2(request, response);
    char *namep, **completions;
    value *callbackp;
    int i, length;

#ifdef WIN32
    /* This is bogus but temporary */
    (void) caml__dummy_request;
#endif

    /* Find the callback, abort if it doesn't exist */
    callbackp = caml_named_value(omake_completion);
    if(callbackp == 0 || *callbackp == 0)
        CAMLreturnT(char **, 0);

    /* The callback returns an array of strings */
    request = caml_copy_string(text);
    response = caml_callback(*callbackp, request);
    
    /* Copy the array of strings */
    length = Wosize_val(response);
    if(length == 0)
        CAMLreturnT(char **, 0);
    completions = malloc((length + 1) * sizeof(char *));
    if(completions == 0)
        CAMLreturnT(char **, 0);
    for(i = 0; i != length; i++) {
        namep = strdup(String_val(Field(response, i)));
        if(namep == 0)
            break;
        completions[i] = namep;
    }
    completions[i] = 0;
    CAMLreturnT(char **, completions);
}

/************************************************************************
 * Readline simulation for Win32.
 */

#ifdef WIN32

/*
 * Number of fields in the history.
 */
#define MAX_HISTORY_LENGTH      256

/*
 * Console modes.
 */
#define RAW_CONSOLE_MODE        (ENABLE_WINDOW_INPUT)
#define COOKED_CONSOLE_MODE     (ENABLE_ECHO_INPUT \
                                | ENABLE_LINE_INPUT\
                                | ENABLE_MOUSE_INPUT\
                                | ENABLE_PROCESSED_INPUT\
                                | ENABLE_WINDOW_INPUT)

/*
 * Number of events to read from the console.
 */
#define INPUT_COUNT             10


/*
 * A ReadLine-style buffer.
 * The buffer should be long enough to hold the input,
 * and the line terminator, and the string terminator.
 */
typedef struct _line_buffer {
    char buffer[LINE_MAX + 3];
    int length, index;
} LineBuffer;

typedef struct _readline {
    /* History */
    LineBuffer history[MAX_HISTORY_LENGTH];
    LineBuffer current;                 // The line being edited
    LineBuffer display;                 // What is displayed
    int start;                          // First line in the history
    int length;                         // Number of lines in the history
    int index;                          // Current line in the history
    int offset;                         // Base history offset
    char filename[PATH_MAX]; // Where is the history saved?

    /* Input processing */
    int escape;                         // Should next char be escaped?

    /* Prompts */
    char prompt[MAX_PROMPT_LENGTH];
    int prompt_length;

    /* Console information */
    int is_console;                     // Are we connected to a console?
    int force_prompt;                   // Force the prompt even if not a console
    HANDLE console_in;                  // Input console
    HANDLE console_out;                 // Screen buffer
    int x, y;                           // Original cursor position
    int w, h;                           // Total size of the window

    // Completion info
    CompletionInfo completion;
} ReadLine;

/*
 * The codes for each operation.
 */
typedef enum {
    CODE_SUCCESS,
    CODE_EOL,
    CODE_EOF
} ProcessCode;

/************************************************************************
 * Utilities.
 */
/*
 * Print an error message.
 */
static void print_error_code(const char *name, DWORD code)
{
    LPTSTR buffer;

    FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER | FORMAT_MESSAGE_FROM_SYSTEM,  // Format flags
                  NULL,                                                         // Location of the message
                  code,                                                         // Error code
                  MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),                    // Language
                  (LPTSTR) &buffer,                                             // Message buffer
                  0,                                                            // Buffer size
                  NULL);                                                        // Arguments

    /* Print the message */
    fprintf(stderr, "%s: failed with code %d: %s\n", name, errno, buffer);
    fflush(stderr);
    LocalFree(buffer);
}

static void print_error(const char *name)
{
    print_error_code(name, GetLastError());
}

/************************************************************************
 * Operations on LineBuffers
 */

/*
 * Index positions.
 */
static int IndexOfNextWord(LineBuffer *linep)
{
    int index;

    index = linep->index;
    while(index < linep->length && linep->buffer[index] > ' ')
        index++;
    while(index < linep->length && linep->buffer[index] <= ' ')
        index++;
    return index;
}

static int IndexOfPrevWord(LineBuffer *linep)
{
    int index;

    index = linep->index;
    while(index && linep->buffer[index] <= ' ')
        index--;
    while(index && linep->buffer[index] > ' ')
        index--;
    return index;
}

/*
 * Move the cursor.
 */
static void LineMoveNextChar(LineBuffer *linep)
{
    if(linep->index != linep->length)
        linep->index++;
}

static void LineMovePrevChar(LineBuffer *linep)
{
    if(linep->index)
        linep->index--;
}

static void LineMoveFirstChar(LineBuffer *linep)
{
    linep->index = 0;
}

static void LineMoveLastChar(LineBuffer *linep)
{
    linep->index = linep->length;
}

static void LineMoveNextWord(LineBuffer *linep)
{
    linep->index = IndexOfNextWord(linep);
}

static void LineMovePrevWord(LineBuffer *linep)
{
    linep->index = IndexOfPrevWord(linep);
}

/*
 * Insert a char at the current position.
 */
static void LineInsertChar(LineBuffer *linep, char c)
{
    int amount;

    if(linep->index == LINE_MAX)
        return;

    /* Insert it */
    amount = linep->length - linep->index;
    memmove(linep->buffer + linep->index + 1, linep->buffer + linep->index, amount);
    linep->buffer[linep->index] = c;
    linep->index++;
    linep->length++;
}

/*
 * Delete characters between the current and the new index.
 */
static void LineDeleteRange(LineBuffer *linep, int index)
{
    int amount, remaining, tmp;

    if(index < linep->index) {
        tmp = index;
        index = linep->index;
        linep->index = tmp;
    }
    amount = index - linep->index;
    remaining = linep->length - index;
    memmove(linep->buffer + linep->index, linep->buffer + index, amount);
    linep->length -= amount;
}

static void LineDeletePrevChar(LineBuffer *linep)
{
    if(linep->index)
        LineDeleteRange(linep, linep->index - 1);
}

static void LineDeleteNextChar(LineBuffer *linep)
{
    if(linep->index < linep->length)
        LineDeleteRange(linep, linep->index + 1);
    else if(linep->index)
        LineDeleteRange(linep, linep->index - 1);
}

static void LineDeletePrevWord(LineBuffer *linep)
{
    LineDeleteRange(linep, IndexOfPrevWord(linep));
}

static void LineDeleteNextWord(LineBuffer *linep)
{
    LineDeleteRange(linep, IndexOfNextWord(linep));
}

static void LineDeleteToBOL(LineBuffer *linep)
{
    LineDeleteRange(linep, 0);
}

static void LineDeleteToEOL(LineBuffer *linep)
{
    linep->length = linep->index;
}

static void LineDeleteLine(LineBuffer *linep)
{
    linep->index = 0;
    linep->length = 0;
}

/*
 * Insert an EOL.
 */
static void LineInsertEOL(LineBuffer *linep)
{
    linep->buffer[linep->length] = '\r';
    linep->buffer[linep->length + 1] = '\n';
    linep->length += 2;
    linep->index = linep->length;
}

/************************************************************************
 * ReadLine operations.
 */

/*
 * Allocate a ReadLine struct.
 */
static ReadLine *AllocReadLine(int is_console, HANDLE console_in, HANDLE console_out)
{
    ReadLine *readp;

    /* Allocate and zero */
    readp = (ReadLine *) malloc(sizeof(ReadLine));
    if(readp) {
        memset(readp, 0, sizeof(ReadLine));
        readp->is_console = is_console;
        readp->console_in = console_in;
        readp->console_out = console_out;
        readp->offset = 1;
    }
    return readp;
}

/*
 * Copy the current line from the history.
 */
static void LoadFromHistory(ReadLine *readp)
{
    LineBuffer *linep;

    linep = readp->history + ((readp->start + readp->index) % MAX_HISTORY_LENGTH);
    readp->current = *linep;
}

/*
 * Set the console stats.
 */
static int ConsoleRaw(ReadLine *readp)
{
    CONSOLE_SCREEN_BUFFER_INFO info;
    int status;

    /* Set in raw mode */
    status = SetConsoleMode(readp->console_in, RAW_CONSOLE_MODE);
    if(status == 0) {
        print_error("SetConsoleMode");
        return -1;
    }
    status = SetConsoleMode(readp->console_out, ENABLE_PROCESSED_OUTPUT | ENABLE_WRAP_AT_EOL_OUTPUT);
    if(status == 0) {
        print_error("SetConsoleMode");
        return -1;
    }

    /* Remember where the cursor currently is */
    status = GetConsoleScreenBufferInfo(readp->console_out, &info);
    if(status == 0) {
        print_error("GetConsoleScreenBufferInfo");
        readp->x = 0;
        readp->y = 0;
        readp->w = 80;
        readp->h = 24;
    }
    else {
        readp->x = info.dwCursorPosition.X;
        readp->y = info.dwCursorPosition.Y;
        readp->w = info.dwSize.X;
        readp->h = info.dwSize.Y;
        if(readp->w <= 0)
            readp->w = 1;
        if(readp->h <= 0)
            readp->h = 1;
    }
    return 0;
}

/*
 * Set the console stats.
 */
static int ConsoleCooked(ReadLine *readp)
{
    int status;

    status = SetConsoleMode(readp->console_in, COOKED_CONSOLE_MODE);
    if(status == 0) {
        print_error("SetConsoleMode");
        return -1;
    }
    return 0;
}

/*
 * Move the cursor to the right index.
 */
static void UpdateCursor(ReadLine *readp, int index)
{
    int i, x, y;
    char *bufp;
    COORD pos;

    bufp = readp->current.buffer;
    x = readp->x;
    y = readp->y;
    for(i = 0; i != index; i++) {
        switch(bufp[i]) {
        case 0:
            break;
        case '\r':
            x = 0;
            break;
        case '\n':
            y++;
            break;
        case '\t':
            x = (x + 8) & ~7;
            break;
        default:
            x++;
            break;
        }
        if(x == readp->w) {
            x = 0;
            y++;
        }
        if(y == readp->h) {
            readp->y--;
            y = readp->h - 1;
        }
    }

    /* Set the cursor */
    pos.X = (SHORT) x;
    pos.Y = (SHORT) y;
    SetConsoleCursorPosition(readp->console_out, pos);
}

/*
 * Go to the next line in the history.
 */
static void ReadNextLine(ReadLine *readp)
{
    if(readp->index < readp->length)
        readp->index++;
    LoadFromHistory(readp);
}

static void ReadPrevLine(ReadLine *readp)
{
    if(readp->index > 0)
        readp->index--;
    LoadFromHistory(readp);
}

/*
 * Clear the screen.
 */
static void ReadClearScreen(ReadLine *readp)
{
    COORD pos;
    DWORD count;

    /* Flood the screen with whitespace */
    pos.X = 0;
    pos.Y = 0;
    count = readp->w * readp->h;
    FillConsoleOutputCharacter(readp->console_out, (TCHAR) ' ', count, pos, &count);

    /* Move the cursor and print the prompt */
    readp->x = 0;
    readp->y = 0;
    UpdateCursor(readp, 0);
    WriteFile(readp->console_out,
              readp->prompt,
              readp->prompt_length,
              &count,
              NULL);

    /* Move the standard cursor */
    readp->y = readp->prompt_length / readp->w;
    readp->x = readp->prompt_length % readp->w;
    UpdateCursor(readp, 0);
}

/*
 * Refresh the current line in the console.
 */
static void Refresh(ReadLine *readp)
{
    LineBuffer *old, *current;
    DWORD count;
    int end;

    old = &readp->display;
    current = &readp->current;

    /* Write to max of old and new lengths */
    if(current->length < old->length)
        end = old->length;
    else
        end = current->length;

    /* Copy the entire line, and pad with spaces */
    memcpy(old->buffer, current->buffer, current->length);
    memset(old->buffer + current->length, ' ', end - current->length);

    /* Write the data to the console */
    UpdateCursor(readp, 0);
    WriteFile(readp->console_out,               // Echo to the console
              old->buffer,                      // Data to write
              end,                              // Write all characters
              &count,                           // Number of bytes written
              NULL);                            // Not overlapped
    UpdateCursor(readp, current->index);
    old->index = current->index;
    old->length = current->length;
}

/************************************************************************
 * Processor.
 */

/*
 * Send the character to the current process.
 */
static ProcessCode process_char(ReadLine *readp, char c)
{
    LineBuffer *linep;
    ProcessCode code;

    code = CODE_SUCCESS;
    linep = &readp->current;
    if(readp->escape) {
        readp->escape = 0;
        LineInsertChar(linep, c);
    }
    else {
        switch(c) {
        case 0:
            break;
        case 'A'-'@':
            LineMoveFirstChar(linep);
            break;
        case 'B'-'@':
            LineMovePrevChar(linep);
            break;
        case 'D'-'@':
            code = CODE_EOF;
            break;
        case 'E'-'@':
            LineMoveLastChar(linep);
            break;
        case 'F'-'@':
            LineMoveNextChar(linep);
            break;
        case 'H'-'@':
            LineDeletePrevChar(linep);
            break;
        case 'K'-'@':
            LineDeleteToEOL(linep);
            break;
        case 'L'-'@':
            ReadClearScreen(readp);
            break;
        case 'P'-'@':
            ReadPrevLine(readp);
            break;
        case 'N'-'@':
            ReadNextLine(readp);
            break;
        case 'V'-'@':
            readp->escape = 1;
            break;
        case 'W'-'@':
            LineDeletePrevWord(linep);
            break;
        case 'X'-'@':
        case 'C'-'@':
            LineDeleteLine(linep);
            break;
        case 127:
            LineDeleteNextChar(linep);
            break;
        case '\n':
        case '\r':
            LineInsertEOL(linep);
            code = CODE_EOL;
            break;
        case '\\'-'@':
        case 'Z'-'@':
            break;
        default:
            LineInsertChar(linep, c);
            break;
        }
    }
    Refresh(readp);
    return code;
}

/*
 * Input loop.
 */
static ProcessCode processor(ReadLine *readp)
{
    INPUT_RECORD input[INPUT_COUNT];
    INPUT_RECORD *event;
    KEY_EVENT_RECORD *key;
    ProcessCode code;
    int status;
    DWORD count, i;
    char c;

    /* Input loop */
    while(1) {
        caml_enter_blocking_section();
        status = ReadConsoleInput(readp->console_in, input, INPUT_COUNT, &count);
        caml_leave_blocking_section();
        if(status == 0) {
            print_error("ReadConsoleInput");
            return CODE_EOF;
        }

        /* Perform each event */
        for(i = 0; i != count; i++) {
            event = &input[i];
            switch(event->EventType) {
            case KEY_EVENT:
                /* Send the key to the process */
                key = &event->Event.KeyEvent;
                c = key->uChar.AsciiChar;
                if(key->bKeyDown) {
                    code = process_char(readp, c);
                    if(code != CODE_SUCCESS)
                        return code;
                }
                break;
            case MOUSE_EVENT:
            case WINDOW_BUFFER_SIZE_EVENT:
            case MENU_EVENT:
            case FOCUS_EVENT:
            default:
                break;
            }
        }
    }
}

/*
 * Read in cooked mode.
 */
static void readline_cooked(ReadLine *readp)
{
    char *s;

    caml_enter_blocking_section();
    s = fgets(readp->current.buffer, LINE_MAX, stdin);
    caml_leave_blocking_section();
    if(s == 0)
        caml_raise_end_of_file();
    readp->current.length = strlen(readp->current.buffer);
}

/*
 * Read in raw console mode.
 */
static void readline_raw(ReadLine *readp)
{
    /* Process in raw mode */
    ConsoleRaw(readp);
    processor(readp);
    ConsoleCooked(readp);
}

/*
 * Once reading is done, reset all the buffers.
 */
static void readline_done(ReadLine *readp)
{
    LineBuffer *linep;
    char *strp;
    int i, c;

    /* Copy the current line to the history */
    linep = readp->history + ((readp->start + readp->length) % MAX_HISTORY_LENGTH);
    *linep = readp->current;

    /* Do not include the eol in the history */
    strp = linep->buffer;
    i = linep->length;
    while(i > 0) {
        c = strp[i - 1];
        if(c == '\n' || c == '\r')
            i--;
        else
            break;
    }
    strp[i] = 0;
    linep->length = i;
    linep->index = i;

    /* Scroll the history if the line is nonempty */
    if(i) {
        if(readp->length < MAX_HISTORY_LENGTH - 1)
            readp->length++;
        else {
            readp->start = (readp->start + 1) % MAX_HISTORY_LENGTH;
            readp->offset++;
        }
        readp->index = readp->length;
    }

    /* Clear the next line in the history */
    linep = readp->history + ((readp->start + readp->length) % MAX_HISTORY_LENGTH);
    linep->index = 0;
    linep->length = 0;

    /* Clear the display buffer */
    linep = &readp->display;
    linep->index = 0;
    linep->length = 0;

    /* Data is copied from the current buffer */
    linep = &readp->current;
    linep->index = 0;
}

/*
 * Read a line from the console.
 */
static void readline(ReadLine *readp, const char *promptp)
{
    int len;

    /* Get the prompt */
    len = strlen(promptp);
    if(len > sizeof(readp->prompt) - 1)
        len = sizeof(readp->prompt) - 1;
    memcpy(readp->prompt, promptp, len);
    readp->prompt[len] = 0;
    readp->prompt_length = len;

    /* Display the prompt */
    if(readp->is_console || readp->force_prompt) {
        printf("%s", readp->prompt);
        fflush(stdout);
    }

    /* Read the input line */
    if(readp->is_console)
        readline_raw(readp);
    else
        readline_cooked(readp);

    /* Add this line to the history */
    readline_done(readp);
}

/*
 * Load the history from a file.
 */
static int do_readline_load_history(ReadLine *readp, const char *filenamep)
{
    char line[LINE_MAX + 32];
    LineBuffer *linep;
    FILE *filep;
    char *strp;

    if(strlen(filenamep) >= PATH_MAX)
        return -1;
    strcpy(readp->filename, filenamep);
    if((filep = fopen(filenamep, "r")) == NULL)
        return -1;

    while(fgets(line, sizeof(line) - 1, filep)) {
        /* Skip over the line number */
        strp = line;
        while(isspace(*strp))
            strp++;
        while(isdigit(*strp))
            strp++;
        while(isspace(*strp))
            strp++;

        /* Copy into a line buffer */
        linep = readp->history + ((readp->start + readp->length) % MAX_HISTORY_LENGTH);
        linep->length = strlen(strp);
        if(linep->length == 0)
            continue;
        if(linep->length > LINE_MAX)
            linep->length = LINE_MAX;
        else
            linep->length--;
        memcpy(linep->buffer, strp, linep->length);
        linep->index = linep->length;

        /* Now scroll the history */
        if(readp->length < MAX_HISTORY_LENGTH - 1)
            readp->length++;
        else {
            readp->start = (readp->start + 1) % MAX_HISTORY_LENGTH;
            readp->offset++;
        }
        readp->index = readp->length;
    }
    fclose(filep);
    return 0;
}

/*
 * Save the history to a file.
 */
static int do_readline_save_history(ReadLine *readp)
{
    int i, offset, start, length;
    LineBuffer *linep;
    FILE *filep;

    if(*readp->filename == 0)
        return -1;
    if((filep = fopen(readp->filename, "w")) == NULL)
        return -1;

    offset = readp->offset;
    start = readp->start;
    length = readp->length;
    for(i = 0; i < length; i++) {
        linep = readp->history + ((start + i) % MAX_HISTORY_LENGTH);
        linep->buffer[linep->length] = 0;
        fprintf(filep, "%5d %s\n", offset + i, linep->buffer);
    }

    fclose(filep);
    return 0;
}

/************************************************************************
 * Main routines.
 */

/*
 * Only one readline buffer.
 */
static ReadLine *readp;

/*
 * Is input a tty?
 */
value omake_isatty(value v_unit)
{
    return Val_int(readp->is_console);
}

value omake_is_interactive(value v_unit)
{
    return Val_int(readp->is_console || readp->force_prompt);
}

value omake_readline_flush(value v_unit)
{
    return Val_unit;
}

/*
 * Some of these history functions can be implemented.
 */
value omake_where_history(value v_unit)
{
    return Val_int(readp->offset + readp->length);
}

value omake_readline_load_file(value v_file)
{
    do_readline_load_history(readp, String_val(v_file));
    return Val_unit;
}

value omake_readline_save_file(value v_unit)
{
    do_readline_save_history(readp);
    return Val_unit;
}

value omake_readline_set_length(value v_length)
{
    return Val_unit;
}

value omake_readline_set_directory(value v_dir)
{
    return Val_unit;
}

value omake_readline_history(value v_unit)
{
    int i, length, start, offset;
    char store[MAX_HISTORY_LENGTH][100];
    char *data[MAX_HISTORY_LENGTH + 1];
    char *strp;
    LineBuffer *linep;

    start = readp->start;
    length = readp->length;
    offset = readp->offset;
    for(i = 0; i < length; i++) {
        linep = readp->history + ((start + i) % MAX_HISTORY_LENGTH);
        strp = store[i];
        sprintf(strp, "%-5d %.70s", i + offset, linep->buffer);
        data[i] = strp;
    }
    data[i] = 0;
    return caml_copy_string_array((char const **) data);
}

/*
 * Force the prompt anyway.
 */
value omake_interactive(value v_bool)
{
    int interactive = Int_val(v_bool);
    if(interactive)
        readp->force_prompt = 1;
    else {
        readp->is_console = 0;
        readp->force_prompt = 0;
    }
    return Val_unit;
}

/*
 * Read an entire line from the input.
 */
value omake_readline(value v_prompt)
{
    LineBuffer *linep;
    value v_str;

    /* Get the line */
    readline(readp, String_val(v_prompt));

    /* Copy it to a string */
    linep = &readp->current;
    v_str = caml_alloc_string(linep->length);
    memcpy(String_val(v_str), linep->buffer, linep->length);

    /* Reset the current buffer */
    linep->index = 0;
    linep->length = 0;
    return v_str;
}

/*
 * Read a single character from the input.
 */
value omake_readstring(value v_prompt, value v_buf, value v_off, value v_len)
{
    LineBuffer *linep;
    int off, len, amount;
    char *buf, *prompt;

    /* If the buffer is empty, read the next line */
    prompt = String_val(v_prompt);
    linep = &readp->current;
    if(linep->index == linep->length) {
        linep->index = 0;
        linep->length = 0;
        readline(readp, prompt);
    }

    /* Get as much as possible */
    buf = String_val(v_buf);
    off = Int_val(v_off);
    len = Int_val(v_len);
    amount = linep->length - linep->index;
    if(amount > len)
        amount = len;
    memcpy(buf + off, linep->buffer + linep->index, amount);
    linep->index += amount;
    return Val_int(amount);
}

/*
 * Attach to the console.
 */
value omake_readline_init(value v_unit)
{
    HANDLE c_stdin, c_stdout, handle;
    DWORD status, mode;
    int is_console;

    /* Get the stdin handle */
    c_stdin = GetStdHandle(STD_INPUT_HANDLE);
    c_stdout = GetStdHandle(STD_OUTPUT_HANDLE);
    if(c_stdin == INVALID_HANDLE_VALUE || c_stdout == INVALID_HANDLE_VALUE)
        caml_failwith("omake_readline_init: no standard channels");

    /* Check if it is a console */
    is_console = 1;
    status = GetConsoleMode(c_stdin, &mode);
    if(status) {
        /* Make sure output is to the console */
        handle = CreateFile("CONOUT$",
                            GENERIC_READ | GENERIC_WRITE,
                            FILE_SHARE_READ | FILE_SHARE_WRITE,
                            NULL,
                            OPEN_EXISTING,
                            FILE_ATTRIBUTE_NORMAL,
                            NULL);
        if(handle == INVALID_HANDLE_VALUE)
            is_console = 0;
        else
            c_stdout = handle;
    }
    else
        is_console = 0;

    /* Good, we have a console */
    readp = AllocReadLine(is_console, c_stdin, c_stdout);
    return Val_unit;
}

#else /* !WIN32 */

/************************************************************************
 * Unix readline interface.
 */

#ifdef RL_PROMPT_START_IGNORE
#define INVIS_START RL_PROMPT_START_IGNORE
#define INVIS_END   RL_PROMPT_END_IGNORE
#else
#define INVIS_START '\001'
#define INVIS_END   '\002'
#endif

/*
 * Input state.
 */
typedef struct _readline {
    /* Console info */
    int console_in;
    int is_console;

    /* Prompts */
    char * prompt;
    int prompt_size;
    int force_prompt;

    /* Buffered data */
    char * buffer;
    int buffer_size;
    int index;
    int length;

    /* Current directory */
    char dir[PATH_MAX];
} ReadLine;

/*
 * Allocate the struct.
 */
static ReadLine *AllocReadLine(int is_console, int console_in)
{
    ReadLine *readp;

    /* Allocate */
    readp = (ReadLine *) malloc(sizeof(ReadLine));
    if(readp == NULL)
        caml_failwith("AllocReadLine: out of memory");
    memset(readp, 0, sizeof(ReadLine));

    /* Initialize */
    readp->buffer = malloc(LINE_MAX);
    if (readp->buffer == NULL)
        caml_failwith("AllocReadLine: out of memory");
    readp->buffer_size = LINE_MAX;

    readp->prompt = malloc(MAX_PROMPT_LENGTH);
    if (readp->prompt == NULL)
        caml_failwith("AllocReadLine: out of memory");
    readp->prompt_size = MAX_PROMPT_LENGTH;

    readp->console_in = console_in;
    readp->is_console = is_console;
    readp->dir[0] = '/';
    return readp;
}

/*
 * Read in cooked mode.
 */
static void readline_cooked(ReadLine *readp)
{
    if(readp->is_console || readp->force_prompt) {
        printf("%s", readp->prompt);
        fflush(stdout);
    }
    if(fgets(readp->buffer, readp->buffer_size, stdin) == NULL)
        readp->length = 0;
    else
        readp->length = strlen(readp->buffer);
}

/*
 * Read in raw console mode.
 */
#ifdef READLINE_ENABLED
static void readline_raw(ReadLine *readp)
{
    char *linep, *expansion = NULL;
    int length, result;

  prompt:
#ifdef READLINE_GNU
    rl_on_new_line();
#endif
    linep = readline(readp->prompt);
    if(linep && *linep && (readp->is_console || readp->force_prompt)) {
        /* Perform a history expansion */
        result = history_expand(linep, &expansion);
        free(linep);

        switch(result) {
        case 0:
            /* No expansion */
            break;
        case 1:
            /* There was an expansion */
            printf("%s\n", expansion);
            fflush(stdout);
            break;
        case 2:
            /* Display, but do not execute */
            printf("%s\n", expansion);
            fflush(stdout);
            free(expansion);
            goto prompt;
        default:
            /* There was an error during expansion */
            printf("History expansion failed\n");
            fflush(stdout);
            free(expansion);
            goto prompt;
        }

        /* Successful expansion */
        add_history(expansion);
        length = strlen(expansion);
        if(length >= readp->buffer_size) {
            char *new_buffer = malloc(length + 1);
            if(new_buffer == NULL)
                caml_failwith("readline_raw: out of memory");
            free(readp->buffer);
            readp->buffer = new_buffer;
            readp->buffer_size = length + 1;
        }
        memcpy(readp->buffer, expansion, length);
        readp->buffer[length] = '\n';
        readp->length = length + 1;
        free(expansion);
    }
    else if(linep) {
        free(linep);
        readp->buffer[0] = '\n';
        readp->length = 1;
    }
    else
        readp->length = 0;
}
#else /* !READLINE_ENABLED */
#define readline_raw readline_cooked
#endif /* !READLINE_ENABLED */

/*
 * Read a line from the console.
 */
static void do_readline(ReadLine *readp, const char *promptp)
{
    int i;

    /* Get the prompt */
    int is_vis = 1, vis_length = 0;
    int last_vis[3] = {0,0,0}, vis_ind = 0;
#ifdef RL_PROMPT_START_IGNORE
    int copy_markers = readp -> is_console;
#else
#define copy_markers 0
#endif
    for(i = 0; *promptp; promptp++) {
        if (i == readp->prompt_size - 1) {
            /* Ran out of memory, allocate bigger buffer */
            char * old_prompt = readp->prompt;
            int new_size = readp->prompt_size * 2;
            readp->prompt = malloc(new_size);
            if (readp->prompt == NULL) {
                readp->prompt = old_prompt;
                caml_failwith("do_readline: out of memory");
            } else {
                memcpy(readp->prompt, old_prompt, i);
                free(old_prompt);
                readp->prompt_size = new_size;
            }
        } 
        if (*promptp == INVIS_START)
            is_vis = 0;
        if (is_vis) {
            vis_length++;
            if (vis_length == MAX_PROMPT_LENGTH + 1) {
                /* We just ran out */
                readp->prompt[last_vis[0]] = '.';
                readp->prompt[last_vis[1]] = '.';
                readp->prompt[last_vis[2]] = '.';
            }
        }

        if (((!is_vis) || (vis_length <= MAX_PROMPT_LENGTH))
                && (copy_markers
                    || ((*promptp != INVIS_START) && (*promptp != INVIS_END)))) {
            if (is_vis)
                last_vis[vis_ind++ % 3] = i;
            readp->prompt[i++] = *promptp;
        }
        if (*promptp == INVIS_END)
            is_vis = 1;
    }
    readp->prompt[i] = 0;

    /* Read the input line */
    if(readp->is_console)
        readline_raw(readp);
    else
        readline_cooked(readp);
    readp->index = 0;
}

/*
 * History file and length.
 */
static char readline_file[2048];
static int readline_length;

static void do_readline_load_file(const char *filep)
{
#ifdef READLINE_ENABLED
    /* Check the length */
    if(strlen(filep) >= sizeof(readline_file))
        caml_invalid_argument("omake_readline_file: filename is too long");

    /* If the file has changed, load the file */
    if(strcmp(readline_file, filep)) {
        strcpy(readline_file, filep);
        read_history(readline_file);
    }
#endif /* READLINE_ENABLED */
}

static void do_readline_save_file()
{
#ifdef READLINE_ENABLED
    if(*readline_file)
        write_history(readline_file);
#endif /* READLINE_ENABLED */
}

static void do_readline_set_length(int length)
{
#ifdef READLINE_ENABLED
    /* If the length has changed, stifle the history */
    if(length != readline_length) {
        readline_length = length;
        stifle_history(readline_length);
    }
#endif /* READLINE_ENABLED */
}

/************************************************************************
 * Completion functions.
 */
static ReadLine *readp;

#ifdef READLINE_ENABLED
/*
 * Flatten the list.
 */
static char **readline_completion_matches(const char *text, int first, int last)
{
    char **matches;
    int amount;

    /* Sanity checking */
    amount = last - first;
    if(amount <= 0 || amount >= LINE_MAX)
        return NULL;

    /* Three kinds of completion */
    if(first == 0)
        matches = readline_completion(omake_command_completion, text);
    else
        matches = readline_completion(omake_filename_completion, text);
    return matches;
}

#endif /* READLINE_ENABLED */

/************************************************************************
 * Public functions.
 */

/*
 * Is input a tty?
 */
value omake_isatty(value v_unit)
{
    return Val_int(readp->is_console);
}

value omake_is_interactive(value v_unit)
{
    return Val_int(readp->is_console || readp->force_prompt);
}

value omake_readline_flush(value v_unit)
{
    if(readp->is_console)
        fflush(stdin);
    return Val_unit;
}

value omake_where_history(value v_unit)
{
    int i;
#ifdef READLINE_ENABLED
    i = history_base + history_length;
#else
    i = 1;
#endif
    return Val_int(i);
}

value omake_readline_load_file(value v_file)
{
    do_readline_load_file(String_val(v_file));
    return Val_unit;
}

value omake_readline_save_file(value v_unit)
{
    do_readline_save_file();
    return Val_unit;
}

value omake_readline_set_length(value v_length)
{
    do_readline_set_length(Int_val(v_length));
    return Val_unit;
}

value omake_readline_set_directory(value v_dir)
{
    /* Copy the current directory */
    strncpy(readp->dir, String_val(v_dir), PATH_MAX - 1);
    readp->dir[PATH_MAX - 1] = 0;
    return Val_unit;
}

value omake_readline_history(value v_unit)
{
#ifdef READLINE_ENABLED
    char *data[history_length + 1];
    char entries[history_length][80];
    int i;

#ifdef READLINE_GNU
    HIST_ENTRY **the_list;
    the_list = history_list();
    i = 0;
    if(the_list) {
        while(the_list[i]) {
            data[i] = entries[i];
            sprintf(data[i], "%-5d %.70s", i + history_base, the_list[i]->line);
            data[i][79] = 0;
            i++;
        }
    }
    data[i] = 0;
#else /* !READLINE_GNU */
    /*
     * BSD readline doesn't have history_list, or any way to get the complete
     * history in one call.
     */
    for(i = 0; i < history_length; i++) {
        HIST_ENTRY *entryp;
        entryp = history_get(history_base + i);
        if(entryp == 0)
            break;
        data[i] = entries[i];
        sprintf(data[i], "%-5d %.70s", i + history_base, entryp->line);
        data[i][79] = 0;
    }
    data[i] = 0;
#endif /* !READLINE_GNU */
#else /* !READLINE_ENABLED */
    char *data[1];
    data[0] = 0;
#endif /* !READLINE_ENABLED */
    return caml_copy_string_array((char const **) data);
}

/*
 * Force the prompt anyway.
 */
value omake_interactive(value v_bool)
{
    int interactive = Int_val(v_bool);
    if(interactive)
        readp->force_prompt = 1;
    else {
        readp->is_console = 0;
        readp->force_prompt = 0;
    }
    return Val_unit;
}

/*
 * Read an entire line from the input.
 */
value omake_readline(value v_prompt)
{
    value v_str;

    /* Get the line */
    do_readline(readp, String_val(v_prompt));

    /* Copy it to the buffer */
    v_str = caml_alloc_string(readp->length);
    memcpy(String_val(v_str), readp->buffer, readp->length);

    /* Reset the current buffer */
    readp->index = 0;
    readp->length = 0;
    return v_str;
}

/*
 * Read a single character from the input.
 */
value omake_readstring(value v_prompt, value v_buf, value v_off, value v_len)
{
    int off, len, amount;
    char *buf, *prompt;

    /* If the buffer is empty, read the next line */
    prompt = String_val(v_prompt);
    if(readp->index == readp->length) {
        readp->index = 0;
        readp->length = 0;
        do_readline(readp, prompt);
    }

    /* Get as much as possible */
    buf = String_val(v_buf);
    off = Int_val(v_off);
    len = Int_val(v_len);
    amount = readp->length - readp->index;
    if(amount > len)
        amount = len;
    memcpy(buf + off, readp->buffer + readp->index, amount);
    readp->index += amount;
    return Val_int(amount);
}

/*
 * Attach to the console.
 */
value omake_readline_init(value v_unit)
{
    readp = AllocReadLine(isatty(0), 0);
#ifdef READLINE_ENABLED
    using_history();
    rl_attempted_completion_function = readline_completion_matches;
    rl_completion_append_character = 0;
#endif
    return Val_unit;
}

#endif /* !WIN32 */

value omake_rl_prompt_wrappers(value v_unit) {
    CAMLparam1(v_unit);
    CAMLlocal1(buf);

#ifdef INVIS_START
    {
        char begin[2] = { INVIS_START, 0};
        char end  [2] = { INVIS_END, 0};
        CAMLlocal2(s1, s2);
        s1 = caml_copy_string(begin);
        s2 = caml_copy_string(end);
        buf = caml_alloc_tuple(2);
        Field(buf, 0) = s1;
        Field(buf, 1) = s2;
    }
#else /* INVIS_START */
    {
        CAMLlocal1(emptystr);
        emptystr = caml_copy_string("");
        buf = caml_alloc_tuple(2);
        Field(buf, 0) = emptystr;
        Field(buf, 1) = emptystr;
    }
#endif /* INVIS_START */
    CAMLreturn(buf);
}

/*
 * vim:tw=100:ts=4:et:sw=4:cin
 */
