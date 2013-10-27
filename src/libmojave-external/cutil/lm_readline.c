/*cxxx
 * Readline Interface for MCC
 * Copyright(c) 2002 Justin David Smith, Caltech
 * Derived from MetaPRL readline code
 */


/*
 * Call the readline package.
 */
#include <stdio.h>
#include <string.h>
#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/fail.h>

#if READLINE
#include <readline/readline.h>
#include <readline/history.h>
#include <errno.h>
#endif


/*
 * Max line length without readline.
 */
#define MAX_LINE_LENGTH         1024


/***  Readline Tab Completion Stubs  ***/


#if READLINE

static int list_index = 0;
static int len = 0;

static char **commands = NULL;


/*
 * Delete a chain of commands
 */
static void destruct_command_list() {

   char **cptr = commands;
   if(cptr == NULL) return;
   while(*cptr != NULL) {
      free(*cptr);
      ++cptr;
   }
   free(commands);
   commands = NULL;

}


/*
 * Clone a string.
 */
static char *dupstr(const char *s) {

   char *r = malloc(strlen(s) + 1);
   if(r == NULL) return(NULL);
   strcpy(r, s);
   return(r);

}


/*
 * Generator function for command completion.  If state == 0,
 * then we start at the top of the list; otherwise, we use our
 * pre-existing state to resume a pending search.
 */
static char *command_generator(char *text, int state) {

   const char *name;

   /* Make sure we have a command list to process */
   if(commands == NULL) return(NULL);

   /* Are we starting on a new word? */
   if(state == 0) {
      list_index = 0;
      len = strlen(text);
   }

   /* Return the next command name which partially matches the text */
   while((name = commands[list_index]) != NULL) {
      ++list_index;
      if(strncmp(name, text, len) == 0) {
         return(dupstr(name));
      }
   }

   /* If nothing matched, then return NULL */
   return(NULL);

}


/*
 * Attempt command-name completion (assuming a list of
 * commands have been registered into the system).
 */
static char **command_completion(char *text, int start, int end) {

   char **matches = NULL;

   if(start == 0) {
      matches = RL_COMPLETION_MATCHES(text, (RL_CP_TYPE *)command_generator);
   }

   return(matches);

}


/*
 * Actually initialize readline.
 */
value caml_initialize_readline(value unit) {

   CAMLparam1(unit);

   /* Setup the default command table */
   commands = NULL;

   /* Tell the completer about our command completion engine */
   rl_attempted_completion_function = (RL_CPP_TYPE *)command_completion;

   /* Set horizontal scroll mode; other modes screw up display */
   rl_variable_bind("horizontal-scroll-mode", "on");

   /* Disable the bell */
   rl_variable_bind("bell-style", "none");

   CAMLreturn(Val_unit);

}


/*
 * Register a new list of commands.
 */
value caml_register_commands(value new_commands) {

   CAMLparam1(new_commands);
   CAMLlocal1(cptr);
   int length;

   /* Try to figure out the length of this list... */
   cptr = new_commands;
   length = 0;
   while(Is_block(cptr)) {
      cptr = Field(cptr, 1);
      ++length;
   }

   /* Allocate the real, internal command structure */
   destruct_command_list();
   commands = malloc(sizeof(char *) * (length + 1));
   if(commands == NULL) {
      CAMLreturn(Val_unit);
   }

   cptr = new_commands;
   length = 0;
   while(Is_block(cptr)) {
      /* This is a CONS cell (I hope!) */
      commands[length] = dupstr(String_val(Field(cptr, 0)));
      cptr = Field(cptr, 1);
      ++length;
   }

   /* Clear the final entry in the commands list */
   commands[length] = NULL;

   /* We were apparently successful */
   CAMLreturn(Val_unit);

}

value caml_read_history(value name) {

    CAMLparam1(name);
    int result;
    result = read_history( String_val(name) );
    if (result == ENOENT) {
        raise_not_found();
    }
    else if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}

value caml_write_history(value name) {

    CAMLparam1(name);
    int result;
    result = write_history( String_val(name) );
    if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}

value caml_history_truncate_file(value name, value nlines) {

    CAMLparam2(name, nlines);
    int result;
    result = history_truncate_file( String_val(name), Long_val(nlines) );
    if (result != 0) {
        CAMLlocal1(error);
        error = copy_string(strerror( result ));
        raise_sys_error( error );
    }
    CAMLreturn(Val_unit);

}




#else /* No READLINE... */


/*
 * Nothing to initialize :)
 */
value caml_initialize_readline(value unit) {

   CAMLparam1(unit);
   CAMLreturn(Val_unit);

}


/*
 * Doesn't make much sense to register commands...
 */
value caml_register_commands(value new_commands) {

   CAMLparam1(new_commands);
   CAMLreturn(Val_unit);

}

value caml_read_history(value name) {

   CAMLparam1(name);
   CAMLreturn(Val_unit);

}
value caml_write_history(value name) {

   CAMLparam1(name);
   CAMLreturn(Val_unit);

}
value caml_history_truncate_file(value name, value len) {

   CAMLparam2(name,len);
   CAMLreturn(Val_unit);

}

#endif /* READLINE enabled? */


/***  OCaml Interface  ***/


/*
 * Read a line into a string buffer.
 * Returns a string option, None at EOF.
 */
value caml_readline(value prompt_arg) {

   CAMLparam1(prompt_arg);
   CAMLlocal2(v, b);
   char *line;

#if READLINE

   line = readline(String_val(prompt_arg));

   /* Readline returns null on EOF */
   if(line == NULL) {
      /* None */
      CAMLreturn(Val_int(0));
   }

   /* This (probably) copies the line */
   if(line != NULL && *line != '\0') {
      /* Add nonempty lines to the history */
      add_history(line);
   }

#else /* No READLINE */

   char *bufp;

   bufp = malloc(MAX_LINE_LENGTH);
   if(bufp == NULL) {
      /* Pretend that we have reached EOF */
      CAMLreturn(Val_int(0));
   }

   /* Get the line (make sure string is terminated) */
   bufp[MAX_LINE_LENGTH - 1] = '\0';
   fputs(String_val(prompt_arg), stdout);
   fflush(stdout);
   line = fgets(bufp, MAX_LINE_LENGTH - 1, stdin);

   /* Readline returns null on EOF */
   if(line == NULL) {
      /* None */
      free(bufp);
      CAMLreturn(Val_int(0));
   }

#endif /* READLINE enabled? */

   /* Copy the line */
   v = copy_string(line);

   /* Some v */
   b = alloc(1, 0);
   Field(b, 0) = v;

   /* Free the buffer */
   free(line);

   CAMLreturn(b);

}
