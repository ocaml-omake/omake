(*
 * Functions and application.
 *
 * \begin{doc}
 * \section{First-class functions}
 * \end{doc}
 *)
include Omake_pos.Make (struct let name = "Omake_builtin_fun" end)


(*
 * Anonymous functions.
 *
 * \section{Functions}
 *
 * \begin{doc}
 * \fun{fun}
 *
 * The \verb+fun+ form introduces anonymous functions.
 *
 * \verb+$(fun <v1>, ..., <vn> => <body>)+
 *
 * The last argument is the body of the function.
 * The other arguments are the parameter names.
 *
 * The three following definitions are equivalent.
 *
 * \begin{verbatim}
 *     F(X, Y) =
 *        return($(addsuffix $(Y), $(X)))
 *
 *     F = $(fun X, Y => $(addsuffix $(Y), $(X)))
 *
 *     F =
 *        fun(X, Y) =>
 *           value $(addsuffix $(Y), $(X))
 * \end{verbatim}
 * \end{doc}
 *)
let fun_fun _ pos loc args =
   let pos = string_pos "fun" pos in
      match args with
         [arg] ->
            arg
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, List.length args)))

(*
 * Function application.
 *
 * \begin{doc}
 * \fun{apply}
 *
 * The \verb+apply+ operator is used to apply a function.
 *
 * \verb+$(apply <fun>, <args>)+
 *
 * Suppose we have the following function definition.
 *
 * \begin{verbatim}
 *     F(X, Y) =
 *        return($(addsuffix $(Y), $(X)))
 * \end{verbatim}
 *
 * The the two expressions below are equivalent.
 *
 * \begin{verbatim}
 *     X = F(a b c, .c)
 *     X = $(apply $(F), a b c, .c)
 * \end{verbatim}
 *
 * The \verb+apply+ form can also be used for partial applications,
 * where a function is passed fewer arguments than it expects.  The
 * result is a function that takes the remaining arguments,
 * and calls the function with the full set of arguments.
 *
 * \begin{verbatim}
 *     add2(i, j) =
 *        add($i, $j)
 *     succ = $(apply $(add2), 1)
 *     i = $(succ 5)   # Computes 1+5
 * \end{verbatim}
 * \end{doc}
 *)
let apply_fun venv pos loc args kargs =
   let pos = string_pos "apply" pos in
   let fun_val, args =
      match args with
         fun_val :: args ->
            fun_val, args
       | [] ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 1, 0)))
   in
      Omake_eval.eval_partial_apply venv pos loc fun_val args kargs

(*
 * Function application.
 *
 * \begin{doc}
 * \fun{applya}
 *
 * The \verb+applya+ operator is used to apply a function to
 * an array of arguments.
 *
 * \verb+$(applya <fun>, <args>)+
 *
 * For example, in the following program, the value
 * of \verb+Z+ is \verb+file.c+.
 *
 * \begin{verbatim}
 *     F(X, Y) =
 *        return($(addsuffix $(Y), $(X)))
 *     args[] =
 *        file
 *        .c
 *     Z = $(applya $(F), $(args))
 * \end{verbatim}
 *
 * The \verb+applya+ form can also be used for partial applications.
 * \end{doc}
 *)
let applya_fun venv pos loc args kargs =
   let pos = string_pos "applya" pos in
   let fun_val, args =
      match args with
         [fun_val; args] ->
            fun_val, args
       | _ ->
            raise (Omake_value_type.OmakeException (loc_pos loc pos, ArityMismatch (ArityExact 2, 0)))
   in
   let args = Omake_value.values_of_value venv pos args in
      Omake_eval.eval_partial_apply venv pos loc fun_val args kargs

(************************************************************************
 * Tables.
 *)
let () =
  let builtin_funs =
    [false, "fun",                  fun_fun,             Omake_ir.ArityExact 1]
  in
  let builtin_kfuns =
    [true,  "apply",                apply_fun,           Omake_ir.ArityAny;
     true,  "applya",               applya_fun,          ArityAny;
    ]
  in
  let builtin_info =
    {Omake_builtin_type.builtin_empty with builtin_funs = builtin_funs;
      builtin_kfuns = builtin_kfuns
    }
  in
  Omake_builtin.register_builtin builtin_info

