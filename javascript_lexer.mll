{
   open Javascript_parser;;
   open Javascript_ast;;
}

let digit = ['0' - '9']
let digit1 = ['1' - '9']
let exponent = ['e' 'E'] ['+' '-']? digit +

(* The type "token" is defined in Javascript_parser.mli *)
rule token = parse
  (* Comments *)
  | "/*" { entry_multilinecomment 0 "" lexbuf }
  | ("//" [^ '\n']*) as x {(let _ = count_newlines x lexbuf in (); token lexbuf)}

  (* Keywords *)
  | '}'         {RCB_KW}
  | '{'         {LCB_KW}
  | '('         {LP_KW}
  | ')'         {RP_KW}
  | ']'         {RSB_KW}
  | '['         {LSB_KW}
  | "function"  {FUNC_KW}
  | "undefined" {UNDEF_KW}
  | "return"    {RET_KW}
  | "let"       {LET_KW}
  | "const"     {CONST_KW}
  | "true"      {TRUE_KW}
  | "false"     {FALSE_KW}
  | "Infinity"  {INFINITY_KW}
  | "NaN"       {NAN_KW}
  | "log"       {LOG_KW}
  | "console"   {CONSOLE_KW}

  (* Operators *)
  | ';'         {SEMICOLON_OP}
  | '!'         {LOG_NOT_OP}
  | "||"        {LOG_OR_OP}
  | "&&"        {LOG_AND_OP}
  | "!=="       {NSTREQ_OP}
  | "==="       {STREQ_OP}
  | '>'         {GT_OP}
  | ">="        {GEQ_OP}
  | '<'         {LT_OP}
  | "<="        {LEQ_OP}
  | '-'         {SUB_OP}
  | '+'         {ADD_OP}
  | '/'         {DIV_OP}
  | '*'         {MUL_OP}
  | '='         {ASSIGN_OP}
  | '?'         {COND_OP}
  | ':'         {COLON_OP}
  | '.'         {DOT_OP}
  | ','         {COMMA_OP}

  (* Strings *)
  | '"'
     ([^ '\\' '"'] (* <- unescaped *)
     (* escape *)
     | '\\' (  ['\\' '\'' '"' 'n' 't' 'b' 'r' '\n']
            | ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
            )
     )*
    '"'
    as x {
           (let _ = count_newlines x lexbuf in STRING(my_unescaped (fst (strip_quotes x))))
         }

  (* TODO: Fix the rule for numbers *)
  (* - decimal: e.g., 1, 10 *)
  (* - binary: e.g., 0b1, 0B10 *)
  (* - octal: e.g., 0o1, 0O17 *)
  (* - hexadecimal: e.g., 0x1, 0Xff, 0x10aAfF *)
  (* - floating point: e.g., 123., 123.456, .123 *)
  (* - scientific notation: e.g., 123e5, 123.E5, 123.456e-5, .123e100 *)
  
  (* Decimal: e.g., 1, 10 *)
  | (digit1 digit*)
  
  (* Binary: e.g., 0b1, 0B10 *)
  (* Not sure if there should be an optional 1 as well in the first character *)
  | (['0'] ['b' 'B'] ['0' '1']*)

  (* Octal: e.g., 0o1, 0O17 *)
  | (['0'] ['o' 'O'] ['0'-'7']*)

  (* Hexadecimal: e.g., 0x1, 0Xff, 0x10aAfF *)
  | (['0'] ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']*)
  
  (* Floating point: e.g., 123., 123.456, .123 *)
  | (digit* ['.'] digit*)

  (* Scientific notation: e.g., 123e5, 123.E5, 123.456e-5, .123e100 *)
  | (digit* ['.']* digit* exponent)

   as x {NUMBER(js_float_of_string x )}


  (* Whitespace *)
  | ['\r' '\n' '\t' ' ']+
    as x {(let _ = count_newlines x lexbuf in (); token lexbuf)}

  (* Identifiers *)
  (* TODO: Fix the rule for identifiers *)
  | (['0'-'9']*['a'-'z']*['A'-'Z']*)*
     as x {IDENT(x)}

  (* End of File *)
  | eof { EOF }
  (* Lexing error *)
  | _ { lex_error "lexing error" lexbuf }

(* Multiline Comments *)
and entry_multilinecomment n x = parse
  | "/*" { entry_multilinecomment (n+1) (x^"/*") lexbuf }
  | "*/" { if (n=0) then ((); token lexbuf) else entry_multilinecomment (n-1) (x^"*/") lexbuf }
  | _ as c { if c='\n' then do_newline lexbuf;
                entry_multilinecomment n (Printf.sprintf "%s%c" x c) lexbuf }
