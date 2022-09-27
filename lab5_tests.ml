open Javascript_parser;;
open Javascript_lexer;;
open Javascript_main;;
open Javascript_ast;;
open Util;;

let lexer_test_fun s =
  tokenize (Lexing.from_string s)


let lexer_tests =
  ("lexer_tests",
   lexer_test_fun,
   (=), (=),
   Some((fun (x : string) -> x), str_token_list),
   [
     (Some("simple subtraction js"),
      "3 - 2",
      Ok([NUMBER(3.0); SUB_OP; NUMBER(2.0); EOF]));

     (Some("simple multiply js"),
      "1 * 2",
      Ok([NUMBER(1.0); MUL_OP; NUMBER(2.0); EOF]));

     (Some("simple divide js"),
      "4 / 2",
      Ok([NUMBER(4.0); DIV_OP; NUMBER(2.0); EOF]));

     (Some("simple add js"),
      "1 + 2",
      Ok([NUMBER(1.0); ADD_OP; NUMBER(2.0); EOF]));

    (Some("medium multiplication js"),
     "3 * 2 * 5",
     Ok([NUMBER(3.0); MUL_OP; NUMBER(2.0); MUL_OP; NUMBER(5.0); EOF]));

    (Some("medium expression mult js"),
     "3 + 2 * 5",
     Ok([NUMBER(3.0); ADD_OP; NUMBER(2.0); MUL_OP; NUMBER(5.0); EOF]));

    (Some("medium expression div js"),
     "3 / 3 * 5",
     Ok([NUMBER(3.0); DIV_OP; NUMBER(3.0); MUL_OP; NUMBER(5.0); EOF]));

    (Some("medium div js"),
     "3 / 3 / 3",
     Ok([NUMBER(3.0); DIV_OP; NUMBER(3.0); DIV_OP; NUMBER(3.0); EOF]));

    (Some("ignoring semi colons js"),
     "3 / 3 * 5;",
     Ok([NUMBER(3.0); DIV_OP; NUMBER(3.0); MUL_OP; NUMBER(5.0); SEMICOLON_OP; EOF]));

    (Some("hard expression (with parenthesis) js"),
     "(3 / 3) * (5 - 2)",
     Ok([LP_KW; NUMBER(3.0); DIV_OP; NUMBER(3.0); RP_KW; MUL_OP; LP_KW; NUMBER(5.0); SUB_OP; NUMBER(2.0); RP_KW; EOF]));

  ]
)


let parser_tests =
  ("parser_tests",
   parse_string,
   eq_program, (=),
   Some((fun (x : string) -> x), str_program),
   [
      (Some("simple expression"),
        "1 + 2",
        Ok(ExprProgram(NoPos, BopExpr( NoPos,
                                       ValExpr(NoPos, NumVal(1.0)),
                                       PlusBop,
                                       ValExpr(NoPos, NumVal(2.0)) ))));

      (Some("Identity lambda"),
        "function (x) {return x;}",
        Ok(ExprProgram(NoPos,
                       FuncExpr(NoPos,
                                (None, (* ident_t option *)
                                 [("x", None)], (* typed_ident_t list *)
                                 ReturnBlock(NoPos, VarExpr(NoPos, "x")), (* block_t *)
                                 None (* typ_t option *)
                                )))));
       (* TODO *)

       (Some("Function Applying value to x"),
        "function (x) {return x + 1;}",
        Ok(ExprProgram(NoPos,
                        FuncExpr(NoPos,
                                  (None,
                                  [("x", None)],
                                  ReturnBlock(NoPos, 
                                              BopExpr(NoPos,
                                                      VarExpr(NoPos, "x"),
                                                      PlusBop,
                                                      ValExpr(NoPos, NumVal(1.0))
                                                      )
                                              ),
                                  None
                                  )
                                )
                        )
            )
        );

       (Some("Two Paramater Function"),
       "function (x, y) {return x - y;}",
       Ok(ExprProgram(NoPos,
                      FuncExpr(NoPos,
                              (None,
                              [("x", None); ("y", None)],
                              ReturnBlock(NoPos,
                                            BopExpr(NoPos,
                                                    VarExpr(NoPos, "x"),
                                                    MinusBop,
                                                    VarExpr(NoPos, "y")
                                                    )
                                          ),
                              None
                              )
                            )
                        )
            )
        );

        (Some("PEMDAS Rules"),
        "1 + 2 / 4",
        Ok(ExprProgram(NoPos,
                        BopExpr(NoPos,
                                ValExpr(NoPos, NumVal(1.0)),
                                PlusBop,
                                BopExpr(NoPos, 
                                        ValExpr(NoPos, NumVal(2.0)),
                                        DivBop,
                                        ValExpr(NoPos, NumVal(4.0)))))));

        (Some("Return boolean"),
        "function (x) {return x === 1;}",
        Ok(ExprProgram(NoPos,
                        FuncExpr(NoPos,
                                  (None,
                                  [("x", None)],
                                  ReturnBlock(NoPos, 
                                                    BopExpr(NoPos,
                                                                  VarExpr(NoPos, "x"),
                                                                  EqBop,
                                                                  ValExpr(NoPos, NumVal(1.0)))
                                    
                                  ),
                                  None
                                  )))));
        
        (Some("Return boolean 2 param"),
        "function (x, y) {return x >= y;}",
        Ok(ExprProgram(NoPos,
                        FuncExpr(NoPos,
                                  (None,
                                  [("x", None); ("y", None)],
                                  ReturnBlock(NoPos, 
                                                    BopExpr(NoPos,
                                                                  VarExpr(NoPos, "x"),
                                                                  GteBop,
                                                                  VarExpr(NoPos, "y"))
                                    
                                  ),
                                  None
                                  )))));

        (Some("Return PEMDAS"),
        "function (x, y) {return x + y / 4;}",
        Ok(ExprProgram(NoPos,
                        FuncExpr(NoPos,
                                  (None,
                                  [("x", None); ("y", None)],
                                  ReturnBlock(NoPos, 
                                                    BopExpr(NoPos,
                                                                  VarExpr(NoPos, "x"),
                                                                  PlusBop,
                                                                  BopExpr(NoPos, 
                                                                              VarExpr(NoPos, "y"),
                                                                              DivBop,
                                                                              ValExpr(NoPos, NumVal(4.0)))
                                    
                                  )),
                                  None
                                  )))));

      (Some ("Multiplication"),
      "4 * 3 + 1",
      Ok(ExprProgram(NoPos,
                          BopExpr(NoPos,
                                  BopExpr(NoPos, ValExpr(NoPos, NumVal(4.0)), TimesBop, ValExpr(NoPos, NumVal(3.0))),
                                  PlusBop,
                                  ValExpr(NoPos, NumVal(1.0))))));
  ])
