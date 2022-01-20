{
module LexerIMP where
}

%wrapper "basic"

$white     = [\ \t\n\f\v\r]
$digit     = [0-9]
$graphic   = $printable

@id        = [A-Za-z_][A-Za-z0-9_]*

tokens :-

$white+             ;
$digit+             {\s -> NUM_TOK(read s)}
"+"                 {\_ -> PLUS_TOK       }
"-"                 {\_ -> MINUS_TOK      }
"*"                 {\_ -> MULT_TOK       }
true                {\s -> TRUE_TOK True  }
false               {\s -> FALSE_TOK False}
"="                 {\_ -> EQUAL_TOK      }
"<="                {\_ -> LTOE_TOK       }
"!"                 {\_ -> NOT_TOK        }
"&&"                {\_ -> AND_TOK        }
skip                {\_ -> SKIP_TOK       }
":="                {\_ -> ASSIGN_TOK     }
";"                 {\_ -> SEMICOLON_TOK  }
if                  {\_ -> IF_TOK         }
then                {\_ -> THEN_TOK       }
else                {\_ -> ELSE_TOK       }
while               {\_ -> WHILE_TOK      }
do                  {\_ -> DO_TOK         }
or                  {\_ -> OR_TOK         }
"["                 {\_ -> LBRACK_TOK     }
"]"                 {\_ -> RBRACK_TOK     }
"/"                 {\_ -> SLASH_TOK      }
","                 {\_ -> COMMA_TOK      }
"("                 {\_ -> LPARENT_TOK    }
")"                 {\_ -> RPARENT_TOK    }
"[["                {\_ -> EVALST_TOK     }
"]]"                {\_ -> EVALEND_TOK    }
@id                 {\s -> VAR_TOK s      }



{
data Token
    = NUM_TOK Int
    | TRUE_TOK Bool
    | FALSE_TOK Bool
    | VAR_TOK String
    | PLUS_TOK
    | MINUS_TOK
    | MULT_TOK
    | LBRACK_TOK
    | RBRACK_TOK
    | SLASH_TOK
    | LPARENT_TOK
    | RPARENT_TOK
    | EVALST_TOK
    | EVALEND_TOK
    | EQUAL_TOK
    | LTOE_TOK
    | NOT_TOK
    | AND_TOK
    | OR_TOK
    | WHILE_TOK 
    | DO_TOK
    | SEMICOLON_TOK
    | IF_TOK 
    | THEN_TOK 
    | ELSE_TOK
    | SKIP_TOK
    | COMMA_TOK
    | ASSIGN_TOK 
    deriving(Eq,Show)
}