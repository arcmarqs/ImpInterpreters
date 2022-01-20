{
module ParserIMP where
import LexerIMP
}

%partial parseState ProgState
%partial parseComs ProgComms
%tokentype { Token }
%error { parseError }


%token

num   { NUM_TOK $$      }
var   { VAR_TOK $$      }
true  { TRUE_TOK $$     }
false { FALSE_TOK $$    }
'+'   { PLUS_TOK        }
'-'   { MINUS_TOK       }
'*'   { MULT_TOK        }
'&&'  { AND_TOK         }
Or    { OR_TOK          }
'='   { EQUAL_TOK       }
'<='  { LTOE_TOK        }
'!'   { NOT_TOK         }
skip  { SKIP_TOK        }
';'   { SEMICOLON_TOK   }
If    { IF_TOK          }
Then  { THEN_TOK        }
Else  { ELSE_TOK        }
While { WHILE_TOK       }
Do    { DO_TOK          }
':='  { ASSIGN_TOK      }
'['   { LBRACK_TOK      }
']'   { RBRACK_TOK      }
'/'   { SLASH_TOK       }
'('   { LPARENT_TOK     } 
')'   { RPARENT_TOK     }
','   { COMMA_TOK       }
'[['  { EVALST_TOK      }
']]'  { EVALEND_TOK     }


%left '<=' '&&' '=' '||' ';' ','
%left '*' '+' '-' 
%right '!'
%%

ProgState : '[' IStates ']' {$2}

ProgComms : '[[' Com ']]' {$2}

IStates : {-empty-}  { [] }
        | IState   {[$1]}
        | IStates ',' IState {$1 ++ [$3]}

IState : num '/' var {($3,$1)} 

Com : skip { Skip }
    | var ':=' AExp {Assign $1 $3}
    | Com ';' Com {Seq $1 $3}
    | If BExp Then Com Else '(' Com ')'  {IfTE $2 $4 $7}
    | While BExp Do '(' Com ')' {While $2 $5}
    | Com Or '(' Com ')'  {Or $1 $4}

BExp : AExp '=' AExp {Equals $1 $3}
     | AExp '<=' AExp {LoE $1 $3}
     | '!' BExp  {NOT $2}
     | BExp '&&' BExp {AND $1 $3}
     | true     {TruthVal True}
     | false    {TruthVal False}

AExp : num { Num $1 }
     | var { Var $1 }
     | AExp '+' AExp {Add $1 $3}
     | AExp '-' AExp {Sub $1 $3}
     | AExp '*' AExp {Mult $1 $3}



{  

data IState = State [(String,Int)]
            deriving Show

data AExp = 
    Num Int
    | Var String 
    | Add AExp AExp
    | Sub AExp AExp
    | Mult AExp AExp
    deriving Show 

data BExp = 
    TruthVal Bool
    | Equals AExp AExp
    | LoE AExp AExp
    | NOT BExp
    | AND BExp BExp
    | OR BExp BExp
    deriving Show

data Com = 
    Assign String AExp
    | Seq Com Com
    | IfTE BExp Com Com
    | While BExp Com
    | Or Com Com
    | Skip
    deriving Show

parseError :: [Token] -> a 
parseError toks = error ("parse error" ++ show toks)
}