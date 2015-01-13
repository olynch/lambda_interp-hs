{
module Parser where
import Data.Char
}

%name parse
%tokentype { Token }
%error { parseError }

%token
	lam { TokenLam }
	var { TokenVar $$ }
	'(' { TokenOP }
	')' { TokenCP }
	'.' { TokenDot }

%%

Exp  : '(' lam var '.' Exp ')'   { Lam ($3 :=> $5) }
	 | '(' Exp Exp ')'			 { $2 :@ $3 }
	 | Exp Exp 					 { $1 :@ $2 }
	 | var 						 { Ref $1 }

{
parseError :: [Token] -> a
parseError (t:ts) = error $ "Parse error from my parser on token: " ++ (show t)

type Var = String
data Lambda = Var :=> Exp

data Exp = Ref Var
		 | Lam Lambda
		 | Exp :@ Exp

instance Show Exp where
	show (Ref a) = a
	show (Lam (a :=> e)) = "(λ" ++ a ++ "." ++ (show e) ++ ")"
	show (e1 :@ e2) = (show e1) ++ (show e2) 

data Token
	= TokenLam
	| TokenDot
	| TokenOP
	| TokenCP
	| TokenVar String
	deriving Show

lexer :: String -> [Token]
lexer [] = []
lexer (c:cs)
	| isSpace c = lexer cs
	| isAlpha c = TokenVar [c] : lexer cs
lexer ('$':cs) = TokenLam : lexer cs
lexer ('.':cs) = TokenDot : lexer cs
lexer ('(':cs) = TokenOP : lexer cs
lexer (')':cs) = TokenCP : lexer cs

main = getContents >>= print . parse . lexer 
}
