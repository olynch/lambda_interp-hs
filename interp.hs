type Var = String
data Tkn = Sym Var
		   | OpenP
		   | CloseP
		   | Dot
		   | Lambda

lex_helper :: String -> [Tkn] -> [Tkn]
lex_helper s:rest tokens = lex rest NewSym:tokens
	where
	NewSym
		| s == '(' = OpenP
		| s == ')' = CloseP
		| s == 'λ' = Lambda
		| s == '.' = Dot
		| s `elem` ['a'-'z'] = Sym s

lex :: String -> [Tkn]
lex s = reverse $ lex_helper s []

parse :: [Tkn] -> Exp


data Lambda = Var :=> Exp
data Exp = Ref Var
 		 | Lam Lambda
		 | Exp :@ Exp

type Σ = (Exp, Env, Kont)
data D = Clo (Lambda, Env)
type Env = Var -> D
data Kont = Mt
		  | Ar (Exp, Env, Kont)
		  | Fn (Lambda, Env, Kont)

isFinal :: Σ -> Bool

isFinal (Lam _, ρ, Mt) = True
isFinal _ = False

type Program = Exp

instance Show Exp where
	show (Ref a) = a
	show (Lam (a :=> e)) = "(λ" ++ a ++ "." ++ (show e) ++ ")"
	show (e1 :@ e2) = (show e1) ++ (show e2) 

{-instance Show Σ where-}
	{-show (e, ρ, κ) = show e-}

inject :: Program -> Σ
inject (e) = (e, ρ0, Mt)
 where ρ0 :: Env
       ρ0 = \x -> error $ "no binding for " ++ x

step :: Σ -> Σ
step (Ref x, ρ, κ)
   = (Lam lam, ρ', κ) where Clo (lam, ρ') = ρ(x)
{- if you are evaluating a reference, look it up in the environment-}

step (f :@ e, ρ, κ)
   = (f, ρ, Ar(e, ρ, κ))
{- if you are evaluating a function application, evaluate the function-}

step (Lam lam, ρ, Ar(e, ρ', κ))
   = (e, ρ', Fn(lam, ρ, κ))
{-if you have evaluated the function, evaluate the argument term-}

step (Lam lam, ρ, Fn(x :=> e, ρ', κ))
   = (e, ρ' // [x ==> Clo (lam, ρ)], κ)
{- if you have evaluated the argument as well, perform the application-}

(==>) :: a -> b -> (a, b)
(==>) x y = (x, y)

(//) :: Eq a => (a -> b) -> [(a, b)] -> (a -> b)
(//) f [(x, y)] = \x' ->
				  if (x == x')
				  then y
				  else f(x')

terminal :: (Σ -> Σ) -> (Σ -> Bool) -> Σ -> Σ
terminal step isFinal ς0 | isFinal ς0 = ς0
						 | otherwise  = terminal step isFinal (step(ς0))

evaluate pr = terminal step isFinal (inject(pr))
