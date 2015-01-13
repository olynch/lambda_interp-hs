module Interp where
import Parser

type Σ = (Exp, Env, Kont)
data D = Clo (Lambda, Env)
type Env = Var -> D
data Kont = Mt
		  | Ar (Exp,    Env, Kont)
		  | Fn (Lambda, Env, Kont)

isFinal :: Σ -> Bool

type Program = Exp

inject :: Program -> Σ
inject (e) = (e, ρ0, Mt)
 where ρ0 :: Env
       ρ0 x = error $ "no binding for " ++ x

step :: Σ -> Σ
step (Ref x, ρ, κ)
   = (Lam lam, ρ', κ) where Clo (lam, ρ') = ρ x
{- if you are evaluating a reference, look it up in the environment-}

step (f :@ e, ρ, κ)
   = (f, ρ, Ar(e, ρ, κ))
{- if you are evaluating a function application, evaluate the function-}

step (Lam lam, ρ, Ar(e, ρ', κ))
   = (e, ρ', Fn(lam, ρ, κ))
{-if you have evaluated the function, evaluate the argument term-}

step (Lam lam, ρ, Fn(x :=> e, ρ', κ))
   = (e, ρ' // (x, Clo (lam, ρ)), κ)
{- if you have evaluated the argument as well, perform the application-}

step (Lam lam, ρ, Mt) = (Lam lam, ρ, Mt)
{- this is the terminal state, nothing changes -}

(//) :: Eq a => (a -> b) -> (a, b) -> a -> b
(f // (x, y)) x' = if x == x'
				   then y
				   else f x'

isFinal (Lam _, _, Mt) = True
isFinal _ = False

terminal :: (Σ -> Σ) -> (Σ -> Bool) -> Σ -> Σ
terminal stepF isFinalF ς0 | isFinal ς0 = ς0
                           | otherwise  = terminal stepF isFinalF (step ς0)

evaluate :: Program -> Σ
evaluate pr = terminal step isFinal (inject pr)

evalStr :: String -> Σ
evalStr = evaluate . Parser.parse . Parser.lexer
