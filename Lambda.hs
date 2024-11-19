module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "λ" ++ x ++ "." ++ show e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]  -- List of variable x
vars (App e1 e2) = nub (vars e1 ++ vars e2) -- List of vars in e1 and e2
vars (Abs x e) = nub (x : vars e) -- List of x and vars in e

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]  -- List of variable x
freeVars (App e1 e2) = nub (freeVars e1 ++ freeVars e2) -- List of free vars in e1 and e2
freeVars (Abs x e) = filter (/= x) (freeVars e) -- List of free vars in e except x

-- 1.3.
newVar :: [String] -> String
newVar xs = head (lexicographicStrings \\ xs)
  where
    lexicographicStrings = concatMap stringsOfLength [1..]

    stringsOfLength 0 = [""]
    stringsOfLength n = [c : s | c <- alphabet, s <- stringsOfLength (n - 1)]

    alphabet = ['a'..'z']

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True -- Var is in normal form
isNormalForm (App (Abs _ _) _) = False  -- App where first part is Abs is not in normal form
isNormalForm (App e1 e2) = isNormalForm e1 && isNormalForm e2 -- App is in normal form if both parts are in normal form
isNormalForm (Abs _ e) = isNormalForm e -- Abs is in normal form if e is in normal form

-- 1.5.
reduce :: String -> Lambda -> Lambda -> Lambda
reduce x e1 e2 = substitute e1  -- Substitute apps of x in e1 with e2
  where
    substitute (Var y)
      | y == x = e2 -- If y is x (\x.x e2), substitute y with e2
      | otherwise = Var y -- If y is not x (\x.y e2), keep y
    substitute (App e1' e2') = App (substitute e1') (substitute e2')  -- Substitute apps of x in e1' and e2' with e2 recursively
    substitute (Abs y e)
      | y == x = Abs y e  -- (\x.\x.e e2)
      | y `elem` freeVars e2 = Abs y' (substitute (rename y y' e))  -- (\x.\y.e e2) where y is in freeVars e2
      | otherwise = Abs y (substitute e)  -- (\x.\y.e e2) and y is not in freeVars e2
      where
        y' = newVar (freeVars e ++ freeVars e2)

    rename old new (Var y)
      | y == old = Var new
      | otherwise = Var y
    rename old new (App e1' e2') = App (rename old new e1') (rename old new e2')
    rename old new (Abs y e)
      | y == old = Abs new (rename old new e)
      | otherwise = Abs y (rename old new e)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (App (Abs x e1) e2) = reduce x e1 e2 -- Reduce \x.e1 e2
normalStep (App e1 e2)
  | not (isNormalForm e1) = App (normalStep e1) e2  -- If e1 is not in normal form, reduce e1
  | otherwise             = App e1 (normalStep e2)  -- If e1 is in normal form, reduce e2
normalStep (Abs x e) = Abs x (normalStep e)         -- Reduce the body of the Abs
normalStep e = e  -- No reduction possible - return the expression as is

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (App (Abs x e1) e2) -- \x.e1 e2
  | isNormalForm e2 = reduce x e1 e2  -- If e2 is in normal form, reduce \x.e1 e2
  | otherwise       = App (Abs x e1) (applicativeStep e2) -- If e2 is not in normal form, reduce e2
applicativeStep (App e1 e2)
  | not (isNormalForm e1) = App (applicativeStep e1) e2 -- If e1 is not in normal form, reduce e1
  | otherwise             = App e1 (applicativeStep e2) -- If e1 is in normal form, reduce e2
applicativeStep (Abs x e) = Abs x (applicativeStep e) -- Reduce the body of the Abs
applicativeStep e = e -- No reduction possible - return the expression as is

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify step expr  -- Simplify the expr using step function until it is in normal form
  | isNormalForm expr = [expr]
  | otherwise         = expr : simplify step (step expr)
  
normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
