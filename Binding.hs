module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda 
          | Binding String Lambda deriving (Eq)

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
substituteMacros :: Context -> Lambda -> Either String Lambda
substituteMacros ctx (Var x) = Right $ Var x    -- Lambda is a variable
substituteMacros ctx (App e1 e2) = do   -- Lambda is an application, substitute macros in e1 and e2
    e1' <- substituteMacros ctx e1
    e2' <- substituteMacros ctx e2
    return $ App e1' e2'
substituteMacros ctx (Abs x e) = do -- Lambda is an abstraction, substitute macros in e
    e' <- substituteMacros ctx e
    return $ Abs x e'
substituteMacros ctx (Macro name) = -- Lambda is a macro, lookup macro in context
    case lookup name ctx of
        Just e -> Right e
        Nothing -> Left $ "Undefined macro: " ++ name

simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx ctx step expr = do
    exprWithMacros <- substituteMacros ctx expr -- Substitute macros in expr
    return (simplify step exprWithMacros)   -- Simplify expr

normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
