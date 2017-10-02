import Prelude hiding((<*>))

data BinOper = Plus | Minus | Mult deriving(Show,Eq)
data UnOper = Negate deriving(Show,Eq)

data Term = IntConstant{ intValue :: Int }    
            | Variable{ varName :: String }    
            | BinaryTerm{ lhv :: Term, oper :: BinOper, rhv :: Term } 
            | UnaryTerm{ unoper :: UnOper, rhv :: Term } deriving(Show,Eq)

(<+>), (<->), (<*>) :: Term -> Term -> Term

(<+>) (IntConstant x) (IntConstant y) = IntConstant (x + y)
(<+>) x y = BinaryTerm x Plus y

(<->) (IntConstant x) (IntConstant y) = IntConstant (x - y)
(<->) x y = BinaryTerm x Minus y


(<*>) (IntConstant x) (IntConstant y) = IntConstant (x * y)
(<*>) x y = BinaryTerm x Mult y

infixr 6 <+>, <->
infixr 7 <*>

replaceVar vName term (IntConstant int) = IntConstant int;
replaceVar vName term (Variable var) = if (var == vName) then term else Variable var
replaceVar vName term (UnaryTerm op r) = UnaryTerm op (replaceVar vName term r)
replaceVar vName term (BinaryTerm l op r) = BinaryTerm (replaceVar vName term l) op (replaceVar vName term r)

