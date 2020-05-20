module TCType where

import AbsChapel (Intent)

-- Type system
data TCType 
	= TError
	| TVoid
	| TBool
	| TChar
	| TInt
    | TReal
	| TString
	| TPoint TCType                     	-- Pointer to
	| TArr Int TCType                		-- Array Dim Type
	| TFun TCType Intent [TCType, Intent] 	-- Function RetType RetIntent [ArgType, ArgIntent]
	deriving (Eq)


class TCTypeable a where
	tctypeOf :: a -> TCType