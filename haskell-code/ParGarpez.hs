{-# OPTIONS_GHC -w #-}
{-# OPTIONS -XMagicHash -XBangPatterns -XTypeSynonymInstances -XFlexibleInstances -cpp #-}
#if __GLASGOW_HASKELL__ >= 710
{-# OPTIONS_GHC -XPartialTypeSignatures #-}
#endif
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParGarpez where
import AbsGarpez
import LexGarpez
import ErrM
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import qualified GHC.Exts as Happy_GHC_Exts
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

newtype HappyAbsSyn  = HappyAbsSyn HappyAny
#if __GLASGOW_HASKELL__ >= 607
type HappyAny = Happy_GHC_Exts.Any
#else
type HappyAny = forall a . a
#endif
newtype HappyWrap4 = HappyWrap4 (Id)
happyIn4 :: (Id) -> (HappyAbsSyn )
happyIn4 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap4 x)
{-# INLINE happyIn4 #-}
happyOut4 :: (HappyAbsSyn ) -> HappyWrap4
happyOut4 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut4 #-}
newtype HappyWrap5 = HappyWrap5 (PBool)
happyIn5 :: (PBool) -> (HappyAbsSyn )
happyIn5 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap5 x)
{-# INLINE happyIn5 #-}
happyOut5 :: (HappyAbsSyn ) -> HappyWrap5
happyOut5 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut5 #-}
newtype HappyWrap6 = HappyWrap6 (PChar)
happyIn6 :: (PChar) -> (HappyAbsSyn )
happyIn6 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap6 x)
{-# INLINE happyIn6 #-}
happyOut6 :: (HappyAbsSyn ) -> HappyWrap6
happyOut6 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut6 #-}
newtype HappyWrap7 = HappyWrap7 (PInt)
happyIn7 :: (PInt) -> (HappyAbsSyn )
happyIn7 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap7 x)
{-# INLINE happyIn7 #-}
happyOut7 :: (HappyAbsSyn ) -> HappyWrap7
happyOut7 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut7 #-}
newtype HappyWrap8 = HappyWrap8 (PFloat)
happyIn8 :: (PFloat) -> (HappyAbsSyn )
happyIn8 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap8 x)
{-# INLINE happyIn8 #-}
happyOut8 :: (HappyAbsSyn ) -> HappyWrap8
happyOut8 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut8 #-}
newtype HappyWrap9 = HappyWrap9 (PString)
happyIn9 :: (PString) -> (HappyAbsSyn )
happyIn9 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap9 x)
{-# INLINE happyIn9 #-}
happyOut9 :: (HappyAbsSyn ) -> HappyWrap9
happyOut9 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut9 #-}
newtype HappyWrap10 = HappyWrap10 (Return)
happyIn10 :: (Return) -> (HappyAbsSyn )
happyIn10 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap10 x)
{-# INLINE happyIn10 #-}
happyOut10 :: (HappyAbsSyn ) -> HappyWrap10
happyOut10 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut10 #-}
newtype HappyWrap11 = HappyWrap11 (Break)
happyIn11 :: (Break) -> (HappyAbsSyn )
happyIn11 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap11 x)
{-# INLINE happyIn11 #-}
happyOut11 :: (HappyAbsSyn ) -> HappyWrap11
happyOut11 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut11 #-}
newtype HappyWrap12 = HappyWrap12 (Continue)
happyIn12 :: (Continue) -> (HappyAbsSyn )
happyIn12 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap12 x)
{-# INLINE happyIn12 #-}
happyOut12 :: (HappyAbsSyn ) -> HappyWrap12
happyOut12 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut12 #-}
newtype HappyWrap13 = HappyWrap13 (RChar)
happyIn13 :: (RChar) -> (HappyAbsSyn )
happyIn13 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap13 x)
{-# INLINE happyIn13 #-}
happyOut13 :: (HappyAbsSyn ) -> HappyWrap13
happyOut13 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut13 #-}
newtype HappyWrap14 = HappyWrap14 (RInt)
happyIn14 :: (RInt) -> (HappyAbsSyn )
happyIn14 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap14 x)
{-# INLINE happyIn14 #-}
happyOut14 :: (HappyAbsSyn ) -> HappyWrap14
happyOut14 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut14 #-}
newtype HappyWrap15 = HappyWrap15 (RFloat)
happyIn15 :: (RFloat) -> (HappyAbsSyn )
happyIn15 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap15 x)
{-# INLINE happyIn15 #-}
happyOut15 :: (HappyAbsSyn ) -> HappyWrap15
happyOut15 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut15 #-}
newtype HappyWrap16 = HappyWrap16 (RString)
happyIn16 :: (RString) -> (HappyAbsSyn )
happyIn16 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap16 x)
{-# INLINE happyIn16 #-}
happyOut16 :: (HappyAbsSyn ) -> HappyWrap16
happyOut16 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut16 #-}
newtype HappyWrap17 = HappyWrap17 (WChar)
happyIn17 :: (WChar) -> (HappyAbsSyn )
happyIn17 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap17 x)
{-# INLINE happyIn17 #-}
happyOut17 :: (HappyAbsSyn ) -> HappyWrap17
happyOut17 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut17 #-}
newtype HappyWrap18 = HappyWrap18 (WInt)
happyIn18 :: (WInt) -> (HappyAbsSyn )
happyIn18 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap18 x)
{-# INLINE happyIn18 #-}
happyOut18 :: (HappyAbsSyn ) -> HappyWrap18
happyOut18 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut18 #-}
newtype HappyWrap19 = HappyWrap19 (WFloat)
happyIn19 :: (WFloat) -> (HappyAbsSyn )
happyIn19 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap19 x)
{-# INLINE happyIn19 #-}
happyOut19 :: (HappyAbsSyn ) -> HappyWrap19
happyOut19 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut19 #-}
newtype HappyWrap20 = HappyWrap20 (WString)
happyIn20 :: (WString) -> (HappyAbsSyn )
happyIn20 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap20 x)
{-# INLINE happyIn20 #-}
happyOut20 :: (HappyAbsSyn ) -> HappyWrap20
happyOut20 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut20 #-}
newtype HappyWrap21 = HappyWrap21 (Program)
happyIn21 :: (Program) -> (HappyAbsSyn )
happyIn21 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap21 x)
{-# INLINE happyIn21 #-}
happyOut21 :: (HappyAbsSyn ) -> HappyWrap21
happyOut21 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut21 #-}
newtype HappyWrap22 = HappyWrap22 (Global)
happyIn22 :: (Global) -> (HappyAbsSyn )
happyIn22 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap22 x)
{-# INLINE happyIn22 #-}
happyOut22 :: (HappyAbsSyn ) -> HappyWrap22
happyOut22 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut22 #-}
newtype HappyWrap23 = HappyWrap23 ([Global])
happyIn23 :: ([Global]) -> (HappyAbsSyn )
happyIn23 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap23 x)
{-# INLINE happyIn23 #-}
happyOut23 :: (HappyAbsSyn ) -> HappyWrap23
happyOut23 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut23 #-}
newtype HappyWrap24 = HappyWrap24 (Declaration)
happyIn24 :: (Declaration) -> (HappyAbsSyn )
happyIn24 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap24 x)
{-# INLINE happyIn24 #-}
happyOut24 :: (HappyAbsSyn ) -> HappyWrap24
happyOut24 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut24 #-}
newtype HappyWrap25 = HappyWrap25 (InitItem)
happyIn25 :: (InitItem) -> (HappyAbsSyn )
happyIn25 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap25 x)
{-# INLINE happyIn25 #-}
happyOut25 :: (HappyAbsSyn ) -> HappyWrap25
happyOut25 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut25 #-}
newtype HappyWrap26 = HappyWrap26 (DeclId)
happyIn26 :: (DeclId) -> (HappyAbsSyn )
happyIn26 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap26 x)
{-# INLINE happyIn26 #-}
happyOut26 :: (HappyAbsSyn ) -> HappyWrap26
happyOut26 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut26 #-}
newtype HappyWrap27 = HappyWrap27 (DeclItem)
happyIn27 :: (DeclItem) -> (HappyAbsSyn )
happyIn27 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap27 x)
{-# INLINE happyIn27 #-}
happyOut27 :: (HappyAbsSyn ) -> HappyWrap27
happyOut27 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut27 #-}
newtype HappyWrap28 = HappyWrap28 ([DeclItem])
happyIn28 :: ([DeclItem]) -> (HappyAbsSyn )
happyIn28 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap28 x)
{-# INLINE happyIn28 #-}
happyOut28 :: (HappyAbsSyn ) -> HappyWrap28
happyOut28 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut28 #-}
newtype HappyWrap29 = HappyWrap29 ([InitItem])
happyIn29 :: ([InitItem]) -> (HappyAbsSyn )
happyIn29 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap29 x)
{-# INLINE happyIn29 #-}
happyOut29 :: (HappyAbsSyn ) -> HappyWrap29
happyOut29 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut29 #-}
newtype HappyWrap30 = HappyWrap30 (Function)
happyIn30 :: (Function) -> (HappyAbsSyn )
happyIn30 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap30 x)
{-# INLINE happyIn30 #-}
happyOut30 :: (HappyAbsSyn ) -> HappyWrap30
happyOut30 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut30 #-}
newtype HappyWrap31 = HappyWrap31 (FunRest)
happyIn31 :: (FunRest) -> (HappyAbsSyn )
happyIn31 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap31 x)
{-# INLINE happyIn31 #-}
happyOut31 :: (HappyAbsSyn ) -> HappyWrap31
happyOut31 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut31 #-}
newtype HappyWrap32 = HappyWrap32 (FormalParam)
happyIn32 :: (FormalParam) -> (HappyAbsSyn )
happyIn32 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap32 x)
{-# INLINE happyIn32 #-}
happyOut32 :: (HappyAbsSyn ) -> HappyWrap32
happyOut32 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut32 #-}
newtype HappyWrap33 = HappyWrap33 (PassBy)
happyIn33 :: (PassBy) -> (HappyAbsSyn )
happyIn33 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap33 x)
{-# INLINE happyIn33 #-}
happyOut33 :: (HappyAbsSyn ) -> HappyWrap33
happyOut33 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut33 #-}
newtype HappyWrap34 = HappyWrap34 ([FormalParam])
happyIn34 :: ([FormalParam]) -> (HappyAbsSyn )
happyIn34 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap34 x)
{-# INLINE happyIn34 #-}
happyOut34 :: (HappyAbsSyn ) -> HappyWrap34
happyOut34 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut34 #-}
newtype HappyWrap35 = HappyWrap35 (Block)
happyIn35 :: (Block) -> (HappyAbsSyn )
happyIn35 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap35 x)
{-# INLINE happyIn35 #-}
happyOut35 :: (HappyAbsSyn ) -> HappyWrap35
happyOut35 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut35 #-}
newtype HappyWrap36 = HappyWrap36 ([Declaration])
happyIn36 :: ([Declaration]) -> (HappyAbsSyn )
happyIn36 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap36 x)
{-# INLINE happyIn36 #-}
happyOut36 :: (HappyAbsSyn ) -> HappyWrap36
happyOut36 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut36 #-}
newtype HappyWrap37 = HappyWrap37 ([Statement])
happyIn37 :: ([Statement]) -> (HappyAbsSyn )
happyIn37 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap37 x)
{-# INLINE happyIn37 #-}
happyOut37 :: (HappyAbsSyn ) -> HappyWrap37
happyOut37 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut37 #-}
newtype HappyWrap38 = HappyWrap38 (Statement)
happyIn38 :: (Statement) -> (HappyAbsSyn )
happyIn38 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap38 x)
{-# INLINE happyIn38 #-}
happyOut38 :: (HappyAbsSyn ) -> HappyWrap38
happyOut38 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut38 #-}
newtype HappyWrap39 = HappyWrap39 (Conditional)
happyIn39 :: (Conditional) -> (HappyAbsSyn )
happyIn39 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap39 x)
{-# INLINE happyIn39 #-}
happyOut39 :: (HappyAbsSyn ) -> HappyWrap39
happyOut39 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut39 #-}
newtype HappyWrap40 = HappyWrap40 (Loop)
happyIn40 :: (Loop) -> (HappyAbsSyn )
happyIn40 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap40 x)
{-# INLINE happyIn40 #-}
happyOut40 :: (HappyAbsSyn ) -> HappyWrap40
happyOut40 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut40 #-}
newtype HappyWrap41 = HappyWrap41 (Jump)
happyIn41 :: (Jump) -> (HappyAbsSyn )
happyIn41 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap41 x)
{-# INLINE happyIn41 #-}
happyOut41 :: (HappyAbsSyn ) -> HappyWrap41
happyOut41 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut41 #-}
newtype HappyWrap42 = HappyWrap42 (WPredefined)
happyIn42 :: (WPredefined) -> (HappyAbsSyn )
happyIn42 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap42 x)
{-# INLINE happyIn42 #-}
happyOut42 :: (HappyAbsSyn ) -> HappyWrap42
happyOut42 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut42 #-}
newtype HappyWrap43 = HappyWrap43 (If)
happyIn43 :: (If) -> (HappyAbsSyn )
happyIn43 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap43 x)
{-# INLINE happyIn43 #-}
happyOut43 :: (HappyAbsSyn ) -> HappyWrap43
happyOut43 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut43 #-}
newtype HappyWrap44 = HappyWrap44 (RestIf)
happyIn44 :: (RestIf) -> (HappyAbsSyn )
happyIn44 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap44 x)
{-# INLINE happyIn44 #-}
happyOut44 :: (HappyAbsSyn ) -> HappyWrap44
happyOut44 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut44 #-}
newtype HappyWrap45 = HappyWrap45 (While)
happyIn45 :: (While) -> (HappyAbsSyn )
happyIn45 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap45 x)
{-# INLINE happyIn45 #-}
happyOut45 :: (HappyAbsSyn ) -> HappyWrap45
happyOut45 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut45 #-}
newtype HappyWrap46 = HappyWrap46 (DoWhile)
happyIn46 :: (DoWhile) -> (HappyAbsSyn )
happyIn46 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap46 x)
{-# INLINE happyIn46 #-}
happyOut46 :: (HappyAbsSyn ) -> HappyWrap46
happyOut46 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut46 #-}
newtype HappyWrap47 = HappyWrap47 (For)
happyIn47 :: (For) -> (HappyAbsSyn )
happyIn47 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap47 x)
{-# INLINE happyIn47 #-}
happyOut47 :: (HappyAbsSyn ) -> HappyWrap47
happyOut47 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut47 #-}
newtype HappyWrap48 = HappyWrap48 (LExp)
happyIn48 :: (LExp) -> (HappyAbsSyn )
happyIn48 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap48 x)
{-# INLINE happyIn48 #-}
happyOut48 :: (HappyAbsSyn ) -> HappyWrap48
happyOut48 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut48 #-}
newtype HappyWrap49 = HappyWrap49 (LExp)
happyIn49 :: (LExp) -> (HappyAbsSyn )
happyIn49 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap49 x)
{-# INLINE happyIn49 #-}
happyOut49 :: (HappyAbsSyn ) -> HappyWrap49
happyOut49 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut49 #-}
newtype HappyWrap50 = HappyWrap50 (LExp)
happyIn50 :: (LExp) -> (HappyAbsSyn )
happyIn50 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap50 x)
{-# INLINE happyIn50 #-}
happyOut50 :: (HappyAbsSyn ) -> HappyWrap50
happyOut50 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut50 #-}
newtype HappyWrap51 = HappyWrap51 (LExp)
happyIn51 :: (LExp) -> (HappyAbsSyn )
happyIn51 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap51 x)
{-# INLINE happyIn51 #-}
happyOut51 :: (HappyAbsSyn ) -> HappyWrap51
happyOut51 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut51 #-}
newtype HappyWrap52 = HappyWrap52 (LExp)
happyIn52 :: (LExp) -> (HappyAbsSyn )
happyIn52 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap52 x)
{-# INLINE happyIn52 #-}
happyOut52 :: (HappyAbsSyn ) -> HappyWrap52
happyOut52 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut52 #-}
newtype HappyWrap53 = HappyWrap53 (RExp)
happyIn53 :: (RExp) -> (HappyAbsSyn )
happyIn53 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap53 x)
{-# INLINE happyIn53 #-}
happyOut53 :: (HappyAbsSyn ) -> HappyWrap53
happyOut53 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut53 #-}
newtype HappyWrap54 = HappyWrap54 (RExp)
happyIn54 :: (RExp) -> (HappyAbsSyn )
happyIn54 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap54 x)
{-# INLINE happyIn54 #-}
happyOut54 :: (HappyAbsSyn ) -> HappyWrap54
happyOut54 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut54 #-}
newtype HappyWrap55 = HappyWrap55 (RExp)
happyIn55 :: (RExp) -> (HappyAbsSyn )
happyIn55 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap55 x)
{-# INLINE happyIn55 #-}
happyOut55 :: (HappyAbsSyn ) -> HappyWrap55
happyOut55 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut55 #-}
newtype HappyWrap56 = HappyWrap56 (RExp)
happyIn56 :: (RExp) -> (HappyAbsSyn )
happyIn56 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap56 x)
{-# INLINE happyIn56 #-}
happyOut56 :: (HappyAbsSyn ) -> HappyWrap56
happyOut56 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut56 #-}
newtype HappyWrap57 = HappyWrap57 (RExp)
happyIn57 :: (RExp) -> (HappyAbsSyn )
happyIn57 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap57 x)
{-# INLINE happyIn57 #-}
happyOut57 :: (HappyAbsSyn ) -> HappyWrap57
happyOut57 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut57 #-}
newtype HappyWrap58 = HappyWrap58 (RExp)
happyIn58 :: (RExp) -> (HappyAbsSyn )
happyIn58 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap58 x)
{-# INLINE happyIn58 #-}
happyOut58 :: (HappyAbsSyn ) -> HappyWrap58
happyOut58 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut58 #-}
newtype HappyWrap59 = HappyWrap59 (RExp)
happyIn59 :: (RExp) -> (HappyAbsSyn )
happyIn59 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap59 x)
{-# INLINE happyIn59 #-}
happyOut59 :: (HappyAbsSyn ) -> HappyWrap59
happyOut59 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut59 #-}
newtype HappyWrap60 = HappyWrap60 (RExp)
happyIn60 :: (RExp) -> (HappyAbsSyn )
happyIn60 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap60 x)
{-# INLINE happyIn60 #-}
happyOut60 :: (HappyAbsSyn ) -> HappyWrap60
happyOut60 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut60 #-}
newtype HappyWrap61 = HappyWrap61 (RExp)
happyIn61 :: (RExp) -> (HappyAbsSyn )
happyIn61 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap61 x)
{-# INLINE happyIn61 #-}
happyOut61 :: (HappyAbsSyn ) -> HappyWrap61
happyOut61 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut61 #-}
newtype HappyWrap62 = HappyWrap62 (RExp)
happyIn62 :: (RExp) -> (HappyAbsSyn )
happyIn62 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap62 x)
{-# INLINE happyIn62 #-}
happyOut62 :: (HappyAbsSyn ) -> HappyWrap62
happyOut62 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut62 #-}
newtype HappyWrap63 = HappyWrap63 (RExp)
happyIn63 :: (RExp) -> (HappyAbsSyn )
happyIn63 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap63 x)
{-# INLINE happyIn63 #-}
happyOut63 :: (HappyAbsSyn ) -> HappyWrap63
happyOut63 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut63 #-}
newtype HappyWrap64 = HappyWrap64 (Literal)
happyIn64 :: (Literal) -> (HappyAbsSyn )
happyIn64 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap64 x)
{-# INLINE happyIn64 #-}
happyOut64 :: (HappyAbsSyn ) -> HappyWrap64
happyOut64 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut64 #-}
newtype HappyWrap65 = HappyWrap65 (RPredefined)
happyIn65 :: (RPredefined) -> (HappyAbsSyn )
happyIn65 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap65 x)
{-# INLINE happyIn65 #-}
happyOut65 :: (HappyAbsSyn ) -> HappyWrap65
happyOut65 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut65 #-}
newtype HappyWrap66 = HappyWrap66 ([RExp])
happyIn66 :: ([RExp]) -> (HappyAbsSyn )
happyIn66 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap66 x)
{-# INLINE happyIn66 #-}
happyOut66 :: (HappyAbsSyn ) -> HappyWrap66
happyOut66 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut66 #-}
newtype HappyWrap67 = HappyWrap67 (Type)
happyIn67 :: (Type) -> (HappyAbsSyn )
happyIn67 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap67 x)
{-# INLINE happyIn67 #-}
happyOut67 :: (HappyAbsSyn ) -> HappyWrap67
happyOut67 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut67 #-}
newtype HappyWrap68 = HappyWrap68 (RetType)
happyIn68 :: (RetType) -> (HappyAbsSyn )
happyIn68 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap68 x)
{-# INLINE happyIn68 #-}
happyOut68 :: (HappyAbsSyn ) -> HappyWrap68
happyOut68 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut68 #-}
newtype HappyWrap69 = HappyWrap69 (SimpleType)
happyIn69 :: (SimpleType) -> (HappyAbsSyn )
happyIn69 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap69 x)
{-# INLINE happyIn69 #-}
happyOut69 :: (HappyAbsSyn ) -> HappyWrap69
happyOut69 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut69 #-}
newtype HappyWrap70 = HappyWrap70 (AssignmentOp)
happyIn70 :: (AssignmentOp) -> (HappyAbsSyn )
happyIn70 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap70 x)
{-# INLINE happyIn70 #-}
happyOut70 :: (HappyAbsSyn ) -> HappyWrap70
happyOut70 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut70 #-}
newtype HappyWrap71 = HappyWrap71 (ComparisonOp)
happyIn71 :: (ComparisonOp) -> (HappyAbsSyn )
happyIn71 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap71 x)
{-# INLINE happyIn71 #-}
happyOut71 :: (HappyAbsSyn ) -> HappyWrap71
happyOut71 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut71 #-}
newtype HappyWrap72 = HappyWrap72 (IncDecOp)
happyIn72 :: (IncDecOp) -> (HappyAbsSyn )
happyIn72 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap72 x)
{-# INLINE happyIn72 #-}
happyOut72 :: (HappyAbsSyn ) -> HappyWrap72
happyOut72 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut72 #-}
newtype HappyWrap73 = HappyWrap73 (SignOp)
happyIn73 :: (SignOp) -> (HappyAbsSyn )
happyIn73 x = Happy_GHC_Exts.unsafeCoerce# (HappyWrap73 x)
{-# INLINE happyIn73 #-}
happyOut73 :: (HappyAbsSyn ) -> HappyWrap73
happyOut73 x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOut73 #-}
happyInTok :: (Token) -> (HappyAbsSyn )
happyInTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyInTok #-}
happyOutTok :: (HappyAbsSyn ) -> (Token)
happyOutTok x = Happy_GHC_Exts.unsafeCoerce# x
{-# INLINE happyOutTok #-}


happyExpList :: HappyAddr
happyExpList = HappyA# "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa7\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x23\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\xc0\x0e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x02\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x25\x02\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x21\x02\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x22\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\xec\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x30\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x01\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x04\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x80\x40\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x27\x1a\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x50\x22\x00\x80\x14\x66\x70\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x90\x48\x34\x81\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x70\xa2\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x52\x33\x00\x00\x00\xc0\x8f\x07\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x10\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x22\x35\x03\x00\x00\x00\xfc\x78\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x20\x00\x00\x00\x00\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x40\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","Id","PBool","PChar","PInt","PFloat","PString","Return","Break","Continue","RChar","RInt","RFloat","RString","WChar","WInt","WFloat","WString","Program","Global","ListGlobal","Declaration","InitItem","DeclId","DeclItem","ListDeclItem","ListInitItem","Function","FunRest","FormalParam","PassBy","ListFormalParam","Block","ListDeclaration","ListStatement","Statement","Conditional","Loop","Jump","WPredefined","If","RestIf","While","DoWhile","For","LExp","LExp1","LExp2","LExp3","LExp4","RExp","RExp1","RExp2","RExp3","RExp4","RExp5","RExp6","RExp7","RExp8","RExp9","RExp10","Literal","RPredefined","ListRExp","Type","RetType","SimpleType","AssignmentOp","ComparisonOp","IncDecOp","SignOp","'!'","'!='","'%'","'%='","'&'","'&&'","'&='","'('","')'","'*'","'*='","'+'","'++'","'+='","','","'-'","'--'","'-='","'/'","'/='","';'","'<'","'<='","'='","'=='","'>'","'>='","'['","']'","'^'","'^='","'bool'","'char'","'const'","'do'","'else'","'float'","'for'","'function'","'if'","'int'","'ref'","'string'","'void'","'while'","'{'","'|='","'||'","'}'","L_Id","L_PBool","L_PChar","L_PInt","L_PFloat","L_PString","L_Return","L_Break","L_Continue","L_RChar","L_RInt","L_RFloat","L_RString","L_WChar","L_WInt","L_WFloat","L_WString","%eof"]
        bit_start = st * 140
        bit_end = (st + 1) * 140
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..139]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

happyActOffsets :: HappyAddr
happyActOffsets = HappyA# "\x00\x00\xd7\xff\x00\x00\xd2\xff\xae\x01\x00\x00\x00\x00\x00\x00\x70\x00\x00\x00\x00\x00\x00\x00\xef\xff\x00\x00\xac\x04\x00\x00\x00\x00\x00\x00\x06\x00\xef\xff\x2a\x00\x13\x00\x34\x00\x48\x00\x50\x00\x00\x00\x00\x00\x5a\x00\x60\x00\x00\x00\x3a\x00\x5b\x00\x00\x00\x00\x00\x47\x00\x52\x04\x00\x00\x47\x00\x82\x00\x00\x00\x98\x00\xc1\x02\x00\x00\xdb\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\xdf\x00\xcc\x00\x13\x01\x01\x01\x00\x00\xe9\x01\x07\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1a\x01\x10\x00\xa9\x04\x8d\x04\x79\x03\x52\x04\xc9\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x79\x03\x20\x01\x51\x00\x00\x00\xe9\x01\x00\x00\x00\x00\x00\x00\x28\x01\x8d\x04\x8d\x04\x8d\x04\x8d\x04\x8d\x04\x8d\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x04\x52\x04\x52\x04\x52\x04\x00\x00\x52\x04\x24\x01\xac\x04\x29\x01\x00\x00\x0b\x01\x70\x00\xc1\x02\xf8\xff\x47\x01\xfc\xff\x4b\x01\x34\x01\x00\x00\x07\x00\x64\x00\x64\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6d\x01\x00\x00\x00\x00\x52\x04\x00\x00\x00\x00\x00\x00\x00\x00\x11\x03\x00\x00\x00\x00\x6e\x04\x4c\x01\x52\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x5c\x01\x6f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x5d\x04\x4a\x01\x71\x01\x75\x01\x76\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x04\x52\x04\x11\x03\x52\x01\x52\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x52\x04\x00\x00\x50\x01\x52\x04\x78\x01\x52\x00\xf3\xff\x81\x01\x7f\x01\x9b\x00\xc8\x00\x5d\x01\x5d\x01\x52\x04\x52\x04\x00\x00\x93\x01\x95\x01\x00\x00\x00\x00\xc9\x00\xf9\xff\x66\x01\x00\x00\x00\x00\xec\xff\x52\x04\x96\x01\x00\x00\xf1\x00\x00\x00\x00\x00\x7e\x01\x00\x00\x00\x00"#

happyGotoOffsets :: HappyAddr
happyGotoOffsets = HappyA# "\x0d\x01\x00\x00\x00\x00\x00\x00\x68\x01\x00\x00\x00\x00\x00\x00\x7b\x01\x00\x00\x00\x00\x00\x00\x23\x01\x00\x00\x87\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc6\x01\x00\x00\x00\x00\x00\x00\x2b\x02\x7b\x00\x00\x00\x3a\x01\x00\x00\x00\x00\x00\x00\xbd\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8d\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x11\x00\x15\x04\xd4\x02\x44\x00\xb9\x00\xf7\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa0\x00\x00\x00\x00\x00\x00\x00\x8f\x01\x00\x00\x00\x00\x00\x00\x00\x00\x74\x03\xb2\x03\xc4\x03\x24\x03\x62\x03\x12\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x02\x84\x02\x71\x02\xd3\x00\x00\x00\x01\x00\x00\x00\xe9\x00\x00\x00\x00\x00\xb5\x01\xd6\x01\xc4\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x1b\x00\x00\x00\x00\x00\x00\x00\xb8\x01\x62\x00\x00\x00\x00\x00\x04\x04\x00\x00\x11\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9f\x01\xc3\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x01\x69\x01\x4a\x00\x00\x00\x83\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc1\x01\x00\x00\x00\x00\x61\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc4\x01\xc9\x01\xdb\x01\x19\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc2\x01\x00\x00\x00\x00\xa2\x00\x33\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xca\x01\x00\x00\x00\x00"#

happyAdjustOffset :: Happy_GHC_Exts.Int# -> Happy_GHC_Exts.Int#
happyAdjustOffset off = off

happyDefActions :: HappyAddr
happyDefActions = HappyA# "\xea\xff\x00\x00\xfe\xff\x00\x00\xed\xff\xe9\xff\xec\xff\xeb\xff\x00\x00\x85\xff\x80\xff\x7f\xff\x00\x00\x7d\xff\x00\x00\x7e\xff\x7c\xff\x7b\xff\x00\x00\x00\x00\x85\xff\x00\x00\xe0\xff\x00\x00\xe5\xff\xe3\xff\xe4\xff\xe2\xff\x00\x00\x83\xff\x00\x00\x00\x00\xfb\xff\xe7\xff\x00\x00\x00\x00\xe8\xff\x00\x00\x00\x00\xde\xff\x83\xff\xd9\xff\xdf\xff\xaf\xff\x91\xff\x90\xff\x8f\xff\x8e\xff\x8d\xff\x8c\xff\x8b\xff\x8a\xff\x89\xff\x98\xff\xb5\xff\xb2\xff\xb0\xff\xae\xff\xe6\xff\xab\xff\xa9\xff\xa7\xff\xa5\xff\xa3\xff\xa0\xff\x9c\xff\x99\xff\x97\xff\x94\xff\x93\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x69\xff\x6b\xff\x68\xff\x6a\xff\xfd\xff\xfc\xff\xfa\xff\xf9\xff\xf5\xff\xf4\xff\xf3\xff\xf2\xff\xe1\xff\x84\xff\xaf\xff\xb6\xff\x00\x00\x98\xff\x00\x00\x9a\xff\xa6\xff\x9b\xff\xb3\xff\xae\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x6e\xff\x71\xff\x70\xff\x6f\xff\x6c\xff\x6d\xff\x00\x00\x00\x00\x00\x00\x00\x00\xb4\xff\x88\xff\xd8\xff\x00\x00\x00\x00\xda\xff\x00\x00\x00\x00\xd9\xff\x87\xff\x00\x00\x00\x00\xac\xff\xaa\xff\xa8\xff\xa4\xff\xa1\xff\xa2\xff\x9e\xff\x9f\xff\x9d\xff\x95\xff\x92\xff\xad\xff\x00\x00\xb1\xff\x96\xff\x88\xff\xd7\xff\xdc\xff\xdd\xff\xd5\xff\xd3\xff\x86\xff\xd4\xff\x00\x00\xaf\xff\xc5\xff\xc3\xff\xc2\xff\xc1\xff\xc0\xff\xbf\xff\xbe\xff\xd1\xff\xd2\xff\xcd\xff\xcc\xff\x00\x00\x00\x00\xc9\xff\xc8\xff\xc7\xff\xc6\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xd6\xff\xf8\xff\xf7\xff\xf6\xff\xf1\xff\xf0\xff\xef\xff\xee\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x75\xff\x73\xff\x77\xff\x79\xff\x78\xff\x76\xff\xce\xff\x7a\xff\x74\xff\x72\xff\x00\x00\xcb\xff\xc4\xff\x88\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xcf\xff\x00\x00\x00\x00\xd0\xff\xca\xff\x00\x00\x00\x00\xbc\xff\xb9\xff\xbd\xff\x00\x00\x00\x00\x00\x00\xb8\xff\x00\x00\xba\xff\xbb\xff\x00\x00\xb7\xff"#

happyCheck :: HappyAddr
happyCheck = HappyA# "\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x0f\x00\x15\x00\x32\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x15\x00\x00\x00\x0a\x00\x00\x00\x0d\x00\x0c\x00\x28\x00\x43\x00\x11\x00\x10\x00\x08\x00\x1d\x00\x2e\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x32\x00\x1c\x00\x30\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x30\x00\x30\x00\x1b\x00\x18\x00\x30\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x2f\x00\x30\x00\x32\x00\x0f\x00\x00\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x09\x00\x09\x00\x32\x00\x15\x00\x14\x00\x44\x00\x45\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x03\x00\x18\x00\x0f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x0a\x00\x35\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x15\x00\x14\x00\x13\x00\x1d\x00\x32\x00\x0a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x30\x00\x30\x00\x21\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x44\x00\x3f\x00\x08\x00\x41\x00\x1c\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x00\x00\x3f\x00\x32\x00\x41\x00\x09\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\x1f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x3f\x00\x40\x00\x41\x00\x27\x00\x32\x00\x30\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x09\x00\x09\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x1c\x00\x1d\x00\x1e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1c\x00\x1d\x00\x1e\x00\x08\x00\x44\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x30\x00\x30\x00\x09\x00\x1c\x00\x30\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\x06\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x11\x00\x1e\x00\x13\x00\x30\x00\x08\x00\x00\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x3f\x00\x09\x00\x41\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x09\x00\x09\x00\x0f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x15\x00\x2e\x00\x00\x00\x44\x00\x19\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x15\x00\x09\x00\x06\x00\x1e\x00\x19\x00\x08\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\x15\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x09\x00\x08\x00\x2e\x00\x08\x00\x12\x00\x00\x00\x14\x00\x08\x00\x08\x00\x2d\x00\x30\x00\x09\x00\x1a\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x08\x00\x24\x00\x2e\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x15\x00\x16\x00\x17\x00\x18\x00\x15\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3f\x00\x15\x00\x41\x00\x15\x00\x15\x00\x2e\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\x03\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x20\x00\x21\x00\x22\x00\x44\x00\x43\x00\x25\x00\x1f\x00\x27\x00\x00\x00\x29\x00\x20\x00\x2b\x00\x2c\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x42\x00\x1f\x00\x1f\x00\x09\x00\x0a\x00\x0b\x00\x0c\x00\x1f\x00\x1f\x00\x28\x00\x02\x00\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x16\x00\x17\x00\xff\xff\x19\x00\x1a\x00\x1b\x00\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x15\x00\x16\x00\x17\x00\x18\x00\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x44\x00\x45\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\x44\x00\x45\x00\xff\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x20\x00\x21\x00\x22\x00\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\x29\x00\xff\xff\x2b\x00\x2c\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\x44\x00\x45\x00\xff\xff\xff\xff\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x08\x00\xff\xff\x0a\x00\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\x44\x00\x45\x00\xff\xff\xff\xff\xff\xff\x32\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\x44\x00\x45\x00\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\x44\x00\x45\x00\xff\xff\xff\xff\xff\xff\x32\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\xff\xff\xff\xff\x00\x00\xff\xff\xff\xff\xff\xff\x44\x00\x45\x00\x06\x00\x07\x00\x08\x00\xff\xff\xff\xff\xff\xff\xff\xff\x0d\x00\x0e\x00\x0f\x00\x10\x00\x00\x00\x01\x00\x02\x00\x03\x00\x04\x00\x05\x00\xff\xff\xff\xff\xff\xff\x09\x00\x0a\x00\x0b\x00\x0c\x00\xff\xff\x1f\x00\xff\xff\xff\xff\x22\x00\x23\x00\x24\x00\x25\x00\x26\x00\x27\x00\xff\xff\x29\x00\x2a\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xff\xff\xff\xff\x44\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x01\x00\xff\xff\xff\xff\xff\xff\x05\x00\xff\xff\x44\x00\x08\x00\xff\xff\x0a\x00\xff\xff\x0c\x00\x0d\x00\xff\xff\x04\x00\x10\x00\x11\x00\x07\x00\xff\xff\xff\xff\xff\xff\x0b\x00\xff\xff\xff\xff\x0e\x00\xff\xff\xff\xff\xff\xff\x12\x00\xff\xff\x14\x00\x15\x00\xff\xff\xff\xff\x18\x00\x08\x00\xff\xff\x0a\x00\xff\xff\xff\xff\x0d\x00\x1f\x00\xff\xff\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\x2f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x23\x00\x05\x00\xff\xff\x26\x00\x08\x00\x28\x00\x0a\x00\xff\xff\x0c\x00\x0d\x00\x2d\x00\x2e\x00\x10\x00\x11\x00\x31\x00\x32\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x38\x00\x39\x00\x3a\x00\xff\xff\xff\xff\xff\xff\xff\xff\x3f\x00\x40\x00\x41\x00\x42\x00\x08\x00\xff\xff\x0a\x00\xff\xff\xff\xff\x0d\x00\xff\xff\xff\xff\xff\xff\x11\x00\xff\xff\xff\xff\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x20\x00\x21\x00\xff\xff\xff\xff\xff\xff\x25\x00\xff\xff\xff\xff\xff\xff\x29\x00\xff\xff\x2b\x00\x2c\x00\xff\xff\xff\xff\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\xff\xff\xff\xff\xff\xff\x3b\x00\x3c\x00\x3d\x00\x3e\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

happyTable :: HappyAddr
happyTable = HappyA# "\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x92\x00\xd8\x00\x03\x00\x31\x00\x32\x00\x33\x00\x34\x00\xe3\x00\x26\x00\x29\x00\x5b\x00\x4f\x00\x6a\x00\xb0\x00\xff\xff\x51\x00\x6b\x00\x5e\x00\x90\x00\x96\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x03\x00\x1f\x00\x75\x00\x31\x00\x32\x00\x33\x00\x34\x00\x75\x00\x75\x00\x27\x00\x24\x00\x75\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x7f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x80\x00\x63\x00\x64\x00\x03\x00\x26\x00\x5b\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x7f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x97\x00\x8d\x00\xd9\x00\x82\xff\x25\x00\xd0\x00\x47\x00\x48\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x67\x00\x24\x00\x23\x00\x31\x00\x32\x00\x33\x00\x34\x00\x68\x00\x21\x00\x60\x00\x36\x00\x37\x00\x38\x00\x39\x00\x22\x00\x98\x00\x69\x00\x5b\x00\x03\x00\x1e\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x75\x00\x75\x00\x99\x00\x31\x00\x32\x00\x33\x00\x34\x00\x47\x00\x08\x00\x2a\x00\x09\x00\x1f\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x7f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\xcc\x00\x5b\x00\x08\x00\x03\x00\x09\x00\xd5\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x3a\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\xe6\x00\x31\x00\x32\x00\x33\x00\x34\x00\x12\x00\x13\x00\x14\x00\xe7\x00\x81\xff\x75\x00\x8e\x00\x36\x00\x37\x00\x38\x00\x39\x00\xd4\x00\xe4\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x78\x00\x79\x00\x7a\x00\x31\x00\x32\x00\x33\x00\x34\x00\x78\x00\x79\x00\x92\x00\x78\x00\x47\x00\x5e\x00\x36\x00\x37\x00\x38\x00\x39\x00\x5f\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x5b\x00\x75\x00\x75\x00\xe9\x00\x76\x00\x75\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x81\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x74\x00\x31\x00\x32\x00\x33\x00\x34\x00\x03\x00\x73\x00\x04\x00\x75\x00\x66\x00\x15\x00\x5c\x00\x37\x00\x38\x00\x39\x00\x7d\x00\x8e\x00\x09\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x8c\x00\x7d\x00\x7f\x00\x31\x00\x32\x00\x33\x00\x34\x00\x16\x00\x96\x00\x15\x00\x47\x00\x17\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xca\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x16\x00\x91\x00\x74\x00\x73\x00\x2a\x00\xcc\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xd2\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\xca\x00\x31\x00\x32\x00\x33\x00\x34\x00\x8e\x00\xc9\x00\x96\x00\xbc\x00\x05\x00\x18\x00\x06\x00\xbb\x00\xba\x00\xd0\x00\x75\x00\xda\x00\x07\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xd7\x00\xe2\x00\x96\x00\x31\x00\x32\x00\x33\x00\x34\x00\x19\x00\x1a\x00\x1b\x00\x1c\x00\xd6\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xd1\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x08\x00\xdc\x00\x09\x00\xdb\x00\xe5\x00\x96\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xce\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x1f\x00\x31\x00\x32\x00\x33\x00\x34\x00\x0b\x00\x0c\x00\x0d\x00\x76\x00\x6b\x00\x0e\x00\x94\x00\x0f\x00\x93\x00\x10\x00\x96\x00\x11\x00\x12\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\xbd\x00\xbc\x00\xdf\x00\x31\x00\x32\x00\x33\x00\x34\x00\xde\x00\xe9\x00\xe0\x00\x6d\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xcd\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x6e\x00\x6f\x00\x00\x00\x70\x00\x71\x00\x72\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xdd\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x18\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x19\x00\x1a\x00\x1b\x00\x59\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xdc\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xe5\x00\x3b\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x82\x00\x3c\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x47\x00\x48\x00\x83\x00\x3d\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\xdb\xff\xdb\xff\x00\x00\x00\x00\x00\x00\xdb\xff\x00\x00\x00\x00\x00\x00\xdb\xff\x7c\x00\xdb\xff\xdb\xff\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x00\x00\x84\x00\x3e\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x47\x00\x48\x00\x00\x00\x61\x00\x3f\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x0b\x00\x0c\x00\x0d\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x11\x00\x12\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x85\x00\x40\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x87\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x5e\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x86\x00\x41\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x03\x00\x8a\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x47\x00\x48\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x5e\x00\x00\x00\x00\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x89\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x47\x00\x48\x00\x00\x00\x00\x00\x00\x00\x03\x00\x88\x00\x42\x00\x43\x00\x44\x00\x45\x00\x46\x00\x00\x00\x00\x00\x9a\x00\x00\x00\x00\x00\x00\x00\x47\x00\x48\x00\x9b\x00\x9c\x00\x9d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x9e\x00\x9f\x00\xa0\x00\xa1\x00\x2b\x00\x2c\x00\x2d\x00\x2e\x00\x2f\x00\x30\x00\x00\x00\x00\x00\x00\x00\x31\x00\x32\x00\x33\x00\x34\x00\x00\x00\xa2\x00\x00\x00\x00\x00\xa3\x00\xa4\x00\xa5\x00\xa6\x00\xa7\x00\xa8\x00\x00\x00\xa9\x00\xaa\x00\xab\x00\xac\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\x00\x00\x00\x00\x47\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x62\x00\x43\x00\x44\x00\x45\x00\x46\x00\x4a\x00\x00\x00\x00\x00\x00\x00\x4b\x00\x00\x00\x47\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\x00\x00\xbf\x00\x50\x00\x51\x00\xc0\x00\x00\x00\x00\x00\x00\x00\xc1\x00\x00\x00\x00\x00\xc2\x00\x00\x00\x00\x00\x00\x00\xc3\x00\x00\x00\xc4\x00\xc5\x00\x00\x00\x00\x00\xc6\x00\x5e\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x4f\x00\xc7\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x52\x00\x53\x00\x21\x00\x54\x00\x55\x00\x00\x00\x00\x00\xc8\x00\x56\x00\x57\x00\x58\x00\x59\x00\xae\x00\x4b\x00\x00\x00\xaf\x00\x4c\x00\xb0\x00\x4d\x00\x00\x00\x4e\x00\x4f\x00\xb1\x00\x96\x00\x50\x00\x51\x00\xb2\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb3\x00\xb4\x00\xb5\x00\x00\x00\x00\x00\x00\x00\x00\x00\xb6\x00\xb7\x00\xb8\x00\xb9\x00\x4c\x00\x00\x00\x4d\x00\x00\x00\x00\x00\x4f\x00\x00\x00\x00\x00\x00\x00\x51\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x52\x00\x53\x00\x21\x00\x54\x00\x55\x00\x00\x00\x00\x00\x00\x00\x56\x00\x57\x00\x58\x00\x59\x00\x0b\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x0e\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00\x11\x00\x12\x00\x00\x00\x00\x00\x03\x00\x52\x00\x53\x00\x21\x00\x54\x00\x55\x00\x00\x00\x00\x00\x00\x00\x56\x00\x57\x00\x58\x00\x59\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#

happyReduceArr = Happy_Data_Array.array (1, 151) [
	(1 , happyReduce_1),
	(2 , happyReduce_2),
	(3 , happyReduce_3),
	(4 , happyReduce_4),
	(5 , happyReduce_5),
	(6 , happyReduce_6),
	(7 , happyReduce_7),
	(8 , happyReduce_8),
	(9 , happyReduce_9),
	(10 , happyReduce_10),
	(11 , happyReduce_11),
	(12 , happyReduce_12),
	(13 , happyReduce_13),
	(14 , happyReduce_14),
	(15 , happyReduce_15),
	(16 , happyReduce_16),
	(17 , happyReduce_17),
	(18 , happyReduce_18),
	(19 , happyReduce_19),
	(20 , happyReduce_20),
	(21 , happyReduce_21),
	(22 , happyReduce_22),
	(23 , happyReduce_23),
	(24 , happyReduce_24),
	(25 , happyReduce_25),
	(26 , happyReduce_26),
	(27 , happyReduce_27),
	(28 , happyReduce_28),
	(29 , happyReduce_29),
	(30 , happyReduce_30),
	(31 , happyReduce_31),
	(32 , happyReduce_32),
	(33 , happyReduce_33),
	(34 , happyReduce_34),
	(35 , happyReduce_35),
	(36 , happyReduce_36),
	(37 , happyReduce_37),
	(38 , happyReduce_38),
	(39 , happyReduce_39),
	(40 , happyReduce_40),
	(41 , happyReduce_41),
	(42 , happyReduce_42),
	(43 , happyReduce_43),
	(44 , happyReduce_44),
	(45 , happyReduce_45),
	(46 , happyReduce_46),
	(47 , happyReduce_47),
	(48 , happyReduce_48),
	(49 , happyReduce_49),
	(50 , happyReduce_50),
	(51 , happyReduce_51),
	(52 , happyReduce_52),
	(53 , happyReduce_53),
	(54 , happyReduce_54),
	(55 , happyReduce_55),
	(56 , happyReduce_56),
	(57 , happyReduce_57),
	(58 , happyReduce_58),
	(59 , happyReduce_59),
	(60 , happyReduce_60),
	(61 , happyReduce_61),
	(62 , happyReduce_62),
	(63 , happyReduce_63),
	(64 , happyReduce_64),
	(65 , happyReduce_65),
	(66 , happyReduce_66),
	(67 , happyReduce_67),
	(68 , happyReduce_68),
	(69 , happyReduce_69),
	(70 , happyReduce_70),
	(71 , happyReduce_71),
	(72 , happyReduce_72),
	(73 , happyReduce_73),
	(74 , happyReduce_74),
	(75 , happyReduce_75),
	(76 , happyReduce_76),
	(77 , happyReduce_77),
	(78 , happyReduce_78),
	(79 , happyReduce_79),
	(80 , happyReduce_80),
	(81 , happyReduce_81),
	(82 , happyReduce_82),
	(83 , happyReduce_83),
	(84 , happyReduce_84),
	(85 , happyReduce_85),
	(86 , happyReduce_86),
	(87 , happyReduce_87),
	(88 , happyReduce_88),
	(89 , happyReduce_89),
	(90 , happyReduce_90),
	(91 , happyReduce_91),
	(92 , happyReduce_92),
	(93 , happyReduce_93),
	(94 , happyReduce_94),
	(95 , happyReduce_95),
	(96 , happyReduce_96),
	(97 , happyReduce_97),
	(98 , happyReduce_98),
	(99 , happyReduce_99),
	(100 , happyReduce_100),
	(101 , happyReduce_101),
	(102 , happyReduce_102),
	(103 , happyReduce_103),
	(104 , happyReduce_104),
	(105 , happyReduce_105),
	(106 , happyReduce_106),
	(107 , happyReduce_107),
	(108 , happyReduce_108),
	(109 , happyReduce_109),
	(110 , happyReduce_110),
	(111 , happyReduce_111),
	(112 , happyReduce_112),
	(113 , happyReduce_113),
	(114 , happyReduce_114),
	(115 , happyReduce_115),
	(116 , happyReduce_116),
	(117 , happyReduce_117),
	(118 , happyReduce_118),
	(119 , happyReduce_119),
	(120 , happyReduce_120),
	(121 , happyReduce_121),
	(122 , happyReduce_122),
	(123 , happyReduce_123),
	(124 , happyReduce_124),
	(125 , happyReduce_125),
	(126 , happyReduce_126),
	(127 , happyReduce_127),
	(128 , happyReduce_128),
	(129 , happyReduce_129),
	(130 , happyReduce_130),
	(131 , happyReduce_131),
	(132 , happyReduce_132),
	(133 , happyReduce_133),
	(134 , happyReduce_134),
	(135 , happyReduce_135),
	(136 , happyReduce_136),
	(137 , happyReduce_137),
	(138 , happyReduce_138),
	(139 , happyReduce_139),
	(140 , happyReduce_140),
	(141 , happyReduce_141),
	(142 , happyReduce_142),
	(143 , happyReduce_143),
	(144 , happyReduce_144),
	(145 , happyReduce_145),
	(146 , happyReduce_146),
	(147 , happyReduce_147),
	(148 , happyReduce_148),
	(149 , happyReduce_149),
	(150 , happyReduce_150),
	(151 , happyReduce_151)
	]

happy_n_terms = 68 :: Int
happy_n_nonterms = 70 :: Int

happyReduce_1 = happySpecReduce_1  0# happyReduction_1
happyReduction_1 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn4
		 (Id (mkPosToken happy_var_1)
	)}

happyReduce_2 = happySpecReduce_1  1# happyReduction_2
happyReduction_2 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn5
		 (PBool (mkPosToken happy_var_1)
	)}

happyReduce_3 = happySpecReduce_1  2# happyReduction_3
happyReduction_3 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn6
		 (PChar (mkPosToken happy_var_1)
	)}

happyReduce_4 = happySpecReduce_1  3# happyReduction_4
happyReduction_4 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn7
		 (PInt (mkPosToken happy_var_1)
	)}

happyReduce_5 = happySpecReduce_1  4# happyReduction_5
happyReduction_5 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn8
		 (PFloat (mkPosToken happy_var_1)
	)}

happyReduce_6 = happySpecReduce_1  5# happyReduction_6
happyReduction_6 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn9
		 (PString (mkPosToken happy_var_1)
	)}

happyReduce_7 = happySpecReduce_1  6# happyReduction_7
happyReduction_7 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn10
		 (Return (mkPosToken happy_var_1)
	)}

happyReduce_8 = happySpecReduce_1  7# happyReduction_8
happyReduction_8 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn11
		 (Break (mkPosToken happy_var_1)
	)}

happyReduce_9 = happySpecReduce_1  8# happyReduction_9
happyReduction_9 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn12
		 (Continue (mkPosToken happy_var_1)
	)}

happyReduce_10 = happySpecReduce_1  9# happyReduction_10
happyReduction_10 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn13
		 (RChar (mkPosToken happy_var_1)
	)}

happyReduce_11 = happySpecReduce_1  10# happyReduction_11
happyReduction_11 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn14
		 (RInt (mkPosToken happy_var_1)
	)}

happyReduce_12 = happySpecReduce_1  11# happyReduction_12
happyReduction_12 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn15
		 (RFloat (mkPosToken happy_var_1)
	)}

happyReduce_13 = happySpecReduce_1  12# happyReduction_13
happyReduction_13 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn16
		 (RString (mkPosToken happy_var_1)
	)}

happyReduce_14 = happySpecReduce_1  13# happyReduction_14
happyReduction_14 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn17
		 (WChar (mkPosToken happy_var_1)
	)}

happyReduce_15 = happySpecReduce_1  14# happyReduction_15
happyReduction_15 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn18
		 (WInt (mkPosToken happy_var_1)
	)}

happyReduce_16 = happySpecReduce_1  15# happyReduction_16
happyReduction_16 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn19
		 (WFloat (mkPosToken happy_var_1)
	)}

happyReduce_17 = happySpecReduce_1  16# happyReduction_17
happyReduction_17 happy_x_1
	 =  case happyOutTok happy_x_1 of { happy_var_1 -> 
	happyIn20
		 (WString (mkPosToken happy_var_1)
	)}

happyReduce_18 = happySpecReduce_1  17# happyReduction_18
happyReduction_18 happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	happyIn21
		 (AbsGarpez.Prog (reverse happy_var_1)
	)}

happyReduce_19 = happySpecReduce_1  18# happyReduction_19
happyReduction_19 happy_x_1
	 =  case happyOut24 happy_x_1 of { (HappyWrap24 happy_var_1) -> 
	happyIn22
		 (AbsGarpez.GlobalDecl happy_var_1
	)}

happyReduce_20 = happySpecReduce_1  18# happyReduction_20
happyReduction_20 happy_x_1
	 =  case happyOut30 happy_x_1 of { (HappyWrap30 happy_var_1) -> 
	happyIn22
		 (AbsGarpez.FunDecl happy_var_1
	)}

happyReduce_21 = happySpecReduce_0  19# happyReduction_21
happyReduction_21  =  happyIn23
		 ([]
	)

happyReduce_22 = happySpecReduce_2  19# happyReduction_22
happyReduction_22 happy_x_2
	happy_x_1
	 =  case happyOut23 happy_x_1 of { (HappyWrap23 happy_var_1) -> 
	case happyOut22 happy_x_2 of { (HappyWrap22 happy_var_2) -> 
	happyIn23
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_23 = happySpecReduce_3  20# happyReduction_23
happyReduction_23 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut29 happy_x_2 of { (HappyWrap29 happy_var_2) -> 
	happyIn24
		 (AbsGarpez.ConstDecl happy_var_2
	)}

happyReduce_24 = happySpecReduce_3  20# happyReduction_24
happyReduction_24 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut28 happy_x_2 of { (HappyWrap28 happy_var_2) -> 
	happyIn24
		 (AbsGarpez.VarDecl happy_var_1 happy_var_2
	)}}

happyReduce_25 = happySpecReduce_3  21# happyReduction_25
happyReduction_25 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn25
		 (AbsGarpez.InitDecl happy_var_1 happy_var_3
	)}}

happyReduce_26 = happySpecReduce_1  22# happyReduction_26
happyReduction_26 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn26
		 (AbsGarpez.DeclOnly happy_var_1
	)}

happyReduce_27 = happySpecReduce_1  23# happyReduction_27
happyReduction_27 happy_x_1
	 =  case happyOut26 happy_x_1 of { (HappyWrap26 happy_var_1) -> 
	happyIn27
		 (AbsGarpez.DeclItemDeclId happy_var_1
	)}

happyReduce_28 = happySpecReduce_1  23# happyReduction_28
happyReduction_28 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn27
		 (AbsGarpez.DeclItemInitItem happy_var_1
	)}

happyReduce_29 = happySpecReduce_1  24# happyReduction_29
happyReduction_29 happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	happyIn28
		 ((:[]) happy_var_1
	)}

happyReduce_30 = happySpecReduce_3  24# happyReduction_30
happyReduction_30 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut27 happy_x_1 of { (HappyWrap27 happy_var_1) -> 
	case happyOut28 happy_x_3 of { (HappyWrap28 happy_var_3) -> 
	happyIn28
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_31 = happySpecReduce_1  25# happyReduction_31
happyReduction_31 happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	happyIn29
		 ((:[]) happy_var_1
	)}

happyReduce_32 = happySpecReduce_3  25# happyReduction_32
happyReduction_32 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut25 happy_x_1 of { (HappyWrap25 happy_var_1) -> 
	case happyOut29 happy_x_3 of { (HappyWrap29 happy_var_3) -> 
	happyIn29
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_33 = happySpecReduce_3  26# happyReduction_33
happyReduction_33 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut68 happy_x_2 of { (HappyWrap68 happy_var_2) -> 
	case happyOut31 happy_x_3 of { (HappyWrap31 happy_var_3) -> 
	happyIn30
		 (AbsGarpez.Fun happy_var_2 happy_var_3
	)}}

happyReduce_34 = happyReduce 5# 27# happyReduction_34
happyReduction_34 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	case happyOut35 happy_x_5 of { (HappyWrap35 happy_var_5) -> 
	happyIn31
		 (AbsGarpez.FRest happy_var_1 happy_var_3 happy_var_5
	) `HappyStk` happyRest}}}

happyReduce_35 = happySpecReduce_3  28# happyReduction_35
happyReduction_35 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut33 happy_x_1 of { (HappyWrap33 happy_var_1) -> 
	case happyOut67 happy_x_2 of { (HappyWrap67 happy_var_2) -> 
	case happyOut4 happy_x_3 of { (HappyWrap4 happy_var_3) -> 
	happyIn32
		 (AbsGarpez.Param happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_36 = happySpecReduce_0  29# happyReduction_36
happyReduction_36  =  happyIn33
		 (AbsGarpez.ValuePass
	)

happyReduce_37 = happySpecReduce_1  29# happyReduction_37
happyReduction_37 happy_x_1
	 =  happyIn33
		 (AbsGarpez.RefPass
	)

happyReduce_38 = happySpecReduce_0  30# happyReduction_38
happyReduction_38  =  happyIn34
		 ([]
	)

happyReduce_39 = happySpecReduce_1  30# happyReduction_39
happyReduction_39 happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	happyIn34
		 ((:[]) happy_var_1
	)}

happyReduce_40 = happySpecReduce_3  30# happyReduction_40
happyReduction_40 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut32 happy_x_1 of { (HappyWrap32 happy_var_1) -> 
	case happyOut34 happy_x_3 of { (HappyWrap34 happy_var_3) -> 
	happyIn34
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_41 = happyReduce 4# 31# happyReduction_41
happyReduction_41 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut36 happy_x_2 of { (HappyWrap36 happy_var_2) -> 
	case happyOut37 happy_x_3 of { (HappyWrap37 happy_var_3) -> 
	happyIn35
		 (AbsGarpez.Blk (reverse happy_var_2) (reverse happy_var_3)
	) `HappyStk` happyRest}}

happyReduce_42 = happySpecReduce_0  32# happyReduction_42
happyReduction_42  =  happyIn36
		 ([]
	)

happyReduce_43 = happySpecReduce_2  32# happyReduction_43
happyReduction_43 happy_x_2
	happy_x_1
	 =  case happyOut36 happy_x_1 of { (HappyWrap36 happy_var_1) -> 
	case happyOut24 happy_x_2 of { (HappyWrap24 happy_var_2) -> 
	happyIn36
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_44 = happySpecReduce_0  33# happyReduction_44
happyReduction_44  =  happyIn37
		 ([]
	)

happyReduce_45 = happySpecReduce_2  33# happyReduction_45
happyReduction_45 happy_x_2
	happy_x_1
	 =  case happyOut37 happy_x_1 of { (HappyWrap37 happy_var_1) -> 
	case happyOut38 happy_x_2 of { (HappyWrap38 happy_var_2) -> 
	happyIn37
		 (flip (:) happy_var_1 happy_var_2
	)}}

happyReduce_46 = happySpecReduce_1  34# happyReduction_46
happyReduction_46 happy_x_1
	 =  case happyOut35 happy_x_1 of { (HappyWrap35 happy_var_1) -> 
	happyIn38
		 (AbsGarpez.BlkStm happy_var_1
	)}

happyReduce_47 = happyReduce 5# 34# happyReduction_47
happyReduction_47 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn38
		 (AbsGarpez.CallStm happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_48 = happyReduce 4# 34# happyReduction_48
happyReduction_48 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	case happyOut70 happy_x_2 of { (HappyWrap70 happy_var_2) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn38
		 (AbsGarpez.AssignStm happy_var_1 happy_var_2 happy_var_3
	) `HappyStk` happyRest}}}

happyReduce_49 = happySpecReduce_2  34# happyReduction_49
happyReduction_49 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn38
		 (AbsGarpez.LExpStm happy_var_1
	)}

happyReduce_50 = happySpecReduce_1  34# happyReduction_50
happyReduction_50 happy_x_1
	 =  case happyOut39 happy_x_1 of { (HappyWrap39 happy_var_1) -> 
	happyIn38
		 (AbsGarpez.CondStm happy_var_1
	)}

happyReduce_51 = happySpecReduce_1  34# happyReduction_51
happyReduction_51 happy_x_1
	 =  case happyOut40 happy_x_1 of { (HappyWrap40 happy_var_1) -> 
	happyIn38
		 (AbsGarpez.LoopStm happy_var_1
	)}

happyReduce_52 = happySpecReduce_2  34# happyReduction_52
happyReduction_52 happy_x_2
	happy_x_1
	 =  case happyOut41 happy_x_1 of { (HappyWrap41 happy_var_1) -> 
	happyIn38
		 (AbsGarpez.JmpStm happy_var_1
	)}

happyReduce_53 = happyReduce 5# 34# happyReduction_53
happyReduction_53 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut42 happy_x_1 of { (HappyWrap42 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn38
		 (AbsGarpez.WriteStm happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_54 = happySpecReduce_1  35# happyReduction_54
happyReduction_54 happy_x_1
	 =  case happyOut43 happy_x_1 of { (HappyWrap43 happy_var_1) -> 
	happyIn39
		 (AbsGarpez.ConditionalIf happy_var_1
	)}

happyReduce_55 = happySpecReduce_1  36# happyReduction_55
happyReduction_55 happy_x_1
	 =  case happyOut45 happy_x_1 of { (HappyWrap45 happy_var_1) -> 
	happyIn40
		 (AbsGarpez.LoopWhile happy_var_1
	)}

happyReduce_56 = happySpecReduce_1  36# happyReduction_56
happyReduction_56 happy_x_1
	 =  case happyOut46 happy_x_1 of { (HappyWrap46 happy_var_1) -> 
	happyIn40
		 (AbsGarpez.LoopDoWhile happy_var_1
	)}

happyReduce_57 = happySpecReduce_1  36# happyReduction_57
happyReduction_57 happy_x_1
	 =  case happyOut47 happy_x_1 of { (HappyWrap47 happy_var_1) -> 
	happyIn40
		 (AbsGarpez.LoopFor happy_var_1
	)}

happyReduce_58 = happySpecReduce_1  37# happyReduction_58
happyReduction_58 happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	happyIn41
		 (AbsGarpez.JumpReturn happy_var_1
	)}

happyReduce_59 = happySpecReduce_2  37# happyReduction_59
happyReduction_59 happy_x_2
	happy_x_1
	 =  case happyOut10 happy_x_1 of { (HappyWrap10 happy_var_1) -> 
	case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn41
		 (AbsGarpez.Jump1 happy_var_1 happy_var_2
	)}}

happyReduce_60 = happySpecReduce_1  37# happyReduction_60
happyReduction_60 happy_x_1
	 =  case happyOut11 happy_x_1 of { (HappyWrap11 happy_var_1) -> 
	happyIn41
		 (AbsGarpez.JumpBreak happy_var_1
	)}

happyReduce_61 = happySpecReduce_1  37# happyReduction_61
happyReduction_61 happy_x_1
	 =  case happyOut12 happy_x_1 of { (HappyWrap12 happy_var_1) -> 
	happyIn41
		 (AbsGarpez.JumpContinue happy_var_1
	)}

happyReduce_62 = happySpecReduce_1  38# happyReduction_62
happyReduction_62 happy_x_1
	 =  case happyOut17 happy_x_1 of { (HappyWrap17 happy_var_1) -> 
	happyIn42
		 (AbsGarpez.WPredefinedWChar happy_var_1
	)}

happyReduce_63 = happySpecReduce_1  38# happyReduction_63
happyReduction_63 happy_x_1
	 =  case happyOut18 happy_x_1 of { (HappyWrap18 happy_var_1) -> 
	happyIn42
		 (AbsGarpez.WPredefinedWInt happy_var_1
	)}

happyReduce_64 = happySpecReduce_1  38# happyReduction_64
happyReduction_64 happy_x_1
	 =  case happyOut19 happy_x_1 of { (HappyWrap19 happy_var_1) -> 
	happyIn42
		 (AbsGarpez.WPredefinedWFloat happy_var_1
	)}

happyReduce_65 = happySpecReduce_1  38# happyReduction_65
happyReduction_65 happy_x_1
	 =  case happyOut20 happy_x_1 of { (HappyWrap20 happy_var_1) -> 
	happyIn42
		 (AbsGarpez.WPredefinedWString happy_var_1
	)}

happyReduce_66 = happyReduce 6# 39# happyReduction_66
happyReduction_66 (happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	case happyOut35 happy_x_5 of { (HappyWrap35 happy_var_5) -> 
	case happyOut44 happy_x_6 of { (HappyWrap44 happy_var_6) -> 
	happyIn43
		 (AbsGarpez.IfCond happy_var_3 happy_var_5 happy_var_6
	) `HappyStk` happyRest}}}

happyReduce_67 = happySpecReduce_0  40# happyReduction_67
happyReduction_67  =  happyIn44
		 (AbsGarpez.RestIf_
	)

happyReduce_68 = happySpecReduce_2  40# happyReduction_68
happyReduction_68 happy_x_2
	happy_x_1
	 =  case happyOut43 happy_x_2 of { (HappyWrap43 happy_var_2) -> 
	happyIn44
		 (AbsGarpez.RestIf1 happy_var_2
	)}

happyReduce_69 = happySpecReduce_2  40# happyReduction_69
happyReduction_69 happy_x_2
	happy_x_1
	 =  case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	happyIn44
		 (AbsGarpez.RestIf2 happy_var_2
	)}

happyReduce_70 = happyReduce 5# 41# happyReduction_70
happyReduction_70 (happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	case happyOut35 happy_x_5 of { (HappyWrap35 happy_var_5) -> 
	happyIn45
		 (AbsGarpez.WhileLoop happy_var_3 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_71 = happyReduce 7# 42# happyReduction_71
happyReduction_71 (happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut35 happy_x_2 of { (HappyWrap35 happy_var_2) -> 
	case happyOut53 happy_x_5 of { (HappyWrap53 happy_var_5) -> 
	happyIn46
		 (AbsGarpez.DoWhileLoop happy_var_2 happy_var_5
	) `HappyStk` happyRest}}

happyReduce_72 = happyReduce 9# 43# happyReduction_72
happyReduction_72 (happy_x_9 `HappyStk`
	happy_x_8 `HappyStk`
	happy_x_7 `HappyStk`
	happy_x_6 `HappyStk`
	happy_x_5 `HappyStk`
	happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut24 happy_x_3 of { (HappyWrap24 happy_var_3) -> 
	case happyOut53 happy_x_5 of { (HappyWrap53 happy_var_5) -> 
	case happyOut53 happy_x_7 of { (HappyWrap53 happy_var_7) -> 
	case happyOut35 happy_x_9 of { (HappyWrap35 happy_var_9) -> 
	happyIn47
		 (AbsGarpez.ForLoop happy_var_3 happy_var_5 happy_var_7 happy_var_9
	) `HappyStk` happyRest}}}}

happyReduce_73 = happySpecReduce_2  44# happyReduction_73
happyReduction_73 happy_x_2
	happy_x_1
	 =  case happyOut49 happy_x_2 of { (HappyWrap49 happy_var_2) -> 
	happyIn48
		 (AbsGarpez.Dereference happy_var_2
	)}

happyReduce_74 = happySpecReduce_1  44# happyReduction_74
happyReduction_74 happy_x_1
	 =  case happyOut49 happy_x_1 of { (HappyWrap49 happy_var_1) -> 
	happyIn48
		 (happy_var_1
	)}

happyReduce_75 = happySpecReduce_2  45# happyReduction_75
happyReduction_75 happy_x_2
	happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	case happyOut72 happy_x_2 of { (HappyWrap72 happy_var_2) -> 
	happyIn49
		 (AbsGarpez.Post happy_var_1 happy_var_2
	)}}

happyReduce_76 = happySpecReduce_2  45# happyReduction_76
happyReduction_76 happy_x_2
	happy_x_1
	 =  case happyOut72 happy_x_1 of { (HappyWrap72 happy_var_1) -> 
	case happyOut51 happy_x_2 of { (HappyWrap51 happy_var_2) -> 
	happyIn49
		 (AbsGarpez.Pre happy_var_1 happy_var_2
	)}}

happyReduce_77 = happySpecReduce_1  45# happyReduction_77
happyReduction_77 happy_x_1
	 =  case happyOut50 happy_x_1 of { (HappyWrap50 happy_var_1) -> 
	happyIn49
		 (happy_var_1
	)}

happyReduce_78 = happyReduce 4# 46# happyReduction_78
happyReduction_78 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	case happyOut53 happy_x_3 of { (HappyWrap53 happy_var_3) -> 
	happyIn50
		 (AbsGarpez.ArrayAccess happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_79 = happySpecReduce_1  46# happyReduction_79
happyReduction_79 happy_x_1
	 =  case happyOut51 happy_x_1 of { (HappyWrap51 happy_var_1) -> 
	happyIn50
		 (happy_var_1
	)}

happyReduce_80 = happySpecReduce_1  47# happyReduction_80
happyReduction_80 happy_x_1
	 =  case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	happyIn51
		 (AbsGarpez.IdExp happy_var_1
	)}

happyReduce_81 = happySpecReduce_1  47# happyReduction_81
happyReduction_81 happy_x_1
	 =  case happyOut52 happy_x_1 of { (HappyWrap52 happy_var_1) -> 
	happyIn51
		 (happy_var_1
	)}

happyReduce_82 = happySpecReduce_3  48# happyReduction_82
happyReduction_82 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn52
		 (happy_var_2
	)}

happyReduce_83 = happySpecReduce_3  49# happyReduction_83
happyReduction_83 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut54 happy_x_3 of { (HappyWrap54 happy_var_3) -> 
	happyIn53
		 (AbsGarpez.LogicalOr happy_var_1 happy_var_3
	)}}

happyReduce_84 = happySpecReduce_1  49# happyReduction_84
happyReduction_84 happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	happyIn53
		 (happy_var_1
	)}

happyReduce_85 = happySpecReduce_3  50# happyReduction_85
happyReduction_85 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut54 happy_x_1 of { (HappyWrap54 happy_var_1) -> 
	case happyOut55 happy_x_3 of { (HappyWrap55 happy_var_3) -> 
	happyIn54
		 (AbsGarpez.LogicalAnd happy_var_1 happy_var_3
	)}}

happyReduce_86 = happySpecReduce_1  50# happyReduction_86
happyReduction_86 happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	happyIn54
		 (happy_var_1
	)}

happyReduce_87 = happySpecReduce_3  51# happyReduction_87
happyReduction_87 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut55 happy_x_1 of { (HappyWrap55 happy_var_1) -> 
	case happyOut56 happy_x_3 of { (HappyWrap56 happy_var_3) -> 
	happyIn55
		 (AbsGarpez.LogicalXor happy_var_1 happy_var_3
	)}}

happyReduce_88 = happySpecReduce_1  51# happyReduction_88
happyReduction_88 happy_x_1
	 =  case happyOut56 happy_x_1 of { (HappyWrap56 happy_var_1) -> 
	happyIn55
		 (happy_var_1
	)}

happyReduce_89 = happySpecReduce_2  52# happyReduction_89
happyReduction_89 happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_2 of { (HappyWrap57 happy_var_2) -> 
	happyIn56
		 (AbsGarpez.LogicalNot happy_var_2
	)}

happyReduce_90 = happySpecReduce_1  52# happyReduction_90
happyReduction_90 happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	happyIn56
		 (happy_var_1
	)}

happyReduce_91 = happySpecReduce_3  53# happyReduction_91
happyReduction_91 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut57 happy_x_1 of { (HappyWrap57 happy_var_1) -> 
	case happyOut71 happy_x_2 of { (HappyWrap71 happy_var_2) -> 
	case happyOut58 happy_x_3 of { (HappyWrap58 happy_var_3) -> 
	happyIn57
		 (AbsGarpez.Comparison happy_var_1 happy_var_2 happy_var_3
	)}}}

happyReduce_92 = happySpecReduce_1  53# happyReduction_92
happyReduction_92 happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	happyIn57
		 (happy_var_1
	)}

happyReduce_93 = happySpecReduce_3  54# happyReduction_93
happyReduction_93 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn58
		 (AbsGarpez.Sum happy_var_1 happy_var_3
	)}}

happyReduce_94 = happySpecReduce_3  54# happyReduction_94
happyReduction_94 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut58 happy_x_1 of { (HappyWrap58 happy_var_1) -> 
	case happyOut59 happy_x_3 of { (HappyWrap59 happy_var_3) -> 
	happyIn58
		 (AbsGarpez.Sub happy_var_1 happy_var_3
	)}}

happyReduce_95 = happySpecReduce_1  54# happyReduction_95
happyReduction_95 happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	happyIn58
		 (happy_var_1
	)}

happyReduce_96 = happySpecReduce_3  55# happyReduction_96
happyReduction_96 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn59
		 (AbsGarpez.Mul happy_var_1 happy_var_3
	)}}

happyReduce_97 = happySpecReduce_3  55# happyReduction_97
happyReduction_97 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn59
		 (AbsGarpez.Div happy_var_1 happy_var_3
	)}}

happyReduce_98 = happySpecReduce_3  55# happyReduction_98
happyReduction_98 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut59 happy_x_1 of { (HappyWrap59 happy_var_1) -> 
	case happyOut60 happy_x_3 of { (HappyWrap60 happy_var_3) -> 
	happyIn59
		 (AbsGarpez.Mod happy_var_1 happy_var_3
	)}}

happyReduce_99 = happySpecReduce_1  55# happyReduction_99
happyReduction_99 happy_x_1
	 =  case happyOut60 happy_x_1 of { (HappyWrap60 happy_var_1) -> 
	happyIn59
		 (happy_var_1
	)}

happyReduce_100 = happySpecReduce_2  56# happyReduction_100
happyReduction_100 happy_x_2
	happy_x_1
	 =  case happyOut73 happy_x_1 of { (HappyWrap73 happy_var_1) -> 
	case happyOut61 happy_x_2 of { (HappyWrap61 happy_var_2) -> 
	happyIn60
		 (AbsGarpez.Sign happy_var_1 happy_var_2
	)}}

happyReduce_101 = happySpecReduce_2  56# happyReduction_101
happyReduction_101 happy_x_2
	happy_x_1
	 =  case happyOut48 happy_x_2 of { (HappyWrap48 happy_var_2) -> 
	happyIn60
		 (AbsGarpez.Reference happy_var_2
	)}

happyReduce_102 = happySpecReduce_1  56# happyReduction_102
happyReduction_102 happy_x_1
	 =  case happyOut61 happy_x_1 of { (HappyWrap61 happy_var_1) -> 
	happyIn60
		 (happy_var_1
	)}

happyReduce_103 = happySpecReduce_1  57# happyReduction_103
happyReduction_103 happy_x_1
	 =  case happyOut48 happy_x_1 of { (HappyWrap48 happy_var_1) -> 
	happyIn61
		 (AbsGarpez.LRExp happy_var_1
	)}

happyReduce_104 = happySpecReduce_1  57# happyReduction_104
happyReduction_104 happy_x_1
	 =  case happyOut62 happy_x_1 of { (HappyWrap62 happy_var_1) -> 
	happyIn61
		 (happy_var_1
	)}

happyReduce_105 = happyReduce 4# 58# happyReduction_105
happyReduction_105 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut4 happy_x_1 of { (HappyWrap4 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn62
		 (AbsGarpez.CallExp happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_106 = happySpecReduce_3  58# happyReduction_106
happyReduction_106 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut65 happy_x_1 of { (HappyWrap65 happy_var_1) -> 
	happyIn62
		 (AbsGarpez.ReadExp happy_var_1
	)}

happyReduce_107 = happySpecReduce_1  58# happyReduction_107
happyReduction_107 happy_x_1
	 =  case happyOut63 happy_x_1 of { (HappyWrap63 happy_var_1) -> 
	happyIn62
		 (happy_var_1
	)}

happyReduce_108 = happySpecReduce_1  59# happyReduction_108
happyReduction_108 happy_x_1
	 =  case happyOut64 happy_x_1 of { (HappyWrap64 happy_var_1) -> 
	happyIn63
		 (AbsGarpez.Lit happy_var_1
	)}

happyReduce_109 = happySpecReduce_3  59# happyReduction_109
happyReduction_109 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_2 of { (HappyWrap53 happy_var_2) -> 
	happyIn63
		 (happy_var_2
	)}

happyReduce_110 = happySpecReduce_1  60# happyReduction_110
happyReduction_110 happy_x_1
	 =  case happyOut5 happy_x_1 of { (HappyWrap5 happy_var_1) -> 
	happyIn64
		 (AbsGarpez.LiteralPBool happy_var_1
	)}

happyReduce_111 = happySpecReduce_1  60# happyReduction_111
happyReduction_111 happy_x_1
	 =  case happyOut6 happy_x_1 of { (HappyWrap6 happy_var_1) -> 
	happyIn64
		 (AbsGarpez.LiteralPChar happy_var_1
	)}

happyReduce_112 = happySpecReduce_1  60# happyReduction_112
happyReduction_112 happy_x_1
	 =  case happyOut7 happy_x_1 of { (HappyWrap7 happy_var_1) -> 
	happyIn64
		 (AbsGarpez.LiteralPInt happy_var_1
	)}

happyReduce_113 = happySpecReduce_1  60# happyReduction_113
happyReduction_113 happy_x_1
	 =  case happyOut8 happy_x_1 of { (HappyWrap8 happy_var_1) -> 
	happyIn64
		 (AbsGarpez.LiteralPFloat happy_var_1
	)}

happyReduce_114 = happySpecReduce_1  60# happyReduction_114
happyReduction_114 happy_x_1
	 =  case happyOut9 happy_x_1 of { (HappyWrap9 happy_var_1) -> 
	happyIn64
		 (AbsGarpez.LiteralPString happy_var_1
	)}

happyReduce_115 = happySpecReduce_1  61# happyReduction_115
happyReduction_115 happy_x_1
	 =  case happyOut13 happy_x_1 of { (HappyWrap13 happy_var_1) -> 
	happyIn65
		 (AbsGarpez.RPredefinedRChar happy_var_1
	)}

happyReduce_116 = happySpecReduce_1  61# happyReduction_116
happyReduction_116 happy_x_1
	 =  case happyOut14 happy_x_1 of { (HappyWrap14 happy_var_1) -> 
	happyIn65
		 (AbsGarpez.RPredefinedRInt happy_var_1
	)}

happyReduce_117 = happySpecReduce_1  61# happyReduction_117
happyReduction_117 happy_x_1
	 =  case happyOut15 happy_x_1 of { (HappyWrap15 happy_var_1) -> 
	happyIn65
		 (AbsGarpez.RPredefinedRFloat happy_var_1
	)}

happyReduce_118 = happySpecReduce_1  61# happyReduction_118
happyReduction_118 happy_x_1
	 =  case happyOut16 happy_x_1 of { (HappyWrap16 happy_var_1) -> 
	happyIn65
		 (AbsGarpez.RPredefinedRString happy_var_1
	)}

happyReduce_119 = happySpecReduce_0  62# happyReduction_119
happyReduction_119  =  happyIn66
		 ([]
	)

happyReduce_120 = happySpecReduce_1  62# happyReduction_120
happyReduction_120 happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	happyIn66
		 ((:[]) happy_var_1
	)}

happyReduce_121 = happySpecReduce_3  62# happyReduction_121
happyReduction_121 happy_x_3
	happy_x_2
	happy_x_1
	 =  case happyOut53 happy_x_1 of { (HappyWrap53 happy_var_1) -> 
	case happyOut66 happy_x_3 of { (HappyWrap66 happy_var_3) -> 
	happyIn66
		 ((:) happy_var_1 happy_var_3
	)}}

happyReduce_122 = happySpecReduce_1  63# happyReduction_122
happyReduction_122 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn67
		 (AbsGarpez.SType happy_var_1
	)}

happyReduce_123 = happyReduce 4# 63# happyReduction_123
happyReduction_123 (happy_x_4 `HappyStk`
	happy_x_3 `HappyStk`
	happy_x_2 `HappyStk`
	happy_x_1 `HappyStk`
	happyRest)
	 = case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	case happyOut7 happy_x_3 of { (HappyWrap7 happy_var_3) -> 
	happyIn67
		 (AbsGarpez.AType happy_var_1 happy_var_3
	) `HappyStk` happyRest}}

happyReduce_124 = happySpecReduce_2  63# happyReduction_124
happyReduction_124 happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn67
		 (AbsGarpez.PType happy_var_1
	)}

happyReduce_125 = happySpecReduce_1  64# happyReduction_125
happyReduction_125 happy_x_1
	 =  case happyOut69 happy_x_1 of { (HappyWrap69 happy_var_1) -> 
	happyIn68
		 (AbsGarpez.SRType happy_var_1
	)}

happyReduce_126 = happySpecReduce_2  64# happyReduction_126
happyReduction_126 happy_x_2
	happy_x_1
	 =  case happyOut67 happy_x_1 of { (HappyWrap67 happy_var_1) -> 
	happyIn68
		 (AbsGarpez.RRType happy_var_1
	)}

happyReduce_127 = happySpecReduce_1  65# happyReduction_127
happyReduction_127 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_bool
	)

happyReduce_128 = happySpecReduce_1  65# happyReduction_128
happyReduction_128 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_char
	)

happyReduce_129 = happySpecReduce_1  65# happyReduction_129
happyReduction_129 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_int
	)

happyReduce_130 = happySpecReduce_1  65# happyReduction_130
happyReduction_130 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_float
	)

happyReduce_131 = happySpecReduce_1  65# happyReduction_131
happyReduction_131 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_string
	)

happyReduce_132 = happySpecReduce_1  65# happyReduction_132
happyReduction_132 happy_x_1
	 =  happyIn69
		 (AbsGarpez.SimpleType_void
	)

happyReduce_133 = happySpecReduce_1  66# happyReduction_133
happyReduction_133 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp1
	)

happyReduce_134 = happySpecReduce_1  66# happyReduction_134
happyReduction_134 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp2
	)

happyReduce_135 = happySpecReduce_1  66# happyReduction_135
happyReduction_135 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp3
	)

happyReduce_136 = happySpecReduce_1  66# happyReduction_136
happyReduction_136 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp4
	)

happyReduce_137 = happySpecReduce_1  66# happyReduction_137
happyReduction_137 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp5
	)

happyReduce_138 = happySpecReduce_1  66# happyReduction_138
happyReduction_138 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp6
	)

happyReduce_139 = happySpecReduce_1  66# happyReduction_139
happyReduction_139 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp7
	)

happyReduce_140 = happySpecReduce_1  66# happyReduction_140
happyReduction_140 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp8
	)

happyReduce_141 = happySpecReduce_1  66# happyReduction_141
happyReduction_141 happy_x_1
	 =  happyIn70
		 (AbsGarpez.AssignmentOp9
	)

happyReduce_142 = happySpecReduce_1  67# happyReduction_142
happyReduction_142 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp1
	)

happyReduce_143 = happySpecReduce_1  67# happyReduction_143
happyReduction_143 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp2
	)

happyReduce_144 = happySpecReduce_1  67# happyReduction_144
happyReduction_144 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp3
	)

happyReduce_145 = happySpecReduce_1  67# happyReduction_145
happyReduction_145 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp4
	)

happyReduce_146 = happySpecReduce_1  67# happyReduction_146
happyReduction_146 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp5
	)

happyReduce_147 = happySpecReduce_1  67# happyReduction_147
happyReduction_147 happy_x_1
	 =  happyIn71
		 (AbsGarpez.ComparisonOp6
	)

happyReduce_148 = happySpecReduce_1  68# happyReduction_148
happyReduction_148 happy_x_1
	 =  happyIn72
		 (AbsGarpez.IncDecOp1
	)

happyReduce_149 = happySpecReduce_1  68# happyReduction_149
happyReduction_149 happy_x_1
	 =  happyIn72
		 (AbsGarpez.IncDecOp2
	)

happyReduce_150 = happySpecReduce_1  69# happyReduction_150
happyReduction_150 happy_x_1
	 =  happyIn73
		 (AbsGarpez.SignOp1
	)

happyReduce_151 = happySpecReduce_1  69# happyReduction_151
happyReduction_151 happy_x_1
	 =  happyIn73
		 (AbsGarpez.SignOp2
	)

happyNewToken action sts stk [] =
	happyDoAction 67# notHappyAtAll action sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = happyDoAction i tk action sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 1#;
	PT _ (TS _ 2) -> cont 2#;
	PT _ (TS _ 3) -> cont 3#;
	PT _ (TS _ 4) -> cont 4#;
	PT _ (TS _ 5) -> cont 5#;
	PT _ (TS _ 6) -> cont 6#;
	PT _ (TS _ 7) -> cont 7#;
	PT _ (TS _ 8) -> cont 8#;
	PT _ (TS _ 9) -> cont 9#;
	PT _ (TS _ 10) -> cont 10#;
	PT _ (TS _ 11) -> cont 11#;
	PT _ (TS _ 12) -> cont 12#;
	PT _ (TS _ 13) -> cont 13#;
	PT _ (TS _ 14) -> cont 14#;
	PT _ (TS _ 15) -> cont 15#;
	PT _ (TS _ 16) -> cont 16#;
	PT _ (TS _ 17) -> cont 17#;
	PT _ (TS _ 18) -> cont 18#;
	PT _ (TS _ 19) -> cont 19#;
	PT _ (TS _ 20) -> cont 20#;
	PT _ (TS _ 21) -> cont 21#;
	PT _ (TS _ 22) -> cont 22#;
	PT _ (TS _ 23) -> cont 23#;
	PT _ (TS _ 24) -> cont 24#;
	PT _ (TS _ 25) -> cont 25#;
	PT _ (TS _ 26) -> cont 26#;
	PT _ (TS _ 27) -> cont 27#;
	PT _ (TS _ 28) -> cont 28#;
	PT _ (TS _ 29) -> cont 29#;
	PT _ (TS _ 30) -> cont 30#;
	PT _ (TS _ 31) -> cont 31#;
	PT _ (TS _ 32) -> cont 32#;
	PT _ (TS _ 33) -> cont 33#;
	PT _ (TS _ 34) -> cont 34#;
	PT _ (TS _ 35) -> cont 35#;
	PT _ (TS _ 36) -> cont 36#;
	PT _ (TS _ 37) -> cont 37#;
	PT _ (TS _ 38) -> cont 38#;
	PT _ (TS _ 39) -> cont 39#;
	PT _ (TS _ 40) -> cont 40#;
	PT _ (TS _ 41) -> cont 41#;
	PT _ (TS _ 42) -> cont 42#;
	PT _ (TS _ 43) -> cont 43#;
	PT _ (TS _ 44) -> cont 44#;
	PT _ (TS _ 45) -> cont 45#;
	PT _ (TS _ 46) -> cont 46#;
	PT _ (TS _ 47) -> cont 47#;
	PT _ (TS _ 48) -> cont 48#;
	PT _ (TS _ 49) -> cont 49#;
	PT _ (T_Id _) -> cont 50#;
	PT _ (T_PBool _) -> cont 51#;
	PT _ (T_PChar _) -> cont 52#;
	PT _ (T_PInt _) -> cont 53#;
	PT _ (T_PFloat _) -> cont 54#;
	PT _ (T_PString _) -> cont 55#;
	PT _ (T_Return _) -> cont 56#;
	PT _ (T_Break _) -> cont 57#;
	PT _ (T_Continue _) -> cont 58#;
	PT _ (T_RChar _) -> cont 59#;
	PT _ (T_RInt _) -> cont 60#;
	PT _ (T_RFloat _) -> cont 61#;
	PT _ (T_RString _) -> cont 62#;
	PT _ (T_WChar _) -> cont 63#;
	PT _ (T_WInt _) -> cont 64#;
	PT _ (T_WFloat _) -> cont 65#;
	PT _ (T_WString _) -> cont 66#;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 67# tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse 0# tks) (\x -> happyReturn (let {(HappyWrap21 x') = happyOut21 x} in x'))

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ id(prToken t) ++ "'"

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $













-- Do not remove this comment. Required to fix CPP parsing when using GCC and a clang-compiled alex.
#if __GLASGOW_HASKELL__ > 706
#define LT(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.<# m)) :: Bool)
#define GTE(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.>=# m)) :: Bool)
#define EQ(n,m) ((Happy_GHC_Exts.tagToEnum# (n Happy_GHC_Exts.==# m)) :: Bool)
#else
#define LT(n,m) (n Happy_GHC_Exts.<# m)
#define GTE(n,m) (n Happy_GHC_Exts.>=# m)
#define EQ(n,m) (n Happy_GHC_Exts.==# m)
#endif



















data Happy_IntList = HappyCons Happy_GHC_Exts.Int# Happy_IntList








































infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is ERROR_TOK, it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept 0# tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
        (happyTcHack j (happyTcHack st)) (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action



happyDoAction i tk st
        = {- nothing -}
          case action of
                0#           -> {- nothing -}
                                     happyFail (happyExpListPerState ((Happy_GHC_Exts.I# (st)) :: Int)) i tk st
                -1#          -> {- nothing -}
                                     happyAccept i tk st
                n | LT(n,(0# :: Happy_GHC_Exts.Int#)) -> {- nothing -}
                                                   (happyReduceArr Happy_Data_Array.! rule) i tk st
                                                   where rule = (Happy_GHC_Exts.I# ((Happy_GHC_Exts.negateInt# ((n Happy_GHC_Exts.+# (1# :: Happy_GHC_Exts.Int#))))))
                n                 -> {- nothing -}
                                     happyShift new_state i tk st
                                     where new_state = (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#))
   where off    = happyAdjustOffset (indexShortOffAddr happyActOffsets st)
         off_i  = (off Happy_GHC_Exts.+# i)
         check  = if GTE(off_i,(0# :: Happy_GHC_Exts.Int#))
                  then EQ(indexShortOffAddr happyCheck off_i, i)
                  else False
         action
          | check     = indexShortOffAddr happyTable off_i
          | otherwise = indexShortOffAddr happyDefActions st




indexShortOffAddr (HappyA# arr) off =
        Happy_GHC_Exts.narrow16Int# i
  where
        i = Happy_GHC_Exts.word2Int# (Happy_GHC_Exts.or# (Happy_GHC_Exts.uncheckedShiftL# high 8#) low)
        high = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr (off' Happy_GHC_Exts.+# 1#)))
        low  = Happy_GHC_Exts.int2Word# (Happy_GHC_Exts.ord# (Happy_GHC_Exts.indexCharOffAddr# arr off'))
        off' = off Happy_GHC_Exts.*# 2#




{-# INLINE happyLt #-}
happyLt x y = LT(x,y)


readArrayBit arr bit =
    Bits.testBit (Happy_GHC_Exts.I# (indexShortOffAddr arr ((unbox_int bit) `Happy_GHC_Exts.iShiftRA#` 4#))) (bit `mod` 16)
  where unbox_int (Happy_GHC_Exts.I# x) = x






data HappyAddr = HappyA# Happy_GHC_Exts.Addr#


-----------------------------------------------------------------------------
-- HappyState data type (not arrays)













-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state 0# tk st sts stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--     trace "shifting the error token" $
     happyDoAction i tk new_state (HappyCons (st) (sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state (HappyCons (st) (sts)) ((happyInTok (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_0 nt fn j tk st@((action)) sts stk
     = happyGoto nt j tk st (HappyCons (st) (sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@((HappyCons (st@(action)) (_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_2 nt fn j tk _ (HappyCons (_) (sts@((HappyCons (st@(action)) (_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happySpecReduce_3 nt fn j tk _ (HappyCons (_) ((HappyCons (_) (sts@((HappyCons (st@(action)) (_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (happyGoto nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) sts of
         sts1@((HappyCons (st1@(action)) (_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (happyGoto nt j tk st1 sts1 r)

happyMonadReduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> happyGoto nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn 0# tk st sts stk
     = happyFail [] 0# tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k (HappyCons (st) (sts)) of
        sts1@((HappyCons (st1@(action)) (_))) ->
         let drop_stk = happyDropStk k stk

             off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st1)
             off_i = (off Happy_GHC_Exts.+# nt)
             new_state = indexShortOffAddr happyTable off_i




          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop 0# l = l
happyDrop n (HappyCons (_) (t)) = happyDrop (n Happy_GHC_Exts.-# (1# :: Happy_GHC_Exts.Int#)) t

happyDropStk 0# l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n Happy_GHC_Exts.-# (1#::Happy_GHC_Exts.Int#)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction


happyGoto nt j tk st = 
   {- nothing -}
   happyDoAction j tk new_state
   where off = happyAdjustOffset (indexShortOffAddr happyGotoOffsets st)
         off_i = (off Happy_GHC_Exts.+# nt)
         new_state = indexShortOffAddr happyTable off_i




-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist 0# tk old_st _ stk@(x `HappyStk` _) =
     let i = (case Happy_GHC_Exts.unsafeCoerce# x of { (Happy_GHC_Exts.I# (i)) -> i }) in
--      trace "failing" $ 
        happyError_ explist i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  ERROR_TOK tk old_st CONS(HAPPYSTATE(action),sts) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        DO_ACTION(action,ERROR_TOK,tk,sts,(saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail explist i tk (action) sts stk =
--      trace "entering error recovery" $
        happyDoAction 0# tk action sts ((Happy_GHC_Exts.unsafeCoerce# (Happy_GHC_Exts.I# (i))) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions


happyTcHack :: Happy_GHC_Exts.Int# -> a -> a
happyTcHack x y = y
{-# INLINE happyTcHack #-}


-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.


{-# NOINLINE happyDoAction #-}
{-# NOINLINE happyTable #-}
{-# NOINLINE happyCheck #-}
{-# NOINLINE happyActOffsets #-}
{-# NOINLINE happyGotoOffsets #-}
{-# NOINLINE happyDefActions #-}

{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
