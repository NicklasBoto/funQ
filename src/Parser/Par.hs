{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module Parser.Par
  ( happyError
  , myLexer
  , pProgram
  , pTerm3
  , pTerm2
  , pTerm1
  , pTerm
  , pLetVar
  , pListLetVar
  , pTup
  , pListTerm
  , pBit
  , pFunDec
  , pListFunDec
  , pFunction
  , pArg
  , pListArg
  , pType2
  , pType1
  , pType
  , pGate
  ) where

import Prelude

import qualified Parser.Abs
import Parser.Lex
import qualified Data.Array as Happy_Data_Array
import qualified Data.Bits as Bits
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.12

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn22 (Parser.Abs.FunVar)
	| HappyAbsSyn23 (Parser.Abs.Var)
	| HappyAbsSyn24 (Parser.Abs.GateIdent)
	| HappyAbsSyn25 (Parser.Abs.Lambda)
	| HappyAbsSyn26 (Parser.Abs.Program)
	| HappyAbsSyn27 (Parser.Abs.Term)
	| HappyAbsSyn31 (Parser.Abs.LetVar)
	| HappyAbsSyn32 ([Parser.Abs.LetVar])
	| HappyAbsSyn33 (Parser.Abs.Tup)
	| HappyAbsSyn34 ([Parser.Abs.Term])
	| HappyAbsSyn35 (Parser.Abs.Bit)
	| HappyAbsSyn36 (Parser.Abs.FunDec)
	| HappyAbsSyn37 ([Parser.Abs.FunDec])
	| HappyAbsSyn38 (Parser.Abs.Function)
	| HappyAbsSyn39 (Parser.Abs.Arg)
	| HappyAbsSyn40 ([Parser.Abs.Arg])
	| HappyAbsSyn41 (Parser.Abs.Type)
	| HappyAbsSyn44 (Parser.Abs.Gate)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134,
 action_135,
 action_136,
 action_137,
 action_138,
 action_139,
 action_140,
 action_141 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82,
 happyReduce_83,
 happyReduce_84,
 happyReduce_85,
 happyReduce_86 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyExpList :: Happy_Data_Array.Array Int Int
happyExpList = Happy_Data_Array.listArray (0,492) ([0,0,0,0,0,64,0,0,50432,65528,4091,12,0,0,50728,57343,24703,0,0,16384,65073,65279,1835,0,0,35328,65521,24567,57,0,0,0,0,16384,0,0,0,0,0,512,0,0,1024,0,0,0,0,0,6304,32767,38399,3,0,0,192,0,0,0,0,0,0,0,16,0,0,0,0,32768,0,0,0,0,0,2048,0,0,0,0,0,64,0,0,0,0,0,2,0,0,4102,4096,4098,0,0,12288,128,4224,128,0,0,384,4,132,4,0,0,49152,57343,16511,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,4096,2,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,768,8,264,8,0,0,16408,16384,16392,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,0,0,0,0,8204,8192,8196,0,0,0,0,0,128,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32,0,0,0,0,0,0,0,0,61834,63487,6175,0,0,0,0,0,0,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,65073,65279,1835,0,0,0,0,0,0,0,0,35920,49151,49407,0,0,32768,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,63685,64511,7343,0,0,0,0,0,0,0,0,512,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,40960,65304,65407,385,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,4,0,0,0,0,0,0,0,16,0,0,6304,32767,41471,1,0,0,10,0,0,0,0,10240,65478,32735,229,0,0,2048,0,0,0,0,0,0,0,0,0,0,0,0,0,64,0,0,32768,0,0,0,0,0,0,0,0,0,0,16384,0,0,0,0,0,0,0,0,0,0,0,8204,8192,8196,0,0,24576,256,8448,256,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,5120,65507,49135,114,0,0,0,0,0,0,0,0,63685,64511,7343,0,0,0,0,0,0,0,0,0,0,0,0,0,0,61834,63487,14687,0,0,20480,65420,65471,458,0,0,1024,0,0,0,0,0,0,0,0,0,0,0,0,0,128,0,0,0,0,4096,0,0,0,16,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32768,64610,65023,3671,0,0,2048,0,0,0,0,0,8192,0,0,0,0,0,0,0,0,0,0,10240,65478,32735,229,0,0,0,0,4096,0,0,0,61834,63487,14687,0,0,0,0,0,0,0
	])

{-# NOINLINE happyExpListPerState #-}
happyExpListPerState st =
    token_strs_expected
  where token_strs = ["error","%dummy","%start_pProgram","%start_pTerm3","%start_pTerm2","%start_pTerm1","%start_pTerm","%start_pLetVar","%start_pListLetVar","%start_pTup","%start_pListTerm","%start_pBit","%start_pFunDec","%start_pListFunDec","%start_pFunction","%start_pArg","%start_pListArg","%start_pType2","%start_pType1","%start_pType","%start_pGate","FunVar","Var","GateIdent","Lambda","Program","Term3","Term2","Term1","Term","LetVar","ListLetVar","Tup","ListTerm","Bit","FunDec","ListFunDec","Function","Arg","ListArg","Type2","Type1","Type","Gate","'!'","'('","')'","'*'","','","'-o'","'.'","'0'","'1'","'='","'><'","'Bit'","'CNOT'","'CR2'","'CR2D'","'CR3'","'CR3D'","'CR4'","'CR4D'","'CR5'","'CR5D'","'CR8'","'CR8D'","'CT'","'FREDKIN'","'H'","'I'","'QBit'","'QFT'","'QFTI'","'S'","'SWAP'","'T'","'TOFFOLI'","'X'","'Y'","'Z'","'else'","'if'","'in'","'let'","'then'","L_FunVar","L_Var","L_GateIdent","L_Lambda","%eof"]
        bit_start = st * 91
        bit_end = (st + 1) * 91
        read_bit = readArrayBit happyExpList
        bits = map read_bit [bit_start..bit_end - 1]
        bits_indexed = zip bits [0..90]
        token_strs_expected = concatMap f bits_indexed
        f (False, _) = []
        f (True, nr) = [token_strs !! nr]

action_0 (87) = happyShift action_20
action_0 (22) = happyGoto action_66
action_0 (26) = happyGoto action_98
action_0 (36) = happyGoto action_67
action_0 (37) = happyGoto action_99
action_0 _ = happyReduce_46

action_1 (46) = happyShift action_83
action_1 (48) = happyShift action_84
action_1 (52) = happyShift action_71
action_1 (53) = happyShift action_72
action_1 (57) = happyShift action_23
action_1 (58) = happyShift action_24
action_1 (59) = happyShift action_25
action_1 (60) = happyShift action_26
action_1 (61) = happyShift action_27
action_1 (62) = happyShift action_28
action_1 (63) = happyShift action_29
action_1 (64) = happyShift action_30
action_1 (65) = happyShift action_31
action_1 (66) = happyShift action_32
action_1 (67) = happyShift action_33
action_1 (68) = happyShift action_34
action_1 (69) = happyShift action_35
action_1 (70) = happyShift action_36
action_1 (71) = happyShift action_37
action_1 (73) = happyShift action_38
action_1 (74) = happyShift action_39
action_1 (75) = happyShift action_40
action_1 (76) = happyShift action_41
action_1 (77) = happyShift action_42
action_1 (78) = happyShift action_43
action_1 (79) = happyShift action_44
action_1 (80) = happyShift action_45
action_1 (81) = happyShift action_46
action_1 (88) = happyShift action_57
action_1 (89) = happyShift action_47
action_1 (23) = happyGoto action_73
action_1 (24) = happyGoto action_21
action_1 (27) = happyGoto action_97
action_1 (33) = happyGoto action_79
action_1 (35) = happyGoto action_81
action_1 (44) = happyGoto action_82
action_1 _ = happyFail (happyExpListPerState 1)

action_2 (46) = happyShift action_83
action_2 (48) = happyShift action_84
action_2 (52) = happyShift action_71
action_2 (53) = happyShift action_72
action_2 (57) = happyShift action_23
action_2 (58) = happyShift action_24
action_2 (59) = happyShift action_25
action_2 (60) = happyShift action_26
action_2 (61) = happyShift action_27
action_2 (62) = happyShift action_28
action_2 (63) = happyShift action_29
action_2 (64) = happyShift action_30
action_2 (65) = happyShift action_31
action_2 (66) = happyShift action_32
action_2 (67) = happyShift action_33
action_2 (68) = happyShift action_34
action_2 (69) = happyShift action_35
action_2 (70) = happyShift action_36
action_2 (71) = happyShift action_37
action_2 (73) = happyShift action_38
action_2 (74) = happyShift action_39
action_2 (75) = happyShift action_40
action_2 (76) = happyShift action_41
action_2 (77) = happyShift action_42
action_2 (78) = happyShift action_43
action_2 (79) = happyShift action_44
action_2 (80) = happyShift action_45
action_2 (81) = happyShift action_46
action_2 (88) = happyShift action_57
action_2 (89) = happyShift action_47
action_2 (23) = happyGoto action_73
action_2 (24) = happyGoto action_21
action_2 (27) = happyGoto action_75
action_2 (28) = happyGoto action_96
action_2 (33) = happyGoto action_79
action_2 (35) = happyGoto action_81
action_2 (44) = happyGoto action_82
action_2 _ = happyFail (happyExpListPerState 2)

action_3 (46) = happyShift action_83
action_3 (48) = happyShift action_84
action_3 (52) = happyShift action_71
action_3 (53) = happyShift action_72
action_3 (57) = happyShift action_23
action_3 (58) = happyShift action_24
action_3 (59) = happyShift action_25
action_3 (60) = happyShift action_26
action_3 (61) = happyShift action_27
action_3 (62) = happyShift action_28
action_3 (63) = happyShift action_29
action_3 (64) = happyShift action_30
action_3 (65) = happyShift action_31
action_3 (66) = happyShift action_32
action_3 (67) = happyShift action_33
action_3 (68) = happyShift action_34
action_3 (69) = happyShift action_35
action_3 (70) = happyShift action_36
action_3 (71) = happyShift action_37
action_3 (73) = happyShift action_38
action_3 (74) = happyShift action_39
action_3 (75) = happyShift action_40
action_3 (76) = happyShift action_41
action_3 (77) = happyShift action_42
action_3 (78) = happyShift action_43
action_3 (79) = happyShift action_44
action_3 (80) = happyShift action_45
action_3 (81) = happyShift action_46
action_3 (83) = happyShift action_85
action_3 (85) = happyShift action_86
action_3 (88) = happyShift action_57
action_3 (89) = happyShift action_47
action_3 (90) = happyShift action_87
action_3 (23) = happyGoto action_73
action_3 (24) = happyGoto action_21
action_3 (25) = happyGoto action_74
action_3 (27) = happyGoto action_75
action_3 (28) = happyGoto action_76
action_3 (29) = happyGoto action_95
action_3 (33) = happyGoto action_79
action_3 (35) = happyGoto action_81
action_3 (44) = happyGoto action_82
action_3 _ = happyFail (happyExpListPerState 3)

action_4 (46) = happyShift action_83
action_4 (48) = happyShift action_84
action_4 (52) = happyShift action_71
action_4 (53) = happyShift action_72
action_4 (57) = happyShift action_23
action_4 (58) = happyShift action_24
action_4 (59) = happyShift action_25
action_4 (60) = happyShift action_26
action_4 (61) = happyShift action_27
action_4 (62) = happyShift action_28
action_4 (63) = happyShift action_29
action_4 (64) = happyShift action_30
action_4 (65) = happyShift action_31
action_4 (66) = happyShift action_32
action_4 (67) = happyShift action_33
action_4 (68) = happyShift action_34
action_4 (69) = happyShift action_35
action_4 (70) = happyShift action_36
action_4 (71) = happyShift action_37
action_4 (73) = happyShift action_38
action_4 (74) = happyShift action_39
action_4 (75) = happyShift action_40
action_4 (76) = happyShift action_41
action_4 (77) = happyShift action_42
action_4 (78) = happyShift action_43
action_4 (79) = happyShift action_44
action_4 (80) = happyShift action_45
action_4 (81) = happyShift action_46
action_4 (83) = happyShift action_85
action_4 (85) = happyShift action_86
action_4 (88) = happyShift action_57
action_4 (89) = happyShift action_47
action_4 (90) = happyShift action_87
action_4 (23) = happyGoto action_73
action_4 (24) = happyGoto action_21
action_4 (25) = happyGoto action_74
action_4 (27) = happyGoto action_75
action_4 (28) = happyGoto action_76
action_4 (29) = happyGoto action_77
action_4 (30) = happyGoto action_94
action_4 (33) = happyGoto action_79
action_4 (35) = happyGoto action_81
action_4 (44) = happyGoto action_82
action_4 _ = happyFail (happyExpListPerState 4)

action_5 (88) = happyShift action_57
action_5 (23) = happyGoto action_90
action_5 (31) = happyGoto action_93
action_5 _ = happyFail (happyExpListPerState 5)

action_6 (88) = happyShift action_57
action_6 (23) = happyGoto action_90
action_6 (31) = happyGoto action_91
action_6 (32) = happyGoto action_92
action_6 _ = happyFail (happyExpListPerState 6)

action_7 (46) = happyShift action_89
action_7 (33) = happyGoto action_88
action_7 _ = happyFail (happyExpListPerState 7)

action_8 (46) = happyShift action_83
action_8 (48) = happyShift action_84
action_8 (52) = happyShift action_71
action_8 (53) = happyShift action_72
action_8 (57) = happyShift action_23
action_8 (58) = happyShift action_24
action_8 (59) = happyShift action_25
action_8 (60) = happyShift action_26
action_8 (61) = happyShift action_27
action_8 (62) = happyShift action_28
action_8 (63) = happyShift action_29
action_8 (64) = happyShift action_30
action_8 (65) = happyShift action_31
action_8 (66) = happyShift action_32
action_8 (67) = happyShift action_33
action_8 (68) = happyShift action_34
action_8 (69) = happyShift action_35
action_8 (70) = happyShift action_36
action_8 (71) = happyShift action_37
action_8 (73) = happyShift action_38
action_8 (74) = happyShift action_39
action_8 (75) = happyShift action_40
action_8 (76) = happyShift action_41
action_8 (77) = happyShift action_42
action_8 (78) = happyShift action_43
action_8 (79) = happyShift action_44
action_8 (80) = happyShift action_45
action_8 (81) = happyShift action_46
action_8 (83) = happyShift action_85
action_8 (85) = happyShift action_86
action_8 (88) = happyShift action_57
action_8 (89) = happyShift action_47
action_8 (90) = happyShift action_87
action_8 (23) = happyGoto action_73
action_8 (24) = happyGoto action_21
action_8 (25) = happyGoto action_74
action_8 (27) = happyGoto action_75
action_8 (28) = happyGoto action_76
action_8 (29) = happyGoto action_77
action_8 (30) = happyGoto action_78
action_8 (33) = happyGoto action_79
action_8 (34) = happyGoto action_80
action_8 (35) = happyGoto action_81
action_8 (44) = happyGoto action_82
action_8 _ = happyFail (happyExpListPerState 8)

action_9 (52) = happyShift action_71
action_9 (53) = happyShift action_72
action_9 (35) = happyGoto action_70
action_9 _ = happyFail (happyExpListPerState 9)

action_10 (87) = happyShift action_20
action_10 (22) = happyGoto action_66
action_10 (36) = happyGoto action_69
action_10 _ = happyFail (happyExpListPerState 10)

action_11 (87) = happyShift action_20
action_11 (22) = happyGoto action_66
action_11 (36) = happyGoto action_67
action_11 (37) = happyGoto action_68
action_11 _ = happyReduce_46

action_12 (88) = happyShift action_57
action_12 (23) = happyGoto action_64
action_12 (38) = happyGoto action_65
action_12 _ = happyFail (happyExpListPerState 12)

action_13 (88) = happyShift action_57
action_13 (23) = happyGoto action_60
action_13 (39) = happyGoto action_63
action_13 _ = happyFail (happyExpListPerState 13)

action_14 (88) = happyShift action_57
action_14 (23) = happyGoto action_60
action_14 (39) = happyGoto action_61
action_14 (40) = happyGoto action_62
action_14 _ = happyReduce_50

action_15 (45) = happyShift action_52
action_15 (46) = happyShift action_53
action_15 (56) = happyShift action_54
action_15 (72) = happyShift action_55
action_15 (77) = happyShift action_56
action_15 (88) = happyShift action_57
action_15 (23) = happyGoto action_48
action_15 (41) = happyGoto action_59
action_15 _ = happyFail (happyExpListPerState 15)

action_16 (45) = happyShift action_52
action_16 (46) = happyShift action_53
action_16 (56) = happyShift action_54
action_16 (72) = happyShift action_55
action_16 (77) = happyShift action_56
action_16 (88) = happyShift action_57
action_16 (23) = happyGoto action_48
action_16 (41) = happyGoto action_49
action_16 (42) = happyGoto action_58
action_16 _ = happyFail (happyExpListPerState 16)

action_17 (45) = happyShift action_52
action_17 (46) = happyShift action_53
action_17 (56) = happyShift action_54
action_17 (72) = happyShift action_55
action_17 (77) = happyShift action_56
action_17 (88) = happyShift action_57
action_17 (23) = happyGoto action_48
action_17 (41) = happyGoto action_49
action_17 (42) = happyGoto action_50
action_17 (43) = happyGoto action_51
action_17 _ = happyFail (happyExpListPerState 17)

action_18 (57) = happyShift action_23
action_18 (58) = happyShift action_24
action_18 (59) = happyShift action_25
action_18 (60) = happyShift action_26
action_18 (61) = happyShift action_27
action_18 (62) = happyShift action_28
action_18 (63) = happyShift action_29
action_18 (64) = happyShift action_30
action_18 (65) = happyShift action_31
action_18 (66) = happyShift action_32
action_18 (67) = happyShift action_33
action_18 (68) = happyShift action_34
action_18 (69) = happyShift action_35
action_18 (70) = happyShift action_36
action_18 (71) = happyShift action_37
action_18 (73) = happyShift action_38
action_18 (74) = happyShift action_39
action_18 (75) = happyShift action_40
action_18 (76) = happyShift action_41
action_18 (77) = happyShift action_42
action_18 (78) = happyShift action_43
action_18 (79) = happyShift action_44
action_18 (80) = happyShift action_45
action_18 (81) = happyShift action_46
action_18 (89) = happyShift action_47
action_18 (24) = happyGoto action_21
action_18 (44) = happyGoto action_22
action_18 _ = happyFail (happyExpListPerState 18)

action_19 (87) = happyShift action_20
action_19 _ = happyFail (happyExpListPerState 19)

action_20 _ = happyReduce_19

action_21 _ = happyReduce_86

action_22 (91) = happyAccept
action_22 _ = happyFail (happyExpListPerState 22)

action_23 _ = happyReduce_69

action_24 _ = happyReduce_76

action_25 _ = happyReduce_77

action_26 _ = happyReduce_78

action_27 _ = happyReduce_79

action_28 _ = happyReduce_80

action_29 _ = happyReduce_81

action_30 _ = happyReduce_82

action_31 _ = happyReduce_83

action_32 _ = happyReduce_84

action_33 _ = happyReduce_85

action_34 _ = happyReduce_75

action_35 _ = happyReduce_72

action_36 _ = happyReduce_62

action_37 _ = happyReduce_66

action_38 _ = happyReduce_73

action_39 _ = happyReduce_74

action_40 _ = happyReduce_67

action_41 _ = happyReduce_71

action_42 _ = happyReduce_68

action_43 _ = happyReduce_70

action_44 _ = happyReduce_63

action_45 _ = happyReduce_64

action_46 _ = happyReduce_65

action_47 _ = happyReduce_21

action_48 _ = happyReduce_52

action_49 (50) = happyShift action_114
action_49 (55) = happyShift action_115
action_49 _ = happyReduce_60

action_50 _ = happyReduce_61

action_51 (91) = happyAccept
action_51 _ = happyFail (happyExpListPerState 51)

action_52 (45) = happyShift action_52
action_52 (46) = happyShift action_53
action_52 (56) = happyShift action_54
action_52 (72) = happyShift action_55
action_52 (77) = happyShift action_56
action_52 (88) = happyShift action_57
action_52 (23) = happyGoto action_48
action_52 (41) = happyGoto action_113
action_52 _ = happyFail (happyExpListPerState 52)

action_53 (45) = happyShift action_52
action_53 (46) = happyShift action_53
action_53 (56) = happyShift action_54
action_53 (72) = happyShift action_55
action_53 (77) = happyShift action_56
action_53 (88) = happyShift action_57
action_53 (23) = happyGoto action_48
action_53 (41) = happyGoto action_49
action_53 (42) = happyGoto action_50
action_53 (43) = happyGoto action_112
action_53 _ = happyFail (happyExpListPerState 53)

action_54 _ = happyReduce_53

action_55 _ = happyReduce_54

action_56 _ = happyReduce_55

action_57 _ = happyReduce_20

action_58 (91) = happyAccept
action_58 _ = happyFail (happyExpListPerState 58)

action_59 (91) = happyAccept
action_59 _ = happyFail (happyExpListPerState 59)

action_60 _ = happyReduce_49

action_61 (88) = happyShift action_57
action_61 (23) = happyGoto action_60
action_61 (39) = happyGoto action_61
action_61 (40) = happyGoto action_111
action_61 _ = happyReduce_50

action_62 (91) = happyAccept
action_62 _ = happyFail (happyExpListPerState 62)

action_63 (91) = happyAccept
action_63 _ = happyFail (happyExpListPerState 63)

action_64 (88) = happyShift action_57
action_64 (23) = happyGoto action_60
action_64 (39) = happyGoto action_61
action_64 (40) = happyGoto action_110
action_64 _ = happyReduce_50

action_65 (91) = happyAccept
action_65 _ = happyFail (happyExpListPerState 65)

action_66 (45) = happyShift action_52
action_66 (46) = happyShift action_53
action_66 (56) = happyShift action_54
action_66 (72) = happyShift action_55
action_66 (77) = happyShift action_56
action_66 (88) = happyShift action_57
action_66 (23) = happyGoto action_48
action_66 (41) = happyGoto action_49
action_66 (42) = happyGoto action_50
action_66 (43) = happyGoto action_109
action_66 _ = happyFail (happyExpListPerState 66)

action_67 (87) = happyShift action_20
action_67 (22) = happyGoto action_66
action_67 (36) = happyGoto action_67
action_67 (37) = happyGoto action_108
action_67 _ = happyReduce_46

action_68 (91) = happyAccept
action_68 _ = happyFail (happyExpListPerState 68)

action_69 (91) = happyAccept
action_69 _ = happyFail (happyExpListPerState 69)

action_70 (91) = happyAccept
action_70 _ = happyFail (happyExpListPerState 70)

action_71 _ = happyReduce_43

action_72 _ = happyReduce_44

action_73 _ = happyReduce_24

action_74 (88) = happyShift action_57
action_74 (23) = happyGoto action_107
action_74 _ = happyFail (happyExpListPerState 74)

action_75 _ = happyReduce_31

action_76 (46) = happyShift action_83
action_76 (48) = happyShift action_84
action_76 (52) = happyShift action_71
action_76 (53) = happyShift action_72
action_76 (57) = happyShift action_23
action_76 (58) = happyShift action_24
action_76 (59) = happyShift action_25
action_76 (60) = happyShift action_26
action_76 (61) = happyShift action_27
action_76 (62) = happyShift action_28
action_76 (63) = happyShift action_29
action_76 (64) = happyShift action_30
action_76 (65) = happyShift action_31
action_76 (66) = happyShift action_32
action_76 (67) = happyShift action_33
action_76 (68) = happyShift action_34
action_76 (69) = happyShift action_35
action_76 (70) = happyShift action_36
action_76 (71) = happyShift action_37
action_76 (73) = happyShift action_38
action_76 (74) = happyShift action_39
action_76 (75) = happyShift action_40
action_76 (76) = happyShift action_41
action_76 (77) = happyShift action_42
action_76 (78) = happyShift action_43
action_76 (79) = happyShift action_44
action_76 (80) = happyShift action_45
action_76 (81) = happyShift action_46
action_76 (88) = happyShift action_57
action_76 (89) = happyShift action_47
action_76 (23) = happyGoto action_73
action_76 (24) = happyGoto action_21
action_76 (27) = happyGoto action_100
action_76 (33) = happyGoto action_79
action_76 (35) = happyGoto action_81
action_76 (44) = happyGoto action_82
action_76 _ = happyReduce_35

action_77 _ = happyReduce_36

action_78 (49) = happyShift action_106
action_78 _ = happyReduce_41

action_79 _ = happyReduce_27

action_80 (91) = happyAccept
action_80 _ = happyFail (happyExpListPerState 80)

action_81 _ = happyReduce_25

action_82 _ = happyReduce_26

action_83 (46) = happyShift action_83
action_83 (48) = happyShift action_84
action_83 (52) = happyShift action_71
action_83 (53) = happyShift action_72
action_83 (57) = happyShift action_23
action_83 (58) = happyShift action_24
action_83 (59) = happyShift action_25
action_83 (60) = happyShift action_26
action_83 (61) = happyShift action_27
action_83 (62) = happyShift action_28
action_83 (63) = happyShift action_29
action_83 (64) = happyShift action_30
action_83 (65) = happyShift action_31
action_83 (66) = happyShift action_32
action_83 (67) = happyShift action_33
action_83 (68) = happyShift action_34
action_83 (69) = happyShift action_35
action_83 (70) = happyShift action_36
action_83 (71) = happyShift action_37
action_83 (73) = happyShift action_38
action_83 (74) = happyShift action_39
action_83 (75) = happyShift action_40
action_83 (76) = happyShift action_41
action_83 (77) = happyShift action_42
action_83 (78) = happyShift action_43
action_83 (79) = happyShift action_44
action_83 (80) = happyShift action_45
action_83 (81) = happyShift action_46
action_83 (83) = happyShift action_85
action_83 (85) = happyShift action_86
action_83 (88) = happyShift action_57
action_83 (89) = happyShift action_47
action_83 (90) = happyShift action_87
action_83 (23) = happyGoto action_73
action_83 (24) = happyGoto action_21
action_83 (25) = happyGoto action_74
action_83 (27) = happyGoto action_75
action_83 (28) = happyGoto action_76
action_83 (29) = happyGoto action_77
action_83 (30) = happyGoto action_105
action_83 (33) = happyGoto action_79
action_83 (35) = happyGoto action_81
action_83 (44) = happyGoto action_82
action_83 _ = happyFail (happyExpListPerState 83)

action_84 _ = happyReduce_28

action_85 (46) = happyShift action_83
action_85 (48) = happyShift action_84
action_85 (52) = happyShift action_71
action_85 (53) = happyShift action_72
action_85 (57) = happyShift action_23
action_85 (58) = happyShift action_24
action_85 (59) = happyShift action_25
action_85 (60) = happyShift action_26
action_85 (61) = happyShift action_27
action_85 (62) = happyShift action_28
action_85 (63) = happyShift action_29
action_85 (64) = happyShift action_30
action_85 (65) = happyShift action_31
action_85 (66) = happyShift action_32
action_85 (67) = happyShift action_33
action_85 (68) = happyShift action_34
action_85 (69) = happyShift action_35
action_85 (70) = happyShift action_36
action_85 (71) = happyShift action_37
action_85 (73) = happyShift action_38
action_85 (74) = happyShift action_39
action_85 (75) = happyShift action_40
action_85 (76) = happyShift action_41
action_85 (77) = happyShift action_42
action_85 (78) = happyShift action_43
action_85 (79) = happyShift action_44
action_85 (80) = happyShift action_45
action_85 (81) = happyShift action_46
action_85 (88) = happyShift action_57
action_85 (89) = happyShift action_47
action_85 (23) = happyGoto action_73
action_85 (24) = happyGoto action_21
action_85 (27) = happyGoto action_75
action_85 (28) = happyGoto action_104
action_85 (33) = happyGoto action_79
action_85 (35) = happyGoto action_81
action_85 (44) = happyGoto action_82
action_85 _ = happyFail (happyExpListPerState 85)

action_86 (46) = happyShift action_103
action_86 _ = happyFail (happyExpListPerState 86)

action_87 _ = happyReduce_22

action_88 (91) = happyAccept
action_88 _ = happyFail (happyExpListPerState 88)

action_89 (46) = happyShift action_83
action_89 (48) = happyShift action_84
action_89 (52) = happyShift action_71
action_89 (53) = happyShift action_72
action_89 (57) = happyShift action_23
action_89 (58) = happyShift action_24
action_89 (59) = happyShift action_25
action_89 (60) = happyShift action_26
action_89 (61) = happyShift action_27
action_89 (62) = happyShift action_28
action_89 (63) = happyShift action_29
action_89 (64) = happyShift action_30
action_89 (65) = happyShift action_31
action_89 (66) = happyShift action_32
action_89 (67) = happyShift action_33
action_89 (68) = happyShift action_34
action_89 (69) = happyShift action_35
action_89 (70) = happyShift action_36
action_89 (71) = happyShift action_37
action_89 (73) = happyShift action_38
action_89 (74) = happyShift action_39
action_89 (75) = happyShift action_40
action_89 (76) = happyShift action_41
action_89 (77) = happyShift action_42
action_89 (78) = happyShift action_43
action_89 (79) = happyShift action_44
action_89 (80) = happyShift action_45
action_89 (81) = happyShift action_46
action_89 (83) = happyShift action_85
action_89 (85) = happyShift action_86
action_89 (88) = happyShift action_57
action_89 (89) = happyShift action_47
action_89 (90) = happyShift action_87
action_89 (23) = happyGoto action_73
action_89 (24) = happyGoto action_21
action_89 (25) = happyGoto action_74
action_89 (27) = happyGoto action_75
action_89 (28) = happyGoto action_76
action_89 (29) = happyGoto action_77
action_89 (30) = happyGoto action_102
action_89 (33) = happyGoto action_79
action_89 (35) = happyGoto action_81
action_89 (44) = happyGoto action_82
action_89 _ = happyFail (happyExpListPerState 89)

action_90 _ = happyReduce_37

action_91 (49) = happyShift action_101
action_91 _ = happyReduce_38

action_92 (91) = happyAccept
action_92 _ = happyFail (happyExpListPerState 92)

action_93 (91) = happyAccept
action_93 _ = happyFail (happyExpListPerState 93)

action_94 (91) = happyAccept
action_94 _ = happyFail (happyExpListPerState 94)

action_95 (91) = happyAccept
action_95 _ = happyFail (happyExpListPerState 95)

action_96 (46) = happyShift action_83
action_96 (48) = happyShift action_84
action_96 (52) = happyShift action_71
action_96 (53) = happyShift action_72
action_96 (57) = happyShift action_23
action_96 (58) = happyShift action_24
action_96 (59) = happyShift action_25
action_96 (60) = happyShift action_26
action_96 (61) = happyShift action_27
action_96 (62) = happyShift action_28
action_96 (63) = happyShift action_29
action_96 (64) = happyShift action_30
action_96 (65) = happyShift action_31
action_96 (66) = happyShift action_32
action_96 (67) = happyShift action_33
action_96 (68) = happyShift action_34
action_96 (69) = happyShift action_35
action_96 (70) = happyShift action_36
action_96 (71) = happyShift action_37
action_96 (73) = happyShift action_38
action_96 (74) = happyShift action_39
action_96 (75) = happyShift action_40
action_96 (76) = happyShift action_41
action_96 (77) = happyShift action_42
action_96 (78) = happyShift action_43
action_96 (79) = happyShift action_44
action_96 (80) = happyShift action_45
action_96 (81) = happyShift action_46
action_96 (88) = happyShift action_57
action_96 (89) = happyShift action_47
action_96 (91) = happyAccept
action_96 (23) = happyGoto action_73
action_96 (24) = happyGoto action_21
action_96 (27) = happyGoto action_100
action_96 (33) = happyGoto action_79
action_96 (35) = happyGoto action_81
action_96 (44) = happyGoto action_82
action_96 _ = happyFail (happyExpListPerState 96)

action_97 (91) = happyAccept
action_97 _ = happyFail (happyExpListPerState 97)

action_98 (91) = happyAccept
action_98 _ = happyFail (happyExpListPerState 98)

action_99 _ = happyReduce_23

action_100 _ = happyReduce_30

action_101 (88) = happyShift action_57
action_101 (23) = happyGoto action_90
action_101 (31) = happyGoto action_91
action_101 (32) = happyGoto action_127
action_101 _ = happyFail (happyExpListPerState 101)

action_102 (49) = happyShift action_124
action_102 _ = happyFail (happyExpListPerState 102)

action_103 (88) = happyShift action_57
action_103 (23) = happyGoto action_90
action_103 (31) = happyGoto action_126
action_103 _ = happyFail (happyExpListPerState 103)

action_104 (46) = happyShift action_83
action_104 (48) = happyShift action_84
action_104 (52) = happyShift action_71
action_104 (53) = happyShift action_72
action_104 (57) = happyShift action_23
action_104 (58) = happyShift action_24
action_104 (59) = happyShift action_25
action_104 (60) = happyShift action_26
action_104 (61) = happyShift action_27
action_104 (62) = happyShift action_28
action_104 (63) = happyShift action_29
action_104 (64) = happyShift action_30
action_104 (65) = happyShift action_31
action_104 (66) = happyShift action_32
action_104 (67) = happyShift action_33
action_104 (68) = happyShift action_34
action_104 (69) = happyShift action_35
action_104 (70) = happyShift action_36
action_104 (71) = happyShift action_37
action_104 (73) = happyShift action_38
action_104 (74) = happyShift action_39
action_104 (75) = happyShift action_40
action_104 (76) = happyShift action_41
action_104 (77) = happyShift action_42
action_104 (78) = happyShift action_43
action_104 (79) = happyShift action_44
action_104 (80) = happyShift action_45
action_104 (81) = happyShift action_46
action_104 (86) = happyShift action_125
action_104 (88) = happyShift action_57
action_104 (89) = happyShift action_47
action_104 (23) = happyGoto action_73
action_104 (24) = happyGoto action_21
action_104 (27) = happyGoto action_100
action_104 (33) = happyGoto action_79
action_104 (35) = happyGoto action_81
action_104 (44) = happyGoto action_82
action_104 _ = happyFail (happyExpListPerState 104)

action_105 (47) = happyShift action_123
action_105 (49) = happyShift action_124
action_105 _ = happyFail (happyExpListPerState 105)

action_106 (46) = happyShift action_83
action_106 (48) = happyShift action_84
action_106 (52) = happyShift action_71
action_106 (53) = happyShift action_72
action_106 (57) = happyShift action_23
action_106 (58) = happyShift action_24
action_106 (59) = happyShift action_25
action_106 (60) = happyShift action_26
action_106 (61) = happyShift action_27
action_106 (62) = happyShift action_28
action_106 (63) = happyShift action_29
action_106 (64) = happyShift action_30
action_106 (65) = happyShift action_31
action_106 (66) = happyShift action_32
action_106 (67) = happyShift action_33
action_106 (68) = happyShift action_34
action_106 (69) = happyShift action_35
action_106 (70) = happyShift action_36
action_106 (71) = happyShift action_37
action_106 (73) = happyShift action_38
action_106 (74) = happyShift action_39
action_106 (75) = happyShift action_40
action_106 (76) = happyShift action_41
action_106 (77) = happyShift action_42
action_106 (78) = happyShift action_43
action_106 (79) = happyShift action_44
action_106 (80) = happyShift action_45
action_106 (81) = happyShift action_46
action_106 (83) = happyShift action_85
action_106 (85) = happyShift action_86
action_106 (88) = happyShift action_57
action_106 (89) = happyShift action_47
action_106 (90) = happyShift action_87
action_106 (23) = happyGoto action_73
action_106 (24) = happyGoto action_21
action_106 (25) = happyGoto action_74
action_106 (27) = happyGoto action_75
action_106 (28) = happyGoto action_76
action_106 (29) = happyGoto action_77
action_106 (30) = happyGoto action_78
action_106 (33) = happyGoto action_79
action_106 (34) = happyGoto action_122
action_106 (35) = happyGoto action_81
action_106 (44) = happyGoto action_82
action_106 _ = happyFail (happyExpListPerState 106)

action_107 (51) = happyShift action_121
action_107 _ = happyFail (happyExpListPerState 107)

action_108 _ = happyReduce_47

action_109 (88) = happyShift action_57
action_109 (23) = happyGoto action_64
action_109 (38) = happyGoto action_120
action_109 _ = happyFail (happyExpListPerState 109)

action_110 (54) = happyShift action_119
action_110 _ = happyFail (happyExpListPerState 110)

action_111 _ = happyReduce_51

action_112 (47) = happyShift action_118
action_112 _ = happyFail (happyExpListPerState 112)

action_113 _ = happyReduce_56

action_114 (45) = happyShift action_52
action_114 (46) = happyShift action_53
action_114 (56) = happyShift action_54
action_114 (72) = happyShift action_55
action_114 (77) = happyShift action_56
action_114 (88) = happyShift action_57
action_114 (23) = happyGoto action_48
action_114 (41) = happyGoto action_49
action_114 (42) = happyGoto action_117
action_114 _ = happyFail (happyExpListPerState 114)

action_115 (45) = happyShift action_52
action_115 (46) = happyShift action_53
action_115 (56) = happyShift action_54
action_115 (72) = happyShift action_55
action_115 (77) = happyShift action_56
action_115 (88) = happyShift action_57
action_115 (23) = happyGoto action_48
action_115 (41) = happyGoto action_49
action_115 (42) = happyGoto action_116
action_115 _ = happyFail (happyExpListPerState 115)

action_116 _ = happyReduce_58

action_117 _ = happyReduce_59

action_118 _ = happyReduce_57

action_119 (46) = happyShift action_83
action_119 (48) = happyShift action_84
action_119 (52) = happyShift action_71
action_119 (53) = happyShift action_72
action_119 (57) = happyShift action_23
action_119 (58) = happyShift action_24
action_119 (59) = happyShift action_25
action_119 (60) = happyShift action_26
action_119 (61) = happyShift action_27
action_119 (62) = happyShift action_28
action_119 (63) = happyShift action_29
action_119 (64) = happyShift action_30
action_119 (65) = happyShift action_31
action_119 (66) = happyShift action_32
action_119 (67) = happyShift action_33
action_119 (68) = happyShift action_34
action_119 (69) = happyShift action_35
action_119 (70) = happyShift action_36
action_119 (71) = happyShift action_37
action_119 (73) = happyShift action_38
action_119 (74) = happyShift action_39
action_119 (75) = happyShift action_40
action_119 (76) = happyShift action_41
action_119 (77) = happyShift action_42
action_119 (78) = happyShift action_43
action_119 (79) = happyShift action_44
action_119 (80) = happyShift action_45
action_119 (81) = happyShift action_46
action_119 (83) = happyShift action_85
action_119 (85) = happyShift action_86
action_119 (88) = happyShift action_57
action_119 (89) = happyShift action_47
action_119 (90) = happyShift action_87
action_119 (23) = happyGoto action_73
action_119 (24) = happyGoto action_21
action_119 (25) = happyGoto action_74
action_119 (27) = happyGoto action_75
action_119 (28) = happyGoto action_76
action_119 (29) = happyGoto action_77
action_119 (30) = happyGoto action_132
action_119 (33) = happyGoto action_79
action_119 (35) = happyGoto action_81
action_119 (44) = happyGoto action_82
action_119 _ = happyFail (happyExpListPerState 119)

action_120 _ = happyReduce_45

action_121 (46) = happyShift action_83
action_121 (48) = happyShift action_84
action_121 (52) = happyShift action_71
action_121 (53) = happyShift action_72
action_121 (57) = happyShift action_23
action_121 (58) = happyShift action_24
action_121 (59) = happyShift action_25
action_121 (60) = happyShift action_26
action_121 (61) = happyShift action_27
action_121 (62) = happyShift action_28
action_121 (63) = happyShift action_29
action_121 (64) = happyShift action_30
action_121 (65) = happyShift action_31
action_121 (66) = happyShift action_32
action_121 (67) = happyShift action_33
action_121 (68) = happyShift action_34
action_121 (69) = happyShift action_35
action_121 (70) = happyShift action_36
action_121 (71) = happyShift action_37
action_121 (73) = happyShift action_38
action_121 (74) = happyShift action_39
action_121 (75) = happyShift action_40
action_121 (76) = happyShift action_41
action_121 (77) = happyShift action_42
action_121 (78) = happyShift action_43
action_121 (79) = happyShift action_44
action_121 (80) = happyShift action_45
action_121 (81) = happyShift action_46
action_121 (83) = happyShift action_85
action_121 (85) = happyShift action_86
action_121 (88) = happyShift action_57
action_121 (89) = happyShift action_47
action_121 (90) = happyShift action_87
action_121 (23) = happyGoto action_73
action_121 (24) = happyGoto action_21
action_121 (25) = happyGoto action_74
action_121 (27) = happyGoto action_75
action_121 (28) = happyGoto action_76
action_121 (29) = happyGoto action_77
action_121 (30) = happyGoto action_131
action_121 (33) = happyGoto action_79
action_121 (35) = happyGoto action_81
action_121 (44) = happyGoto action_82
action_121 _ = happyFail (happyExpListPerState 121)

action_122 _ = happyReduce_42

action_123 _ = happyReduce_29

action_124 (46) = happyShift action_83
action_124 (48) = happyShift action_84
action_124 (52) = happyShift action_71
action_124 (53) = happyShift action_72
action_124 (57) = happyShift action_23
action_124 (58) = happyShift action_24
action_124 (59) = happyShift action_25
action_124 (60) = happyShift action_26
action_124 (61) = happyShift action_27
action_124 (62) = happyShift action_28
action_124 (63) = happyShift action_29
action_124 (64) = happyShift action_30
action_124 (65) = happyShift action_31
action_124 (66) = happyShift action_32
action_124 (67) = happyShift action_33
action_124 (68) = happyShift action_34
action_124 (69) = happyShift action_35
action_124 (70) = happyShift action_36
action_124 (71) = happyShift action_37
action_124 (73) = happyShift action_38
action_124 (74) = happyShift action_39
action_124 (75) = happyShift action_40
action_124 (76) = happyShift action_41
action_124 (77) = happyShift action_42
action_124 (78) = happyShift action_43
action_124 (79) = happyShift action_44
action_124 (80) = happyShift action_45
action_124 (81) = happyShift action_46
action_124 (83) = happyShift action_85
action_124 (85) = happyShift action_86
action_124 (88) = happyShift action_57
action_124 (89) = happyShift action_47
action_124 (90) = happyShift action_87
action_124 (23) = happyGoto action_73
action_124 (24) = happyGoto action_21
action_124 (25) = happyGoto action_74
action_124 (27) = happyGoto action_75
action_124 (28) = happyGoto action_76
action_124 (29) = happyGoto action_77
action_124 (30) = happyGoto action_78
action_124 (33) = happyGoto action_79
action_124 (34) = happyGoto action_130
action_124 (35) = happyGoto action_81
action_124 (44) = happyGoto action_82
action_124 _ = happyFail (happyExpListPerState 124)

action_125 (46) = happyShift action_83
action_125 (48) = happyShift action_84
action_125 (52) = happyShift action_71
action_125 (53) = happyShift action_72
action_125 (57) = happyShift action_23
action_125 (58) = happyShift action_24
action_125 (59) = happyShift action_25
action_125 (60) = happyShift action_26
action_125 (61) = happyShift action_27
action_125 (62) = happyShift action_28
action_125 (63) = happyShift action_29
action_125 (64) = happyShift action_30
action_125 (65) = happyShift action_31
action_125 (66) = happyShift action_32
action_125 (67) = happyShift action_33
action_125 (68) = happyShift action_34
action_125 (69) = happyShift action_35
action_125 (70) = happyShift action_36
action_125 (71) = happyShift action_37
action_125 (73) = happyShift action_38
action_125 (74) = happyShift action_39
action_125 (75) = happyShift action_40
action_125 (76) = happyShift action_41
action_125 (77) = happyShift action_42
action_125 (78) = happyShift action_43
action_125 (79) = happyShift action_44
action_125 (80) = happyShift action_45
action_125 (81) = happyShift action_46
action_125 (83) = happyShift action_85
action_125 (85) = happyShift action_86
action_125 (88) = happyShift action_57
action_125 (89) = happyShift action_47
action_125 (90) = happyShift action_87
action_125 (23) = happyGoto action_73
action_125 (24) = happyGoto action_21
action_125 (25) = happyGoto action_74
action_125 (27) = happyGoto action_75
action_125 (28) = happyGoto action_76
action_125 (29) = happyGoto action_77
action_125 (30) = happyGoto action_129
action_125 (33) = happyGoto action_79
action_125 (35) = happyGoto action_81
action_125 (44) = happyGoto action_82
action_125 _ = happyFail (happyExpListPerState 125)

action_126 (49) = happyShift action_128
action_126 _ = happyFail (happyExpListPerState 126)

action_127 _ = happyReduce_39

action_128 (88) = happyShift action_57
action_128 (23) = happyGoto action_90
action_128 (31) = happyGoto action_91
action_128 (32) = happyGoto action_135
action_128 _ = happyFail (happyExpListPerState 128)

action_129 (82) = happyShift action_134
action_129 _ = happyFail (happyExpListPerState 129)

action_130 (47) = happyShift action_133
action_130 _ = happyFail (happyExpListPerState 130)

action_131 _ = happyReduce_34

action_132 _ = happyReduce_48

action_133 _ = happyReduce_40

action_134 (46) = happyShift action_83
action_134 (48) = happyShift action_84
action_134 (52) = happyShift action_71
action_134 (53) = happyShift action_72
action_134 (57) = happyShift action_23
action_134 (58) = happyShift action_24
action_134 (59) = happyShift action_25
action_134 (60) = happyShift action_26
action_134 (61) = happyShift action_27
action_134 (62) = happyShift action_28
action_134 (63) = happyShift action_29
action_134 (64) = happyShift action_30
action_134 (65) = happyShift action_31
action_134 (66) = happyShift action_32
action_134 (67) = happyShift action_33
action_134 (68) = happyShift action_34
action_134 (69) = happyShift action_35
action_134 (70) = happyShift action_36
action_134 (71) = happyShift action_37
action_134 (73) = happyShift action_38
action_134 (74) = happyShift action_39
action_134 (75) = happyShift action_40
action_134 (76) = happyShift action_41
action_134 (77) = happyShift action_42
action_134 (78) = happyShift action_43
action_134 (79) = happyShift action_44
action_134 (80) = happyShift action_45
action_134 (81) = happyShift action_46
action_134 (83) = happyShift action_85
action_134 (85) = happyShift action_86
action_134 (88) = happyShift action_57
action_134 (89) = happyShift action_47
action_134 (90) = happyShift action_87
action_134 (23) = happyGoto action_73
action_134 (24) = happyGoto action_21
action_134 (25) = happyGoto action_74
action_134 (27) = happyGoto action_75
action_134 (28) = happyGoto action_76
action_134 (29) = happyGoto action_77
action_134 (30) = happyGoto action_137
action_134 (33) = happyGoto action_79
action_134 (35) = happyGoto action_81
action_134 (44) = happyGoto action_82
action_134 _ = happyFail (happyExpListPerState 134)

action_135 (47) = happyShift action_136
action_135 _ = happyFail (happyExpListPerState 135)

action_136 (54) = happyShift action_138
action_136 _ = happyFail (happyExpListPerState 136)

action_137 _ = happyReduce_32

action_138 (46) = happyShift action_83
action_138 (48) = happyShift action_84
action_138 (52) = happyShift action_71
action_138 (53) = happyShift action_72
action_138 (57) = happyShift action_23
action_138 (58) = happyShift action_24
action_138 (59) = happyShift action_25
action_138 (60) = happyShift action_26
action_138 (61) = happyShift action_27
action_138 (62) = happyShift action_28
action_138 (63) = happyShift action_29
action_138 (64) = happyShift action_30
action_138 (65) = happyShift action_31
action_138 (66) = happyShift action_32
action_138 (67) = happyShift action_33
action_138 (68) = happyShift action_34
action_138 (69) = happyShift action_35
action_138 (70) = happyShift action_36
action_138 (71) = happyShift action_37
action_138 (73) = happyShift action_38
action_138 (74) = happyShift action_39
action_138 (75) = happyShift action_40
action_138 (76) = happyShift action_41
action_138 (77) = happyShift action_42
action_138 (78) = happyShift action_43
action_138 (79) = happyShift action_44
action_138 (80) = happyShift action_45
action_138 (81) = happyShift action_46
action_138 (83) = happyShift action_85
action_138 (85) = happyShift action_86
action_138 (88) = happyShift action_57
action_138 (89) = happyShift action_47
action_138 (90) = happyShift action_87
action_138 (23) = happyGoto action_73
action_138 (24) = happyGoto action_21
action_138 (25) = happyGoto action_74
action_138 (27) = happyGoto action_75
action_138 (28) = happyGoto action_76
action_138 (29) = happyGoto action_77
action_138 (30) = happyGoto action_139
action_138 (33) = happyGoto action_79
action_138 (35) = happyGoto action_81
action_138 (44) = happyGoto action_82
action_138 _ = happyFail (happyExpListPerState 138)

action_139 (84) = happyShift action_140
action_139 _ = happyFail (happyExpListPerState 139)

action_140 (46) = happyShift action_83
action_140 (48) = happyShift action_84
action_140 (52) = happyShift action_71
action_140 (53) = happyShift action_72
action_140 (57) = happyShift action_23
action_140 (58) = happyShift action_24
action_140 (59) = happyShift action_25
action_140 (60) = happyShift action_26
action_140 (61) = happyShift action_27
action_140 (62) = happyShift action_28
action_140 (63) = happyShift action_29
action_140 (64) = happyShift action_30
action_140 (65) = happyShift action_31
action_140 (66) = happyShift action_32
action_140 (67) = happyShift action_33
action_140 (68) = happyShift action_34
action_140 (69) = happyShift action_35
action_140 (70) = happyShift action_36
action_140 (71) = happyShift action_37
action_140 (73) = happyShift action_38
action_140 (74) = happyShift action_39
action_140 (75) = happyShift action_40
action_140 (76) = happyShift action_41
action_140 (77) = happyShift action_42
action_140 (78) = happyShift action_43
action_140 (79) = happyShift action_44
action_140 (80) = happyShift action_45
action_140 (81) = happyShift action_46
action_140 (83) = happyShift action_85
action_140 (85) = happyShift action_86
action_140 (88) = happyShift action_57
action_140 (89) = happyShift action_47
action_140 (90) = happyShift action_87
action_140 (23) = happyGoto action_73
action_140 (24) = happyGoto action_21
action_140 (25) = happyGoto action_74
action_140 (27) = happyGoto action_75
action_140 (28) = happyGoto action_76
action_140 (29) = happyGoto action_77
action_140 (30) = happyGoto action_141
action_140 (33) = happyGoto action_79
action_140 (35) = happyGoto action_81
action_140 (44) = happyGoto action_82
action_140 _ = happyFail (happyExpListPerState 140)

action_141 _ = happyReduce_33

happyReduce_19 = happySpecReduce_1  22 happyReduction_19
happyReduction_19 (HappyTerminal (PT _ (T_FunVar happy_var_1)))
	 =  HappyAbsSyn22
		 (Parser.Abs.FunVar happy_var_1
	)
happyReduction_19 _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  23 happyReduction_20
happyReduction_20 (HappyTerminal (PT _ (T_Var happy_var_1)))
	 =  HappyAbsSyn23
		 (Parser.Abs.Var happy_var_1
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  24 happyReduction_21
happyReduction_21 (HappyTerminal (PT _ (T_GateIdent happy_var_1)))
	 =  HappyAbsSyn24
		 (Parser.Abs.GateIdent happy_var_1
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_1  25 happyReduction_22
happyReduction_22 (HappyTerminal (PT _ (T_Lambda happy_var_1)))
	 =  HappyAbsSyn25
		 (Parser.Abs.Lambda happy_var_1
	)
happyReduction_22 _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  26 happyReduction_23
happyReduction_23 (HappyAbsSyn37  happy_var_1)
	 =  HappyAbsSyn26
		 (Parser.Abs.PDef happy_var_1
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  27 happyReduction_24
happyReduction_24 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn27
		 (Parser.Abs.TVar happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  27 happyReduction_25
happyReduction_25 (HappyAbsSyn35  happy_var_1)
	 =  HappyAbsSyn27
		 (Parser.Abs.TBit happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  27 happyReduction_26
happyReduction_26 (HappyAbsSyn44  happy_var_1)
	 =  HappyAbsSyn27
		 (Parser.Abs.TGate happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_1  27 happyReduction_27
happyReduction_27 (HappyAbsSyn33  happy_var_1)
	 =  HappyAbsSyn27
		 (Parser.Abs.TTup happy_var_1
	)
happyReduction_27 _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  27 happyReduction_28
happyReduction_28 _
	 =  HappyAbsSyn27
		 (Parser.Abs.TStar
	)

happyReduce_29 = happySpecReduce_3  27 happyReduction_29
happyReduction_29 _
	(HappyAbsSyn27  happy_var_2)
	_
	 =  HappyAbsSyn27
		 (happy_var_2
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_2  28 happyReduction_30
happyReduction_30 (HappyAbsSyn27  happy_var_2)
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (Parser.Abs.TApp happy_var_1 happy_var_2
	)
happyReduction_30 _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  28 happyReduction_31
happyReduction_31 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happyReduce 6 29 happyReduction_32
happyReduction_32 ((HappyAbsSyn27  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Parser.Abs.TIfEl happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_33 = happyReduce 10 29 happyReduction_33
happyReduction_33 ((HappyAbsSyn27  happy_var_10) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_8) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn32  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn31  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Parser.Abs.TLet happy_var_3 happy_var_5 happy_var_8 happy_var_10
	) `HappyStk` happyRest

happyReduce_34 = happyReduce 4 29 happyReduction_34
happyReduction_34 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn23  happy_var_2) `HappyStk`
	(HappyAbsSyn25  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn27
		 (Parser.Abs.TLamb happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_35 = happySpecReduce_1  29 happyReduction_35
happyReduction_35 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_35 _  = notHappyAtAll 

happyReduce_36 = happySpecReduce_1  30 happyReduction_36
happyReduction_36 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn27
		 (happy_var_1
	)
happyReduction_36 _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  31 happyReduction_37
happyReduction_37 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn31
		 (Parser.Abs.LVar happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_1  32 happyReduction_38
happyReduction_38 (HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn32
		 ((:[]) happy_var_1
	)
happyReduction_38 _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_3  32 happyReduction_39
happyReduction_39 (HappyAbsSyn32  happy_var_3)
	_
	(HappyAbsSyn31  happy_var_1)
	 =  HappyAbsSyn32
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_39 _ _ _  = notHappyAtAll 

happyReduce_40 = happyReduce 5 33 happyReduction_40
happyReduction_40 (_ `HappyStk`
	(HappyAbsSyn34  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn27  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn33
		 (Parser.Abs.Tuple happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_41 = happySpecReduce_1  34 happyReduction_41
happyReduction_41 (HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn34
		 ((:[]) happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_3  34 happyReduction_42
happyReduction_42 (HappyAbsSyn34  happy_var_3)
	_
	(HappyAbsSyn27  happy_var_1)
	 =  HappyAbsSyn34
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_42 _ _ _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  35 happyReduction_43
happyReduction_43 _
	 =  HappyAbsSyn35
		 (Parser.Abs.BZero
	)

happyReduce_44 = happySpecReduce_1  35 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn35
		 (Parser.Abs.BOne
	)

happyReduce_45 = happySpecReduce_3  36 happyReduction_45
happyReduction_45 (HappyAbsSyn38  happy_var_3)
	(HappyAbsSyn41  happy_var_2)
	(HappyAbsSyn22  happy_var_1)
	 =  HappyAbsSyn36
		 (Parser.Abs.FDecl happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happySpecReduce_0  37 happyReduction_46
happyReduction_46  =  HappyAbsSyn37
		 ([]
	)

happyReduce_47 = happySpecReduce_2  37 happyReduction_47
happyReduction_47 (HappyAbsSyn37  happy_var_2)
	(HappyAbsSyn36  happy_var_1)
	 =  HappyAbsSyn37
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_47 _ _  = notHappyAtAll 

happyReduce_48 = happyReduce 4 38 happyReduction_48
happyReduction_48 ((HappyAbsSyn27  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn40  happy_var_2) `HappyStk`
	(HappyAbsSyn23  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn38
		 (Parser.Abs.FDef happy_var_1 happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_49 = happySpecReduce_1  39 happyReduction_49
happyReduction_49 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn39
		 (Parser.Abs.FArg happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_0  40 happyReduction_50
happyReduction_50  =  HappyAbsSyn40
		 ([]
	)

happyReduce_51 = happySpecReduce_2  40 happyReduction_51
happyReduction_51 (HappyAbsSyn40  happy_var_2)
	(HappyAbsSyn39  happy_var_1)
	 =  HappyAbsSyn40
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_51 _ _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_1  41 happyReduction_52
happyReduction_52 (HappyAbsSyn23  happy_var_1)
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeVar happy_var_1
	)
happyReduction_52 _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  41 happyReduction_53
happyReduction_53 _
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeBit
	)

happyReduce_54 = happySpecReduce_1  41 happyReduction_54
happyReduction_54 _
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeQbit
	)

happyReduce_55 = happySpecReduce_1  41 happyReduction_55
happyReduction_55 _
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeVoid
	)

happyReduce_56 = happySpecReduce_2  41 happyReduction_56
happyReduction_56 (HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeDup happy_var_2
	)
happyReduction_56 _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  41 happyReduction_57
happyReduction_57 _
	(HappyAbsSyn41  happy_var_2)
	_
	 =  HappyAbsSyn41
		 (happy_var_2
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  42 happyReduction_58
happyReduction_58 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeTens happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_3  42 happyReduction_59
happyReduction_59 (HappyAbsSyn41  happy_var_3)
	_
	(HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (Parser.Abs.TypeFunc happy_var_1 happy_var_3
	)
happyReduction_59 _ _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  42 happyReduction_60
happyReduction_60 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_1  43 happyReduction_61
happyReduction_61 (HappyAbsSyn41  happy_var_1)
	 =  HappyAbsSyn41
		 (happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  44 happyReduction_62
happyReduction_62 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GH
	)

happyReduce_63 = happySpecReduce_1  44 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GX
	)

happyReduce_64 = happySpecReduce_1  44 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GY
	)

happyReduce_65 = happySpecReduce_1  44 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GZ
	)

happyReduce_66 = happySpecReduce_1  44 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GI
	)

happyReduce_67 = happySpecReduce_1  44 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GS
	)

happyReduce_68 = happySpecReduce_1  44 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GT
	)

happyReduce_69 = happySpecReduce_1  44 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCNOT
	)

happyReduce_70 = happySpecReduce_1  44 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GTOF
	)

happyReduce_71 = happySpecReduce_1  44 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GSWP
	)

happyReduce_72 = happySpecReduce_1  44 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GFRDK
	)

happyReduce_73 = happySpecReduce_1  44 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GQFT
	)

happyReduce_74 = happySpecReduce_1  44 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GQFTI
	)

happyReduce_75 = happySpecReduce_1  44 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCT
	)

happyReduce_76 = happySpecReduce_1  44 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR2
	)

happyReduce_77 = happySpecReduce_1  44 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR2D
	)

happyReduce_78 = happySpecReduce_1  44 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR3
	)

happyReduce_79 = happySpecReduce_1  44 happyReduction_79
happyReduction_79 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR3D
	)

happyReduce_80 = happySpecReduce_1  44 happyReduction_80
happyReduction_80 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR4
	)

happyReduce_81 = happySpecReduce_1  44 happyReduction_81
happyReduction_81 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR4D
	)

happyReduce_82 = happySpecReduce_1  44 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR5
	)

happyReduce_83 = happySpecReduce_1  44 happyReduction_83
happyReduction_83 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR5D
	)

happyReduce_84 = happySpecReduce_1  44 happyReduction_84
happyReduction_84 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR8
	)

happyReduce_85 = happySpecReduce_1  44 happyReduction_85
happyReduction_85 _
	 =  HappyAbsSyn44
		 (Parser.Abs.GCR8D
	)

happyReduce_86 = happySpecReduce_1  44 happyReduction_86
happyReduction_86 (HappyAbsSyn24  happy_var_1)
	 =  HappyAbsSyn44
		 (Parser.Abs.GGate happy_var_1
	)
happyReduction_86 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 91 91 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 45;
	PT _ (TS _ 2) -> cont 46;
	PT _ (TS _ 3) -> cont 47;
	PT _ (TS _ 4) -> cont 48;
	PT _ (TS _ 5) -> cont 49;
	PT _ (TS _ 6) -> cont 50;
	PT _ (TS _ 7) -> cont 51;
	PT _ (TS _ 8) -> cont 52;
	PT _ (TS _ 9) -> cont 53;
	PT _ (TS _ 10) -> cont 54;
	PT _ (TS _ 11) -> cont 55;
	PT _ (TS _ 12) -> cont 56;
	PT _ (TS _ 13) -> cont 57;
	PT _ (TS _ 14) -> cont 58;
	PT _ (TS _ 15) -> cont 59;
	PT _ (TS _ 16) -> cont 60;
	PT _ (TS _ 17) -> cont 61;
	PT _ (TS _ 18) -> cont 62;
	PT _ (TS _ 19) -> cont 63;
	PT _ (TS _ 20) -> cont 64;
	PT _ (TS _ 21) -> cont 65;
	PT _ (TS _ 22) -> cont 66;
	PT _ (TS _ 23) -> cont 67;
	PT _ (TS _ 24) -> cont 68;
	PT _ (TS _ 25) -> cont 69;
	PT _ (TS _ 26) -> cont 70;
	PT _ (TS _ 27) -> cont 71;
	PT _ (TS _ 28) -> cont 72;
	PT _ (TS _ 29) -> cont 73;
	PT _ (TS _ 30) -> cont 74;
	PT _ (TS _ 31) -> cont 75;
	PT _ (TS _ 32) -> cont 76;
	PT _ (TS _ 33) -> cont 77;
	PT _ (TS _ 34) -> cont 78;
	PT _ (TS _ 35) -> cont 79;
	PT _ (TS _ 36) -> cont 80;
	PT _ (TS _ 37) -> cont 81;
	PT _ (TS _ 38) -> cont 82;
	PT _ (TS _ 39) -> cont 83;
	PT _ (TS _ 40) -> cont 84;
	PT _ (TS _ 41) -> cont 85;
	PT _ (TS _ 42) -> cont 86;
	PT _ (T_FunVar happy_dollar_dollar) -> cont 87;
	PT _ (T_Var happy_dollar_dollar) -> cont 88;
	PT _ (T_GateIdent happy_dollar_dollar) -> cont 89;
	PT _ (T_Lambda happy_dollar_dollar) -> cont 90;
	_ -> happyError' ((tk:tks), [])
	}

happyError_ explist 91 tk tks = happyError' (tks, explist)
happyError_ explist _ tk tks = happyError' ((tk:tks), explist)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = ((>>=))
happyReturn :: () => a -> Err a
happyReturn = (return)
happyThen1 m k tks = ((>>=)) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (return) a
happyError' :: () => ([(Token)], [String]) -> Err a
happyError' = (\(tokens, _) -> happyError tokens)
pProgram tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn26 z -> happyReturn z; _other -> notHappyAtAll })

pTerm3 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_1 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pTerm2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_2 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pTerm1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_3 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_4 tks) (\x -> case x of {HappyAbsSyn27 z -> happyReturn z; _other -> notHappyAtAll })

pLetVar tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_5 tks) (\x -> case x of {HappyAbsSyn31 z -> happyReturn z; _other -> notHappyAtAll })

pListLetVar tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_6 tks) (\x -> case x of {HappyAbsSyn32 z -> happyReturn z; _other -> notHappyAtAll })

pTup tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_7 tks) (\x -> case x of {HappyAbsSyn33 z -> happyReturn z; _other -> notHappyAtAll })

pListTerm tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_8 tks) (\x -> case x of {HappyAbsSyn34 z -> happyReturn z; _other -> notHappyAtAll })

pBit tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_9 tks) (\x -> case x of {HappyAbsSyn35 z -> happyReturn z; _other -> notHappyAtAll })

pFunDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_10 tks) (\x -> case x of {HappyAbsSyn36 z -> happyReturn z; _other -> notHappyAtAll })

pListFunDec tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_11 tks) (\x -> case x of {HappyAbsSyn37 z -> happyReturn z; _other -> notHappyAtAll })

pFunction tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_12 tks) (\x -> case x of {HappyAbsSyn38 z -> happyReturn z; _other -> notHappyAtAll })

pArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_13 tks) (\x -> case x of {HappyAbsSyn39 z -> happyReturn z; _other -> notHappyAtAll })

pListArg tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_14 tks) (\x -> case x of {HappyAbsSyn40 z -> happyReturn z; _other -> notHappyAtAll })

pType2 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_15 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType1 tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_16 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pType tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_17 tks) (\x -> case x of {HappyAbsSyn41 z -> happyReturn z; _other -> notHappyAtAll })

pGate tks = happySomeParser where
 happySomeParser = happyThen (happyParse action_18 tks) (\x -> case x of {HappyAbsSyn44 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- $Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp $










































data Happy_IntList = HappyCons Int Happy_IntList








































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
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action









































indexShortOffAddr arr off = arr Happy_Data_Array.! off


{-# INLINE happyLt #-}
happyLt x y = (x < y)






readArrayBit arr bit =
    Bits.testBit (indexShortOffAddr arr (bit `div` 16)) (bit `mod` 16)






-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail [] (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             _ = nt :: Int
             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction









happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery (ERROR_TOK is the error token)

-- parse error if we are in recovery and we fail again
happyFail explist (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
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
happyFail explist i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ((HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







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
