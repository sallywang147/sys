{-|

This module is the simple Double Free checker. Its symbolic counterpart is in
DoubleFreeSymbolic.hs.

-}
module Checkers.DFStatic ( dfCheck
                                 , blankDFState
                                 , DFState
                                 , DFBug(..)
                                 ) where
import           Checkers.Utils.StaticUtils
import           Control.Monad              (forM_, when)
import qualified Checkers.DFSymbolic as D
import           Data.List                  (isInfixOf)
import qualified Data.Set                   as S
import           LLVM.AST                   hiding (args)
import qualified LLVM.AST.Operand           as O
import           LLVM.AST.Typed
import           LLVMAST.Interface          hiding (getFunName)
import           Static.CheckerState

-- | Identify suspicious-looking code snippets that might involve double frees.
dfCheck :: Int -> Named Instruction -> DFChecker ()
dfCheck lineno ninstr = do
  filterFloats ninstr
  filterAsserts ninstr
  modelAssigns ninstr

  case ninstr of
    -- Detect calls to free and mark variables as freed
    Do (Call _ _ _ (Right func) ops _ _) | D.isFree func ->
      forM_ (map fst ops) $ \op ->
        case nameOf op of
          Nothing -> return ()
          Just varName -> do
            free <- isFreed op
            if free
              then addDFBug op lineno -- Double free detected
              else markAsFreed varName

    _ -> return ()

-- | Mark a variable as freed
markAsFreed :: Name -> DFChecker ()
markAsFreed name = do
  s <- getState
  putState $ s { freed = S.insert name (freed s) }

-- | Check if a variable has already been freed
isFreed :: Operand -> DFChecker Bool
isFreed (O.LocalReference _ name) = do
  s <- getState
  return $ S.member name (freed s)
isFreed _ = return False

-- | State to track freed variables
data DFState = DFState { freed :: S.Set Name } deriving (Eq, Ord, Show)

blankDFState :: DFState
blankDFState = DFState S.empty

type DFChecker a = Checker DFState DFBug a

data DFBug = DFBug { dfBugFilePath  :: FilePath
                   , dfBugFunction  :: Name
                   , dfBugPath      :: Path
                   , dfBugLine      :: Int
                   , dfBugPointer   :: Name
                   } deriving (Eq, Ord, Show)

-- | Report a double free bug
addDFBug :: Operand -> Int -> DFChecker ()
addDFBug op lineno = do
  fp <- getFilepath
  fn <- getFunName
  path <- getPath
  let name = nameOf' op
  addBug $ DFBug { dfBugFilePath = fp
                 , dfBugFunction = fn
                 , dfBugPath     = path
                 , dfBugLine     = lineno
                 , dfBugPointer  = name
                 }
