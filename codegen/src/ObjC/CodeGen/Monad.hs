{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | The code generation monad and its environment.
--
-- Uses 'RWS' so that:
--
-- * The 'CodeGenEnv' (hierarchy, known types, current framework) is
--   available everywhere via 'Reader'.
-- * 'SkippedBinding' reports accumulate via 'Writer', eliminating
--   manual threading of skip lists.
-- * 'GenState' provides mutable caches if needed.
module ObjC.CodeGen.Monad
  ( -- * Monad
    CodeGen
  , runCodeGen
    -- * Environment
  , CodeGenEnv(..)
  , mkEnv
    -- * State
  , GenState(..)
  , emptyGenState
    -- * Reader helpers
  , askHierarchy
  , askFwMap
  , askKnownTypes
  , askFramework
  , askDepFws
    -- * Writer helpers
  , skipBinding
  ) where

import Control.Monad.RWS.Strict (RWS, runRWS, asks, tell)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

import ObjC.CodeGen.IR (ClassHierarchy)
import ObjC.CodeGen.TypeMap (KnownTypes)
import ObjC.CodeGen.Generate.Types (SkippedBinding(..))

-- ---------------------------------------------------------------------------
-- Environment (Reader)
-- ---------------------------------------------------------------------------

-- | Read-only context available throughout generation of a single
-- framework package.
data CodeGenEnv = CodeGenEnv
  { cgeHierarchy    :: ClassHierarchy
    -- ^ The full, merged class hierarchy.
  , cgeFwMap         :: Map Text Text
    -- ^ Class name → framework name.
  , cgeKnownTypes   :: KnownTypes
    -- ^ All generated class, struct, and enum names.
  , cgeFramework    :: Text
    -- ^ The framework currently being generated.
  , cgeDepFws       :: Set Text
    -- ^ Dependency package names (e.g., @\"objc-foundation\"@).
  , cgeAvailableFws :: Set Text
    -- ^ All framework names that will have generated packages.
  }

-- | Build a 'CodeGenEnv'.  Convenience constructor when all fields
-- are known.
mkEnv
  :: ClassHierarchy
  -> Map Text Text
  -> KnownTypes
  -> Text
  -> Set Text
  -> Set Text
  -> CodeGenEnv
mkEnv = CodeGenEnv

-- ---------------------------------------------------------------------------
-- State
-- ---------------------------------------------------------------------------

-- | Mutable state carried during generation.  Starts minimal;
-- extend as caching opportunities arise.
data GenState = GenState
  deriving (Show)

-- | Initial state.
emptyGenState :: GenState
emptyGenState = GenState

-- ---------------------------------------------------------------------------
-- The monad
-- ---------------------------------------------------------------------------

-- | Code generation monad.
--
-- * __Reader__: 'CodeGenEnv'
-- * __Writer__: @['SkippedBinding']@
-- * __State__:  'GenState'
type CodeGen a = RWS CodeGenEnv [SkippedBinding] GenState a

-- | Run a 'CodeGen' action, returning the result and accumulated skips.
runCodeGen :: CodeGenEnv -> CodeGen a -> (a, [SkippedBinding])
runCodeGen env action =
  let (a, _st, skips) = runRWS action env emptyGenState
  in (a, skips)

-- ---------------------------------------------------------------------------
-- Reader helpers
-- ---------------------------------------------------------------------------

askHierarchy :: CodeGen ClassHierarchy
askHierarchy = asks cgeHierarchy

askFwMap :: CodeGen (Map Text Text)
askFwMap = asks cgeFwMap

askKnownTypes :: CodeGen KnownTypes
askKnownTypes = asks cgeKnownTypes

askFramework :: CodeGen Text
askFramework = asks cgeFramework

askDepFws :: CodeGen (Set Text)
askDepFws = asks cgeDepFws

-- ---------------------------------------------------------------------------
-- Writer helpers
-- ---------------------------------------------------------------------------

-- | Record a skipped binding via the Writer.
skipBinding :: Text -> Text -> Text -> CodeGen ()
skipBinding cls sel reason = do
  fw <- askFramework
  tell [SkippedBinding
    { skipFramework = fw
    , skipClass     = cls
    , skipSelector  = sel
    , skipReason    = reason
    }]
