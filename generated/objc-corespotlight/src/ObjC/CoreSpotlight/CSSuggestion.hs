{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSuggestion@.
module ObjC.CoreSpotlight.CSSuggestion
  ( CSSuggestion
  , IsCSSuggestion(..)
  , compareByRank
  , compare_
  , suggestionKind
  , compareByRankSelector
  , compareSelector
  , suggestionKindSelector

  -- * Enum types
  , CSSuggestionKind(CSSuggestionKind)
  , pattern CSSuggestionKindNone
  , pattern CSSuggestionKindCustom
  , pattern CSSuggestionKindDefault
  , NSComparisonResult(NSComparisonResult)
  , pattern NSOrderedAscending
  , pattern NSOrderedSame
  , pattern NSOrderedDescending

  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- compareByRank:@
compareByRank :: (IsCSSuggestion csSuggestion, IsCSSuggestion other) => csSuggestion -> other -> IO NSComparisonResult
compareByRank csSuggestion  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg csSuggestion (mkSelector "compareByRank:") retCLong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- compare:@
compare_ :: (IsCSSuggestion csSuggestion, IsCSSuggestion other) => csSuggestion -> other -> IO NSComparisonResult
compare_ csSuggestion  other =
withObjCPtr other $ \raw_other ->
    fmap (coerce :: CLong -> NSComparisonResult) $ sendMsg csSuggestion (mkSelector "compare:") retCLong [argPtr (castPtr raw_other :: Ptr ())]

-- | @- suggestionKind@
suggestionKind :: IsCSSuggestion csSuggestion => csSuggestion -> IO CSSuggestionKind
suggestionKind csSuggestion  =
  fmap (coerce :: CLong -> CSSuggestionKind) $ sendMsg csSuggestion (mkSelector "suggestionKind") retCLong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compareByRank:@
compareByRankSelector :: Selector
compareByRankSelector = mkSelector "compareByRank:"

-- | @Selector@ for @compare:@
compareSelector :: Selector
compareSelector = mkSelector "compare:"

-- | @Selector@ for @suggestionKind@
suggestionKindSelector :: Selector
suggestionKindSelector = mkSelector "suggestionKind"

