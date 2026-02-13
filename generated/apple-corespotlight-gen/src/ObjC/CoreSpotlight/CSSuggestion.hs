{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CSSuggestion@.
module ObjC.CoreSpotlight.CSSuggestion
  ( CSSuggestion
  , IsCSSuggestion(..)
  , compareByRank
  , compare_
  , localizedAttributedSuggestion
  , suggestionKind
  , compareByRankSelector
  , compareSelector
  , localizedAttributedSuggestionSelector
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreSpotlight.Internal.Classes
import ObjC.CoreSpotlight.Internal.Enums
import ObjC.Foundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- compareByRank:@
compareByRank :: (IsCSSuggestion csSuggestion, IsCSSuggestion other) => csSuggestion -> other -> IO NSComparisonResult
compareByRank csSuggestion other =
  sendMessage csSuggestion compareByRankSelector (toCSSuggestion other)

-- | @- compare:@
compare_ :: (IsCSSuggestion csSuggestion, IsCSSuggestion other) => csSuggestion -> other -> IO NSComparisonResult
compare_ csSuggestion other =
  sendMessage csSuggestion compareSelector (toCSSuggestion other)

-- | @- localizedAttributedSuggestion@
localizedAttributedSuggestion :: IsCSSuggestion csSuggestion => csSuggestion -> IO (Id NSAttributedString)
localizedAttributedSuggestion csSuggestion =
  sendMessage csSuggestion localizedAttributedSuggestionSelector

-- | @- suggestionKind@
suggestionKind :: IsCSSuggestion csSuggestion => csSuggestion -> IO CSSuggestionKind
suggestionKind csSuggestion =
  sendMessage csSuggestion suggestionKindSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @compareByRank:@
compareByRankSelector :: Selector '[Id CSSuggestion] NSComparisonResult
compareByRankSelector = mkSelector "compareByRank:"

-- | @Selector@ for @compare:@
compareSelector :: Selector '[Id CSSuggestion] NSComparisonResult
compareSelector = mkSelector "compare:"

-- | @Selector@ for @localizedAttributedSuggestion@
localizedAttributedSuggestionSelector :: Selector '[] (Id NSAttributedString)
localizedAttributedSuggestionSelector = mkSelector "localizedAttributedSuggestion"

-- | @Selector@ for @suggestionKind@
suggestionKindSelector :: Selector '[] CSSuggestionKind
suggestionKindSelector = mkSelector "suggestionKind"

