{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKLocalSearchCompletion@.
module ObjC.MapKit.MKLocalSearchCompletion
  ( MKLocalSearchCompletion
  , IsMKLocalSearchCompletion(..)
  , title
  , titleHighlightRanges
  , subtitle
  , subtitleHighlightRanges
  , subtitleHighlightRangesSelector
  , subtitleSelector
  , titleHighlightRangesSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSString)
title mkLocalSearchCompletion =
  sendMessage mkLocalSearchCompletion titleSelector

-- | @- titleHighlightRanges@
titleHighlightRanges :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSArray)
titleHighlightRanges mkLocalSearchCompletion =
  sendMessage mkLocalSearchCompletion titleHighlightRangesSelector

-- | @- subtitle@
subtitle :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSString)
subtitle mkLocalSearchCompletion =
  sendMessage mkLocalSearchCompletion subtitleSelector

-- | @- subtitleHighlightRanges@
subtitleHighlightRanges :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSArray)
subtitleHighlightRanges mkLocalSearchCompletion =
  sendMessage mkLocalSearchCompletion subtitleHighlightRangesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @titleHighlightRanges@
titleHighlightRangesSelector :: Selector '[] (Id NSArray)
titleHighlightRangesSelector = mkSelector "titleHighlightRanges"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @subtitleHighlightRanges@
subtitleHighlightRangesSelector :: Selector '[] (Id NSArray)
subtitleHighlightRangesSelector = mkSelector "subtitleHighlightRanges"

