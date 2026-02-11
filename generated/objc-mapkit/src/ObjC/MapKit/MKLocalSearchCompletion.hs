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
  , titleSelector
  , titleHighlightRangesSelector
  , subtitleSelector
  , subtitleHighlightRangesSelector


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

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- title@
title :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSString)
title mkLocalSearchCompletion  =
  sendMsg mkLocalSearchCompletion (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- titleHighlightRanges@
titleHighlightRanges :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSArray)
titleHighlightRanges mkLocalSearchCompletion  =
  sendMsg mkLocalSearchCompletion (mkSelector "titleHighlightRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subtitle@
subtitle :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSString)
subtitle mkLocalSearchCompletion  =
  sendMsg mkLocalSearchCompletion (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- subtitleHighlightRanges@
subtitleHighlightRanges :: IsMKLocalSearchCompletion mkLocalSearchCompletion => mkLocalSearchCompletion -> IO (Id NSArray)
subtitleHighlightRanges mkLocalSearchCompletion  =
  sendMsg mkLocalSearchCompletion (mkSelector "subtitleHighlightRanges") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @titleHighlightRanges@
titleHighlightRangesSelector :: Selector
titleHighlightRangesSelector = mkSelector "titleHighlightRanges"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @subtitleHighlightRanges@
subtitleHighlightRangesSelector :: Selector
subtitleHighlightRangesSelector = mkSelector "subtitleHighlightRanges"

