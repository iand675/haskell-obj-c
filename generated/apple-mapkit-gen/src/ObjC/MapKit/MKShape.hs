{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKShape@.
module ObjC.MapKit.MKShape
  ( MKShape
  , IsMKShape(..)
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , setSubtitleSelector
  , setTitleSelector
  , subtitleSelector
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
title :: IsMKShape mkShape => mkShape -> IO (Id NSString)
title mkShape =
  sendMessage mkShape titleSelector

-- | @- setTitle:@
setTitle :: (IsMKShape mkShape, IsNSString value) => mkShape -> value -> IO ()
setTitle mkShape value =
  sendMessage mkShape setTitleSelector (toNSString value)

-- | @- subtitle@
subtitle :: IsMKShape mkShape => mkShape -> IO (Id NSString)
subtitle mkShape =
  sendMessage mkShape subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsMKShape mkShape, IsNSString value) => mkShape -> value -> IO ()
setSubtitle mkShape value =
  sendMessage mkShape setSubtitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector '[] (Id NSString)
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector '[Id NSString] ()
setSubtitleSelector = mkSelector "setSubtitle:"

