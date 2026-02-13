{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MKUserLocation@.
module ObjC.MapKit.MKUserLocation
  ( MKUserLocation
  , IsMKUserLocation(..)
  , updating
  , heading
  , title
  , setTitle
  , subtitle
  , setSubtitle
  , headingSelector
  , setSubtitleSelector
  , setTitleSelector
  , subtitleSelector
  , titleSelector
  , updatingSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MapKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- updating@
updating :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO Bool
updating mkUserLocation =
  sendMessage mkUserLocation updatingSelector

-- | @- heading@
heading :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO RawId
heading mkUserLocation =
  sendMessage mkUserLocation headingSelector

-- | @- title@
title :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO (Id NSString)
title mkUserLocation =
  sendMessage mkUserLocation titleSelector

-- | @- setTitle:@
setTitle :: (IsMKUserLocation mkUserLocation, IsNSString value) => mkUserLocation -> value -> IO ()
setTitle mkUserLocation value =
  sendMessage mkUserLocation setTitleSelector (toNSString value)

-- | @- subtitle@
subtitle :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO (Id NSString)
subtitle mkUserLocation =
  sendMessage mkUserLocation subtitleSelector

-- | @- setSubtitle:@
setSubtitle :: (IsMKUserLocation mkUserLocation, IsNSString value) => mkUserLocation -> value -> IO ()
setSubtitle mkUserLocation value =
  sendMessage mkUserLocation setSubtitleSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updating@
updatingSelector :: Selector '[] Bool
updatingSelector = mkSelector "updating"

-- | @Selector@ for @heading@
headingSelector :: Selector '[] RawId
headingSelector = mkSelector "heading"

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

