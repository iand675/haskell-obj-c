{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @EKStructuredLocation@.
module ObjC.EventKit.EKStructuredLocation
  ( EKStructuredLocation
  , IsEKStructuredLocation(..)
  , locationWithTitle
  , locationWithMapItem
  , title
  , setTitle
  , radius
  , setRadius
  , locationWithMapItemSelector
  , locationWithTitleSelector
  , radiusSelector
  , setRadiusSelector
  , setTitleSelector
  , titleSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.MapKit.Internal.Classes

-- | @+ locationWithTitle:@
locationWithTitle :: IsNSString title => title -> IO (Id EKStructuredLocation)
locationWithTitle title =
  do
    cls' <- getRequiredClass "EKStructuredLocation"
    sendClassMessage cls' locationWithTitleSelector (toNSString title)

-- | @+ locationWithMapItem:@
locationWithMapItem :: IsMKMapItem mapItem => mapItem -> IO (Id EKStructuredLocation)
locationWithMapItem mapItem =
  do
    cls' <- getRequiredClass "EKStructuredLocation"
    sendClassMessage cls' locationWithMapItemSelector (toMKMapItem mapItem)

-- | @- title@
title :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> IO (Id NSString)
title ekStructuredLocation =
  sendMessage ekStructuredLocation titleSelector

-- | @- setTitle:@
setTitle :: (IsEKStructuredLocation ekStructuredLocation, IsNSString value) => ekStructuredLocation -> value -> IO ()
setTitle ekStructuredLocation value =
  sendMessage ekStructuredLocation setTitleSelector (toNSString value)

-- | @- radius@
radius :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> IO CDouble
radius ekStructuredLocation =
  sendMessage ekStructuredLocation radiusSelector

-- | @- setRadius:@
setRadius :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> CDouble -> IO ()
setRadius ekStructuredLocation value =
  sendMessage ekStructuredLocation setRadiusSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationWithTitle:@
locationWithTitleSelector :: Selector '[Id NSString] (Id EKStructuredLocation)
locationWithTitleSelector = mkSelector "locationWithTitle:"

-- | @Selector@ for @locationWithMapItem:@
locationWithMapItemSelector :: Selector '[Id MKMapItem] (Id EKStructuredLocation)
locationWithMapItemSelector = mkSelector "locationWithMapItem:"

-- | @Selector@ for @title@
titleSelector :: Selector '[] (Id NSString)
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector '[Id NSString] ()
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @radius@
radiusSelector :: Selector '[] CDouble
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector '[CDouble] ()
setRadiusSelector = mkSelector "setRadius:"

