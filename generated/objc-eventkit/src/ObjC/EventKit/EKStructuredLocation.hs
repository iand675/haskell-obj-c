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
  , locationWithTitleSelector
  , locationWithMapItemSelector
  , titleSelector
  , setTitleSelector
  , radiusSelector
  , setRadiusSelector


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

import ObjC.EventKit.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.MapKit.Internal.Classes

-- | @+ locationWithTitle:@
locationWithTitle :: IsNSString title => title -> IO (Id EKStructuredLocation)
locationWithTitle title =
  do
    cls' <- getRequiredClass "EKStructuredLocation"
    withObjCPtr title $ \raw_title ->
      sendClassMsg cls' (mkSelector "locationWithTitle:") (retPtr retVoid) [argPtr (castPtr raw_title :: Ptr ())] >>= retainedObject . castPtr

-- | @+ locationWithMapItem:@
locationWithMapItem :: IsMKMapItem mapItem => mapItem -> IO (Id EKStructuredLocation)
locationWithMapItem mapItem =
  do
    cls' <- getRequiredClass "EKStructuredLocation"
    withObjCPtr mapItem $ \raw_mapItem ->
      sendClassMsg cls' (mkSelector "locationWithMapItem:") (retPtr retVoid) [argPtr (castPtr raw_mapItem :: Ptr ())] >>= retainedObject . castPtr

-- | @- title@
title :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> IO (Id NSString)
title ekStructuredLocation  =
  sendMsg ekStructuredLocation (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsEKStructuredLocation ekStructuredLocation, IsNSString value) => ekStructuredLocation -> value -> IO ()
setTitle ekStructuredLocation  value =
withObjCPtr value $ \raw_value ->
    sendMsg ekStructuredLocation (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- radius@
radius :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> IO CDouble
radius ekStructuredLocation  =
  sendMsg ekStructuredLocation (mkSelector "radius") retCDouble []

-- | @- setRadius:@
setRadius :: IsEKStructuredLocation ekStructuredLocation => ekStructuredLocation -> CDouble -> IO ()
setRadius ekStructuredLocation  value =
  sendMsg ekStructuredLocation (mkSelector "setRadius:") retVoid [argCDouble (fromIntegral value)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @locationWithTitle:@
locationWithTitleSelector :: Selector
locationWithTitleSelector = mkSelector "locationWithTitle:"

-- | @Selector@ for @locationWithMapItem:@
locationWithMapItemSelector :: Selector
locationWithMapItemSelector = mkSelector "locationWithMapItem:"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @radius@
radiusSelector :: Selector
radiusSelector = mkSelector "radius"

-- | @Selector@ for @setRadius:@
setRadiusSelector :: Selector
setRadiusSelector = mkSelector "setRadius:"

