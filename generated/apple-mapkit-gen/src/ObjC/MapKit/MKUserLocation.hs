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
  , updatingSelector
  , headingSelector
  , titleSelector
  , setTitleSelector
  , subtitleSelector
  , setSubtitleSelector


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

-- | @- updating@
updating :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO Bool
updating mkUserLocation  =
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg mkUserLocation (mkSelector "updating") retCULong []

-- | @- heading@
heading :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO RawId
heading mkUserLocation  =
    fmap (RawId . castPtr) $ sendMsg mkUserLocation (mkSelector "heading") (retPtr retVoid) []

-- | @- title@
title :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO (Id NSString)
title mkUserLocation  =
    sendMsg mkUserLocation (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsMKUserLocation mkUserLocation, IsNSString value) => mkUserLocation -> value -> IO ()
setTitle mkUserLocation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkUserLocation (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsMKUserLocation mkUserLocation => mkUserLocation -> IO (Id NSString)
subtitle mkUserLocation  =
    sendMsg mkUserLocation (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsMKUserLocation mkUserLocation, IsNSString value) => mkUserLocation -> value -> IO ()
setSubtitle mkUserLocation  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mkUserLocation (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @updating@
updatingSelector :: Selector
updatingSelector = mkSelector "updating"

-- | @Selector@ for @heading@
headingSelector :: Selector
headingSelector = mkSelector "heading"

-- | @Selector@ for @title@
titleSelector :: Selector
titleSelector = mkSelector "title"

-- | @Selector@ for @setTitle:@
setTitleSelector :: Selector
setTitleSelector = mkSelector "setTitle:"

-- | @Selector@ for @subtitle@
subtitleSelector :: Selector
subtitleSelector = mkSelector "subtitle"

-- | @Selector@ for @setSubtitle:@
setSubtitleSelector :: Selector
setSubtitleSelector = mkSelector "setSubtitle:"

