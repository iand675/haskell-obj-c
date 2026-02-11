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

-- | @- title@
title :: IsMKShape mkShape => mkShape -> IO (Id NSString)
title mkShape  =
  sendMsg mkShape (mkSelector "title") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setTitle:@
setTitle :: (IsMKShape mkShape, IsNSString value) => mkShape -> value -> IO ()
setTitle mkShape  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkShape (mkSelector "setTitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- subtitle@
subtitle :: IsMKShape mkShape => mkShape -> IO (Id NSString)
subtitle mkShape  =
  sendMsg mkShape (mkSelector "subtitle") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setSubtitle:@
setSubtitle :: (IsMKShape mkShape, IsNSString value) => mkShape -> value -> IO ()
setSubtitle mkShape  value =
withObjCPtr value $ \raw_value ->
    sendMsg mkShape (mkSelector "setSubtitle:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

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

