{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSDictionaryControllerKeyValuePair@.
module ObjC.AppKit.NSDictionaryControllerKeyValuePair
  ( NSDictionaryControllerKeyValuePair
  , IsNSDictionaryControllerKeyValuePair(..)
  , init_
  , key
  , setKey
  , value
  , setValue
  , localizedKey
  , setLocalizedKey
  , explicitlyIncluded
  , initSelector
  , keySelector
  , setKeySelector
  , valueSelector
  , setValueSelector
  , localizedKeySelector
  , setLocalizedKeySelector
  , explicitlyIncludedSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSDictionaryControllerKeyValuePair)
init_ nsDictionaryControllerKeyValuePair  =
  sendMsg nsDictionaryControllerKeyValuePair (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- key@
key :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSString)
key nsDictionaryControllerKeyValuePair  =
  sendMsg nsDictionaryControllerKeyValuePair (mkSelector "key") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setKey:@
setKey :: (IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair, IsNSString value) => nsDictionaryControllerKeyValuePair -> value -> IO ()
setKey nsDictionaryControllerKeyValuePair  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryControllerKeyValuePair (mkSelector "setKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- value@
value :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO RawId
value nsDictionaryControllerKeyValuePair  =
  fmap (RawId . castPtr) $ sendMsg nsDictionaryControllerKeyValuePair (mkSelector "value") (retPtr retVoid) []

-- | @- setValue:@
setValue :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> RawId -> IO ()
setValue nsDictionaryControllerKeyValuePair  value =
  sendMsg nsDictionaryControllerKeyValuePair (mkSelector "setValue:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- | @- localizedKey@
localizedKey :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSString)
localizedKey nsDictionaryControllerKeyValuePair  =
  sendMsg nsDictionaryControllerKeyValuePair (mkSelector "localizedKey") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setLocalizedKey:@
setLocalizedKey :: (IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair, IsNSString value) => nsDictionaryControllerKeyValuePair -> value -> IO ()
setLocalizedKey nsDictionaryControllerKeyValuePair  value =
withObjCPtr value $ \raw_value ->
    sendMsg nsDictionaryControllerKeyValuePair (mkSelector "setLocalizedKey:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | @- explicitlyIncluded@
explicitlyIncluded :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO Bool
explicitlyIncluded nsDictionaryControllerKeyValuePair  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsDictionaryControllerKeyValuePair (mkSelector "explicitlyIncluded") retCULong []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @value@
valueSelector :: Selector
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @localizedKey@
localizedKeySelector :: Selector
localizedKeySelector = mkSelector "localizedKey"

-- | @Selector@ for @setLocalizedKey:@
setLocalizedKeySelector :: Selector
setLocalizedKeySelector = mkSelector "setLocalizedKey:"

-- | @Selector@ for @explicitlyIncluded@
explicitlyIncludedSelector :: Selector
explicitlyIncludedSelector = mkSelector "explicitlyIncluded"

