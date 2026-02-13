{-# LANGUAGE DataKinds #-}
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
  , explicitlyIncludedSelector
  , initSelector
  , keySelector
  , localizedKeySelector
  , setKeySelector
  , setLocalizedKeySelector
  , setValueSelector
  , valueSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSDictionaryControllerKeyValuePair)
init_ nsDictionaryControllerKeyValuePair =
  sendOwnedMessage nsDictionaryControllerKeyValuePair initSelector

-- | @- key@
key :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSString)
key nsDictionaryControllerKeyValuePair =
  sendMessage nsDictionaryControllerKeyValuePair keySelector

-- | @- setKey:@
setKey :: (IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair, IsNSString value) => nsDictionaryControllerKeyValuePair -> value -> IO ()
setKey nsDictionaryControllerKeyValuePair value =
  sendMessage nsDictionaryControllerKeyValuePair setKeySelector (toNSString value)

-- | @- value@
value :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO RawId
value nsDictionaryControllerKeyValuePair =
  sendMessage nsDictionaryControllerKeyValuePair valueSelector

-- | @- setValue:@
setValue :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> RawId -> IO ()
setValue nsDictionaryControllerKeyValuePair value =
  sendMessage nsDictionaryControllerKeyValuePair setValueSelector value

-- | @- localizedKey@
localizedKey :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO (Id NSString)
localizedKey nsDictionaryControllerKeyValuePair =
  sendMessage nsDictionaryControllerKeyValuePair localizedKeySelector

-- | @- setLocalizedKey:@
setLocalizedKey :: (IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair, IsNSString value) => nsDictionaryControllerKeyValuePair -> value -> IO ()
setLocalizedKey nsDictionaryControllerKeyValuePair value =
  sendMessage nsDictionaryControllerKeyValuePair setLocalizedKeySelector (toNSString value)

-- | @- explicitlyIncluded@
explicitlyIncluded :: IsNSDictionaryControllerKeyValuePair nsDictionaryControllerKeyValuePair => nsDictionaryControllerKeyValuePair -> IO Bool
explicitlyIncluded nsDictionaryControllerKeyValuePair =
  sendMessage nsDictionaryControllerKeyValuePair explicitlyIncludedSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSDictionaryControllerKeyValuePair)
initSelector = mkSelector "init"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @setKey:@
setKeySelector :: Selector '[Id NSString] ()
setKeySelector = mkSelector "setKey:"

-- | @Selector@ for @value@
valueSelector :: Selector '[] RawId
valueSelector = mkSelector "value"

-- | @Selector@ for @setValue:@
setValueSelector :: Selector '[RawId] ()
setValueSelector = mkSelector "setValue:"

-- | @Selector@ for @localizedKey@
localizedKeySelector :: Selector '[] (Id NSString)
localizedKeySelector = mkSelector "localizedKey"

-- | @Selector@ for @setLocalizedKey:@
setLocalizedKeySelector :: Selector '[Id NSString] ()
setLocalizedKeySelector = mkSelector "setLocalizedKey:"

-- | @Selector@ for @explicitlyIncluded@
explicitlyIncludedSelector :: Selector '[] Bool
explicitlyIncludedSelector = mkSelector "explicitlyIncluded"

