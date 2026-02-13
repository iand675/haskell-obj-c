{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUAudioUnitPreset
--
-- A collection of parameter settings provided by the audio unit implementor, producing a			useful sound or starting point.
--
-- Generated bindings for @AUAudioUnitPreset@.
module ObjC.AudioToolbox.AUAudioUnitPreset
  ( AUAudioUnitPreset
  , IsAUAudioUnitPreset(..)
  , number
  , setNumber
  , name
  , setName
  , nameSelector
  , numberSelector
  , setNameSelector
  , setNumberSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | number
--
-- The preset's unique numeric identifier.
--
-- ObjC selector: @- number@
number :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> IO CLong
number auAudioUnitPreset =
  sendMessage auAudioUnitPreset numberSelector

-- | number
--
-- The preset's unique numeric identifier.
--
-- ObjC selector: @- setNumber:@
setNumber :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> CLong -> IO ()
setNumber auAudioUnitPreset value =
  sendMessage auAudioUnitPreset setNumberSelector value

-- | name
--
-- The preset's name.
--
-- ObjC selector: @- name@
name :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> IO (Id NSString)
name auAudioUnitPreset =
  sendMessage auAudioUnitPreset nameSelector

-- | name
--
-- The preset's name.
--
-- ObjC selector: @- setName:@
setName :: (IsAUAudioUnitPreset auAudioUnitPreset, IsNSString value) => auAudioUnitPreset -> value -> IO ()
setName auAudioUnitPreset value =
  sendMessage auAudioUnitPreset setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @number@
numberSelector :: Selector '[] CLong
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector '[CLong] ()
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

