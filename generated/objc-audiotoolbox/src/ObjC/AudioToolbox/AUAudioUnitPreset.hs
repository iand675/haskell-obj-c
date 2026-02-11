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
  , numberSelector
  , setNumberSelector
  , nameSelector
  , setNameSelector


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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | number
--
-- The preset's unique numeric identifier.
--
-- ObjC selector: @- number@
number :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> IO CLong
number auAudioUnitPreset  =
  sendMsg auAudioUnitPreset (mkSelector "number") retCLong []

-- | number
--
-- The preset's unique numeric identifier.
--
-- ObjC selector: @- setNumber:@
setNumber :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> CLong -> IO ()
setNumber auAudioUnitPreset  value =
  sendMsg auAudioUnitPreset (mkSelector "setNumber:") retVoid [argCLong (fromIntegral value)]

-- | name
--
-- The preset's name.
--
-- ObjC selector: @- name@
name :: IsAUAudioUnitPreset auAudioUnitPreset => auAudioUnitPreset -> IO (Id NSString)
name auAudioUnitPreset  =
  sendMsg auAudioUnitPreset (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | name
--
-- The preset's name.
--
-- ObjC selector: @- setName:@
setName :: (IsAUAudioUnitPreset auAudioUnitPreset, IsNSString value) => auAudioUnitPreset -> value -> IO ()
setName auAudioUnitPreset  value =
withObjCPtr value $ \raw_value ->
    sendMsg auAudioUnitPreset (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @number@
numberSelector :: Selector
numberSelector = mkSelector "number"

-- | @Selector@ for @setNumber:@
setNumberSelector :: Selector
setNumberSelector = mkSelector "setNumber:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

