{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MERAWProcessingParameter
--
-- An object implementing this protocol is implemented by the RAW Processor to describe each processing parameter the Processor exposes.
--
-- The MERAWProcessingParameter protocol provides an interface for the VideoToolbox to query descriptions of the different parameters that can be used to influence Processor operation.  A distinct MERAWProcessingParameter is created for each parameter supported by the Processor, and the set of supported parameters is returned by the MERAWProcessor's processingParameters interface.
--
-- Generated bindings for @MERAWProcessingParameter@.
module ObjC.MediaExtension.MERAWProcessingParameter
  ( MERAWProcessingParameter
  , IsMERAWProcessingParameter(..)
  , name
  , key
  , longDescription
  , enabled
  , setEnabled
  , enabledSelector
  , keySelector
  , longDescriptionSelector
  , nameSelector
  , setEnabledSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MediaExtension.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | name
--
-- A localized human-readable name for the parameter, suitable for displaying in application UI.
--
-- ObjC selector: @- name@
name :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
name merawProcessingParameter =
  sendMessage merawProcessingParameter nameSelector

-- | key
--
-- A unique key string identifying this parameter.
--
-- ObjC selector: @- key@
key :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
key merawProcessingParameter =
  sendMessage merawProcessingParameter keySelector

-- | longDescription
--
-- A localized description of the parameter, suitable for displaying in a tool tip or similar explanatory UI.
--
-- ObjC selector: @- longDescription@
longDescription :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO (Id NSString)
longDescription merawProcessingParameter =
  sendMessage merawProcessingParameter longDescriptionSelector

-- | enabled
--
-- Indicates whether the parameter is enabled or disabled by the extension.
--
-- This parameter can only be modified by the extension.  From the application-facing interface, VTRAWProcessingSession, this is a read-only value which indicates whether the parameter should be greyed out and disabled in any UI being generated.
--
-- ObjC selector: @- enabled@
enabled :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> IO Bool
enabled merawProcessingParameter =
  sendMessage merawProcessingParameter enabledSelector

-- | enabled
--
-- Indicates whether the parameter is enabled or disabled by the extension.
--
-- This parameter can only be modified by the extension.  From the application-facing interface, VTRAWProcessingSession, this is a read-only value which indicates whether the parameter should be greyed out and disabled in any UI being generated.
--
-- ObjC selector: @- setEnabled:@
setEnabled :: IsMERAWProcessingParameter merawProcessingParameter => merawProcessingParameter -> Bool -> IO ()
setEnabled merawProcessingParameter value =
  sendMessage merawProcessingParameter setEnabledSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @key@
keySelector :: Selector '[] (Id NSString)
keySelector = mkSelector "key"

-- | @Selector@ for @longDescription@
longDescriptionSelector :: Selector '[] (Id NSString)
longDescriptionSelector = mkSelector "longDescription"

-- | @Selector@ for @enabled@
enabledSelector :: Selector '[] Bool
enabledSelector = mkSelector "enabled"

-- | @Selector@ for @setEnabled:@
setEnabledSelector :: Selector '[Bool] ()
setEnabledSelector = mkSelector "setEnabled:"

