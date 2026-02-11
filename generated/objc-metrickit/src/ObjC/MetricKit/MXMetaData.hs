{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXMetaData
--
-- A class that contains miscellaneous metadata about an associated payload.
--
-- Generated bindings for @MXMetaData@.
module ObjC.MetricKit.MXMetaData
  ( MXMetaData
  , IsMXMetaData(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , regionFormat
  , osVersion
  , deviceType
  , applicationBuildVersion
  , platformArchitecture
  , pid
  , bundleIdentifier
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , regionFormatSelector
  , osVersionSelector
  , deviceTypeSelector
  , applicationBuildVersionSelector
  , platformArchitectureSelector
  , pidSelector
  , bundleIdentifierSelector


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

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this metadata.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSData)
jsonRepresentation mxMetaData  =
  sendMsg mxMetaData (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | DictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this metadata.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- DictionaryRepresentation@
dictionaryRepresentation :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSDictionary)
dictionaryRepresentation mxMetaData  =
  sendMsg mxMetaData (mkSelector "DictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | regionFormat
--
-- An NSString designating the region format associated with the application.
--
-- ObjC selector: @- regionFormat@
regionFormat :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
regionFormat mxMetaData  =
  sendMsg mxMetaData (mkSelector "regionFormat") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | osVersion
--
-- An NSString designating the OS version associated with the device.
--
-- ObjC selector: @- osVersion@
osVersion :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
osVersion mxMetaData  =
  sendMsg mxMetaData (mkSelector "osVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | deviceType
--
-- An NSString designating the device type associated with this device.
--
-- ObjC selector: @- deviceType@
deviceType :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
deviceType mxMetaData  =
  sendMsg mxMetaData (mkSelector "deviceType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | applicationBuildVersion
--
-- An NSString designating the app build version.
--
-- ObjC selector: @- applicationBuildVersion@
applicationBuildVersion :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
applicationBuildVersion mxMetaData  =
  sendMsg mxMetaData (mkSelector "applicationBuildVersion") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | platformArchitecture
--
-- An NSString designating the current architecture.
--
-- ObjC selector: @- platformArchitecture@
platformArchitecture :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
platformArchitecture mxMetaData  =
  sendMsg mxMetaData (mkSelector "platformArchitecture") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | pid
--
-- pid of the process
--
-- Note: A value of -1 indicates that the PID was unavailable for the containing payload.
--
-- ObjC selector: @- pid@
pid :: IsMXMetaData mxMetaData => mxMetaData -> IO CInt
pid mxMetaData  =
  sendMsg mxMetaData (mkSelector "pid") retCInt []

-- | bundleIdentifier
--
-- String representation of the bundle ID of the process.
--
-- ObjC selector: @- bundleIdentifier@
bundleIdentifier :: IsMXMetaData mxMetaData => mxMetaData -> IO (Id NSString)
bundleIdentifier mxMetaData  =
  sendMsg mxMetaData (mkSelector "bundleIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @DictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "DictionaryRepresentation"

-- | @Selector@ for @regionFormat@
regionFormatSelector :: Selector
regionFormatSelector = mkSelector "regionFormat"

-- | @Selector@ for @osVersion@
osVersionSelector :: Selector
osVersionSelector = mkSelector "osVersion"

-- | @Selector@ for @deviceType@
deviceTypeSelector :: Selector
deviceTypeSelector = mkSelector "deviceType"

-- | @Selector@ for @applicationBuildVersion@
applicationBuildVersionSelector :: Selector
applicationBuildVersionSelector = mkSelector "applicationBuildVersion"

-- | @Selector@ for @platformArchitecture@
platformArchitectureSelector :: Selector
platformArchitectureSelector = mkSelector "platformArchitecture"

-- | @Selector@ for @pid@
pidSelector :: Selector
pidSelector = mkSelector "pid"

-- | @Selector@ for @bundleIdentifier@
bundleIdentifierSelector :: Selector
bundleIdentifierSelector = mkSelector "bundleIdentifier"

