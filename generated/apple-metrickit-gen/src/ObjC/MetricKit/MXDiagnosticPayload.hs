{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXDiagnosticPayload
--
-- A wrapper class which contains a diagnostic payload and associated properties of that payload.
--
-- MXDiagnosticPayload encapsulates currently supported diagnostics that can be vended by MetricKit. Arrays of MXDiangostic subclasses on MXDiagnosticPayload are nullable. If an array of MXDiagnostic subclasses is nil, it indicates that the diagnostics are not available for this payload.
--
-- MXDiagnosticPayload exposes a convenience function, JSONRepresentation, to convert the contents of the payload to a human readable JSON. This should be used in conjunction with other APIs that accept NSData.
--
-- An MXDiagnosticPayload contains diagnostics that cover a 24 hour period of application usage. The properties timeStampBegin and timeStampEnd should be used to determine which time range the payload covers.
--
-- It is possible for an MXDiagnosticPayload to cover regions of time where an application was updated, and thus each MXDiagnostic subclass will contain its own application version string. This is in contrast to MXMetricPayload, where only the latest application version string is included as metadata of the payload. Each MXDiagnostic subclass application version string should be inspected prior to processing.
--
-- Generated bindings for @MXDiagnosticPayload@.
module ObjC.MetricKit.MXDiagnosticPayload
  ( MXDiagnosticPayload
  , IsMXDiagnosticPayload(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , cpuExceptionDiagnostics
  , diskWriteExceptionDiagnostics
  , hangDiagnostics
  , appLaunchDiagnostics
  , crashDiagnostics
  , timeStampBegin
  , timeStampEnd
  , appLaunchDiagnosticsSelector
  , cpuExceptionDiagnosticsSelector
  , crashDiagnosticsSelector
  , dictionaryRepresentationSelector
  , diskWriteExceptionDiagnosticsSelector
  , hangDiagnosticsSelector
  , jsonRepresentationSelector
  , timeStampBeginSelector
  , timeStampEndSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this diagnostic payload.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSData)
jsonRepresentation mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload jsonRepresentationSelector

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this diagnostic payload.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDictionary)
dictionaryRepresentation mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload dictionaryRepresentationSelector

-- | cpuExceptionDiagnostics
--
-- An array containing CPU exception diagnostics for this application.
--
-- ObjC selector: @- cpuExceptionDiagnostics@
cpuExceptionDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
cpuExceptionDiagnostics mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload cpuExceptionDiagnosticsSelector

-- | diskWriteExceptionDiagnostics
--
-- An array containing disk write exception diagnostics for this application.
--
-- ObjC selector: @- diskWriteExceptionDiagnostics@
diskWriteExceptionDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
diskWriteExceptionDiagnostics mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload diskWriteExceptionDiagnosticsSelector

-- | hangDiagnostics
--
-- An array containing hang diagnostics for this application.
--
-- ObjC selector: @- hangDiagnostics@
hangDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
hangDiagnostics mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload hangDiagnosticsSelector

-- | appLaunchDiagnostics
--
-- An array containing app launch diagnostics for this application.
--
-- ObjC selector: @- appLaunchDiagnostics@
appLaunchDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
appLaunchDiagnostics mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload appLaunchDiagnosticsSelector

-- | crashDiagnostics
--
-- An array containing crash diagnostics for this application.
--
-- ObjC selector: @- crashDiagnostics@
crashDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
crashDiagnostics mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload crashDiagnosticsSelector

-- | timeStampBegin
--
-- An NSDate object that indicates the start time for which the payload was generated.
--
-- ObjC selector: @- timeStampBegin@
timeStampBegin :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDate)
timeStampBegin mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload timeStampBeginSelector

-- | timeStampEnd
--
-- An NSDate object that indicates the end time for which the payload was generated.
--
-- ObjC selector: @- timeStampEnd@
timeStampEnd :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDate)
timeStampEnd mxDiagnosticPayload =
  sendMessage mxDiagnosticPayload timeStampEndSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @cpuExceptionDiagnostics@
cpuExceptionDiagnosticsSelector :: Selector '[] (Id NSArray)
cpuExceptionDiagnosticsSelector = mkSelector "cpuExceptionDiagnostics"

-- | @Selector@ for @diskWriteExceptionDiagnostics@
diskWriteExceptionDiagnosticsSelector :: Selector '[] (Id NSArray)
diskWriteExceptionDiagnosticsSelector = mkSelector "diskWriteExceptionDiagnostics"

-- | @Selector@ for @hangDiagnostics@
hangDiagnosticsSelector :: Selector '[] (Id NSArray)
hangDiagnosticsSelector = mkSelector "hangDiagnostics"

-- | @Selector@ for @appLaunchDiagnostics@
appLaunchDiagnosticsSelector :: Selector '[] (Id NSArray)
appLaunchDiagnosticsSelector = mkSelector "appLaunchDiagnostics"

-- | @Selector@ for @crashDiagnostics@
crashDiagnosticsSelector :: Selector '[] (Id NSArray)
crashDiagnosticsSelector = mkSelector "crashDiagnostics"

-- | @Selector@ for @timeStampBegin@
timeStampBeginSelector :: Selector '[] (Id NSDate)
timeStampBeginSelector = mkSelector "timeStampBegin"

-- | @Selector@ for @timeStampEnd@
timeStampEndSelector :: Selector '[] (Id NSDate)
timeStampEndSelector = mkSelector "timeStampEnd"

