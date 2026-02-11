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
  , crashDiagnostics
  , timeStampBegin
  , timeStampEnd
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , cpuExceptionDiagnosticsSelector
  , diskWriteExceptionDiagnosticsSelector
  , hangDiagnosticsSelector
  , crashDiagnosticsSelector
  , timeStampBeginSelector
  , timeStampEndSelector


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
-- Convenience method to return a JSON representation of this diagnostic payload.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSData)
jsonRepresentation mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this diagnostic payload.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDictionary)
dictionaryRepresentation mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | cpuExceptionDiagnostics
--
-- An array containing CPU exception diagnostics for this application.
--
-- ObjC selector: @- cpuExceptionDiagnostics@
cpuExceptionDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
cpuExceptionDiagnostics mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "cpuExceptionDiagnostics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | diskWriteExceptionDiagnostics
--
-- An array containing disk write exception diagnostics for this application.
--
-- ObjC selector: @- diskWriteExceptionDiagnostics@
diskWriteExceptionDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
diskWriteExceptionDiagnostics mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "diskWriteExceptionDiagnostics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | hangDiagnostics
--
-- An array containing hang diagnostics for this application.
--
-- ObjC selector: @- hangDiagnostics@
hangDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
hangDiagnostics mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "hangDiagnostics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | crashDiagnostics
--
-- An array containing crash diagnostics for this application.
--
-- ObjC selector: @- crashDiagnostics@
crashDiagnostics :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSArray)
crashDiagnostics mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "crashDiagnostics") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | timeStampBegin
--
-- An NSDate object that indicates the start time for which the payload was generated.
--
-- ObjC selector: @- timeStampBegin@
timeStampBegin :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDate)
timeStampBegin mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "timeStampBegin") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | timeStampEnd
--
-- An NSDate object that indicates the end time for which the payload was generated.
--
-- ObjC selector: @- timeStampEnd@
timeStampEnd :: IsMXDiagnosticPayload mxDiagnosticPayload => mxDiagnosticPayload -> IO (Id NSDate)
timeStampEnd mxDiagnosticPayload  =
  sendMsg mxDiagnosticPayload (mkSelector "timeStampEnd") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @cpuExceptionDiagnostics@
cpuExceptionDiagnosticsSelector :: Selector
cpuExceptionDiagnosticsSelector = mkSelector "cpuExceptionDiagnostics"

-- | @Selector@ for @diskWriteExceptionDiagnostics@
diskWriteExceptionDiagnosticsSelector :: Selector
diskWriteExceptionDiagnosticsSelector = mkSelector "diskWriteExceptionDiagnostics"

-- | @Selector@ for @hangDiagnostics@
hangDiagnosticsSelector :: Selector
hangDiagnosticsSelector = mkSelector "hangDiagnostics"

-- | @Selector@ for @crashDiagnostics@
crashDiagnosticsSelector :: Selector
crashDiagnosticsSelector = mkSelector "crashDiagnostics"

-- | @Selector@ for @timeStampBegin@
timeStampBeginSelector :: Selector
timeStampBeginSelector = mkSelector "timeStampBegin"

-- | @Selector@ for @timeStampEnd@
timeStampEndSelector :: Selector
timeStampEndSelector = mkSelector "timeStampEnd"

