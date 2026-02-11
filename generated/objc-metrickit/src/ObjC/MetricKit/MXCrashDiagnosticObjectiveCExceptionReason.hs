{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCrashDiagnosticObjectiveCExceptionReason
--
-- A class that represents Crash exception reason.
--
-- Crash reports that are caused by an uncaught Objective-C NSException can in some cases contain detailed information about the type, name and description of the exception object.                This information is captured in a structured way in a MXCrashDiagnosticObjectiveCExceptionReason object and may have some pieces redacted to avoid exposing sensitive user data.
--
-- Generated bindings for @MXCrashDiagnosticObjectiveCExceptionReason@.
module ObjC.MetricKit.MXCrashDiagnosticObjectiveCExceptionReason
  ( MXCrashDiagnosticObjectiveCExceptionReason
  , IsMXCrashDiagnosticObjectiveCExceptionReason(..)
  , jsonRepresentation
  , dictionaryRepresentation
  , composedMessage
  , formatString
  , arguments
  , exceptionType
  , className
  , exceptionName
  , jsonRepresentationSelector
  , dictionaryRepresentationSelector
  , composedMessageSelector
  , formatStringSelector
  , argumentsSelector
  , exceptionTypeSelector
  , classNameSelector
  , exceptionNameSelector


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
-- Convenience method to return a JSON representation of this MXCrashDiagnosticObjectiveCExceptionReason object.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSData)
jsonRepresentation mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this MXCrashDiagnosticObjectiveCExceptionReason object.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSDictionary)
dictionaryRepresentation mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "dictionaryRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | composedMessage
--
-- A human-readable message string summarizing the reason for the exception.
--
-- ObjC selector: @- composedMessage@
composedMessage :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
composedMessage mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "composedMessage") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | formatString
--
-- A string representing the exception message before arguments are substituted into the message
--
-- ObjC selector: @- formatString@
formatString :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
formatString mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "formatString") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | arguments
--
-- An NSArray of strings representing arguments passed to the formatString.
--
-- ObjC selector: @- arguments@
arguments :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSArray)
arguments mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "arguments") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionType
--
-- A human-readable string denoting type of the exception
--
-- ObjC selector: @- exceptionType@
exceptionType :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
exceptionType mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "exceptionType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | className
--
-- A string representing the class name of the exception, for example NSException.
--
-- ObjC selector: @- className@
className :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
className mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "className") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | exceptionName
--
-- A string representing name of the exception
--
-- This will align with the "name" field of the NSException
--
-- ObjC selector: @- exceptionName@
exceptionName :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
exceptionName mxCrashDiagnosticObjectiveCExceptionReason  =
  sendMsg mxCrashDiagnosticObjectiveCExceptionReason (mkSelector "exceptionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @composedMessage@
composedMessageSelector :: Selector
composedMessageSelector = mkSelector "composedMessage"

-- | @Selector@ for @formatString@
formatStringSelector :: Selector
formatStringSelector = mkSelector "formatString"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @exceptionType@
exceptionTypeSelector :: Selector
exceptionTypeSelector = mkSelector "exceptionType"

-- | @Selector@ for @className@
classNameSelector :: Selector
classNameSelector = mkSelector "className"

-- | @Selector@ for @exceptionName@
exceptionNameSelector :: Selector
exceptionNameSelector = mkSelector "exceptionName"

