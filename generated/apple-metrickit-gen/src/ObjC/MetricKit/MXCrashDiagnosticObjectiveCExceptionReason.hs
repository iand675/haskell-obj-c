{-# LANGUAGE DataKinds #-}
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
  , argumentsSelector
  , classNameSelector
  , composedMessageSelector
  , dictionaryRepresentationSelector
  , exceptionNameSelector
  , exceptionTypeSelector
  , formatStringSelector
  , jsonRepresentationSelector


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
-- Convenience method to return a JSON representation of this MXCrashDiagnosticObjectiveCExceptionReason object.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSData)
jsonRepresentation mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason jsonRepresentationSelector

-- | dictionaryRepresentation
--
-- Convenience method to return a NSDictionary representation of this MXCrashDiagnosticObjectiveCExceptionReason object.
--
-- Returns: An NSDictionary object containing the dictionary representation
--
-- ObjC selector: @- dictionaryRepresentation@
dictionaryRepresentation :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSDictionary)
dictionaryRepresentation mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason dictionaryRepresentationSelector

-- | composedMessage
--
-- A human-readable message string summarizing the reason for the exception.
--
-- ObjC selector: @- composedMessage@
composedMessage :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
composedMessage mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason composedMessageSelector

-- | formatString
--
-- A string representing the exception message before arguments are substituted into the message
--
-- ObjC selector: @- formatString@
formatString :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
formatString mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason formatStringSelector

-- | arguments
--
-- An NSArray of strings representing arguments passed to the formatString.
--
-- ObjC selector: @- arguments@
arguments :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSArray)
arguments mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason argumentsSelector

-- | exceptionType
--
-- A human-readable string denoting type of the exception
--
-- ObjC selector: @- exceptionType@
exceptionType :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
exceptionType mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason exceptionTypeSelector

-- | className
--
-- A string representing the class name of the exception, for example NSException.
--
-- ObjC selector: @- className@
className :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
className mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason classNameSelector

-- | exceptionName
--
-- A string representing name of the exception
--
-- This will align with the "name" field of the NSException
--
-- ObjC selector: @- exceptionName@
exceptionName :: IsMXCrashDiagnosticObjectiveCExceptionReason mxCrashDiagnosticObjectiveCExceptionReason => mxCrashDiagnosticObjectiveCExceptionReason -> IO (Id NSString)
exceptionName mxCrashDiagnosticObjectiveCExceptionReason =
  sendMessage mxCrashDiagnosticObjectiveCExceptionReason exceptionNameSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

-- | @Selector@ for @dictionaryRepresentation@
dictionaryRepresentationSelector :: Selector '[] (Id NSDictionary)
dictionaryRepresentationSelector = mkSelector "dictionaryRepresentation"

-- | @Selector@ for @composedMessage@
composedMessageSelector :: Selector '[] (Id NSString)
composedMessageSelector = mkSelector "composedMessage"

-- | @Selector@ for @formatString@
formatStringSelector :: Selector '[] (Id NSString)
formatStringSelector = mkSelector "formatString"

-- | @Selector@ for @arguments@
argumentsSelector :: Selector '[] (Id NSArray)
argumentsSelector = mkSelector "arguments"

-- | @Selector@ for @exceptionType@
exceptionTypeSelector :: Selector '[] (Id NSString)
exceptionTypeSelector = mkSelector "exceptionType"

-- | @Selector@ for @className@
classNameSelector :: Selector '[] (Id NSString)
classNameSelector = mkSelector "className"

-- | @Selector@ for @exceptionName@
exceptionNameSelector :: Selector '[] (Id NSString)
exceptionNameSelector = mkSelector "exceptionName"

