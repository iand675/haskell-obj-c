{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionConversionValidator
--
-- Performs a validation of captions for a conversion operation and warns about problems that are encountered.
--
-- Generated bindings for @AVCaptionConversionValidator@.
module ObjC.AVFoundation.AVCaptionConversionValidator
  ( AVCaptionConversionValidator
  , IsAVCaptionConversionValidator(..)
  , init_
  , new
  , validateCaptionConversionWithWarningHandler
  , stopValidating
  , status
  , captions
  , warnings
  , initSelector
  , newSelector
  , validateCaptionConversionWithWarningHandlerSelector
  , stopValidatingSelector
  , statusSelector
  , captionsSelector
  , warningsSelector

  -- * Enum types
  , AVCaptionConversionValidatorStatus(AVCaptionConversionValidatorStatus)
  , pattern AVCaptionConversionValidatorStatusUnknown
  , pattern AVCaptionConversionValidatorStatusValidating
  , pattern AVCaptionConversionValidatorStatusCompleted
  , pattern AVCaptionConversionValidatorStatusStopped

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

import ObjC.AVFoundation.Internal.Classes
import ObjC.AVFoundation.Internal.Enums
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> IO (Id AVCaptionConversionValidator)
init_ avCaptionConversionValidator  =
  sendMsg avCaptionConversionValidator (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionConversionValidator)
new  =
  do
    cls' <- getRequiredClass "AVCaptionConversionValidator"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | validateCaptionConversionWithWarningHandler:
--
-- Initiates the specified validation and changes the value of status to AVCaptionConversionValidatorStatusValidating.
--
-- @handler@ — Specifies a block to be executed in order to warn you of a specific problem.
--
-- It is an error to invoke this method when the value of status is greater than AVCaptionConversionValidatorStatusUnknown.   If you wish to stop a validation operation in progress before it has been completed, send the message stopValidating to the receiver.   When the validation is complete and all warnings have been reported, the block will be executed once with a value of nil for its warning parameter. When this occurs, the value of status will have been changed to AVCaptionConversionValidatorStatusCompleted.
--
-- ObjC selector: @- validateCaptionConversionWithWarningHandler:@
validateCaptionConversionWithWarningHandler :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> Ptr () -> IO ()
validateCaptionConversionWithWarningHandler avCaptionConversionValidator  handler =
  sendMsg avCaptionConversionValidator (mkSelector "validateCaptionConversionWithWarningHandler:") retVoid [argPtr (castPtr handler :: Ptr ())]

-- | stopValidating
--
-- Stops validation and changes the value of status to AVCaptionConversionValidatorStatusStopped.
--
-- You can call this method at any time, even within your warning handler.
--
-- ObjC selector: @- stopValidating@
stopValidating :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> IO ()
stopValidating avCaptionConversionValidator  =
  sendMsg avCaptionConversionValidator (mkSelector "stopValidating") retVoid []

-- | status
--
-- Indicates the status of the validation.
--
-- ObjC selector: @- status@
status :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> IO AVCaptionConversionValidatorStatus
status avCaptionConversionValidator  =
  fmap (coerce :: CLong -> AVCaptionConversionValidatorStatus) $ sendMsg avCaptionConversionValidator (mkSelector "status") retCLong []

-- | captions
--
-- The array of captions to be validated for the specified conversion operation.
--
-- ObjC selector: @- captions@
captions :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> IO (Id NSArray)
captions avCaptionConversionValidator  =
  sendMsg avCaptionConversionValidator (mkSelector "captions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | warnings
--
-- Provides the collection of warnings for problems that have been encountered. While the value of status is AVCaptionConversionValidatorStatusValidating, the count of warnings may increase.
--
-- ObjC selector: @- warnings@
warnings :: IsAVCaptionConversionValidator avCaptionConversionValidator => avCaptionConversionValidator -> IO (Id NSArray)
warnings avCaptionConversionValidator  =
  sendMsg avCaptionConversionValidator (mkSelector "warnings") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @validateCaptionConversionWithWarningHandler:@
validateCaptionConversionWithWarningHandlerSelector :: Selector
validateCaptionConversionWithWarningHandlerSelector = mkSelector "validateCaptionConversionWithWarningHandler:"

-- | @Selector@ for @stopValidating@
stopValidatingSelector :: Selector
stopValidatingSelector = mkSelector "stopValidating"

-- | @Selector@ for @status@
statusSelector :: Selector
statusSelector = mkSelector "status"

-- | @Selector@ for @captions@
captionsSelector :: Selector
captionsSelector = mkSelector "captions"

-- | @Selector@ for @warnings@
warningsSelector :: Selector
warningsSelector = mkSelector "warnings"

