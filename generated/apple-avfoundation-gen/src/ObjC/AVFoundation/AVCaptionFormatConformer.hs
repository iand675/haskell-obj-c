{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AVCaptionFormatConformer
--
-- Performs a conversion of canonical caption to conform to a specific format.
--
-- Generated bindings for @AVCaptionFormatConformer@.
module ObjC.AVFoundation.AVCaptionFormatConformer
  ( AVCaptionFormatConformer
  , IsAVCaptionFormatConformer(..)
  , init_
  , new
  , captionFormatConformerWithConversionSettings
  , initWithConversionSettings
  , conformedCaptionForCaption_error
  , conformsCaptionsToTimeRange
  , setConformsCaptionsToTimeRange
  , captionFormatConformerWithConversionSettingsSelector
  , conformedCaptionForCaption_errorSelector
  , conformsCaptionsToTimeRangeSelector
  , initSelector
  , initWithConversionSettingsSelector
  , newSelector
  , setConformsCaptionsToTimeRangeSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AVFoundation.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> IO (Id AVCaptionFormatConformer)
init_ avCaptionFormatConformer =
  sendOwnedMessage avCaptionFormatConformer initSelector

-- | @+ new@
new :: IO (Id AVCaptionFormatConformer)
new  =
  do
    cls' <- getRequiredClass "AVCaptionFormatConformer"
    sendOwnedClassMessage cls' newSelector

-- | captionFormatConformerWithConversionSettings:conversionSettings:
--
-- Returns an instance of AVCaptionFormatConformer that can convert a canonical caption to conform to a specific format.
--
-- @conversionSettings@ — Describes the conversion operation for which the caption is to be conformed.
--
-- Returns: A new instance of AVCaptionFormatConformer configured to perform the specified conversion.
--
-- ObjC selector: @+ captionFormatConformerWithConversionSettings:@
captionFormatConformerWithConversionSettings :: IsNSDictionary conversionSettings => conversionSettings -> IO (Id AVCaptionFormatConformer)
captionFormatConformerWithConversionSettings conversionSettings =
  do
    cls' <- getRequiredClass "AVCaptionFormatConformer"
    sendClassMessage cls' captionFormatConformerWithConversionSettingsSelector (toNSDictionary conversionSettings)

-- | initWithConversionSettings:conversionSettings:
--
-- Returns an instance of AVCaptionFormatConformer that can convert a canonical caption to conform to a specific format.
--
-- @conversionSettings@ — Describes the conversion operation for which the caption is to be conformed.
--
-- Returns: A new instance of AVCaptionFormatConformer configured to perform the specified conversion.
--
-- This method throws an exception if the conversion setting's AVCaptionMediaTypeKey is not equal to AVMediaTypeClosedCaption, or if its AVCaptionMediaSubTypeKey is not equal to kCMClosedCaptionFormatType_CEA608.
--
-- ObjC selector: @- initWithConversionSettings:@
initWithConversionSettings :: (IsAVCaptionFormatConformer avCaptionFormatConformer, IsNSDictionary conversionSettings) => avCaptionFormatConformer -> conversionSettings -> IO (Id AVCaptionFormatConformer)
initWithConversionSettings avCaptionFormatConformer conversionSettings =
  sendOwnedMessage avCaptionFormatConformer initWithConversionSettingsSelector (toNSDictionary conversionSettings)

-- | conformedCaptionForCaption:error:
--
-- Creates a format-compliant caption that conforms to a specific format by converting a given canonical caption.
--
-- @caption@ — Specifies a canonical caption to be converted.
--
-- @outError@ — A pointer where a NSError object may be returned.
--
-- Returns: A format-compliant caption that conforms to a specific format.
--
-- ObjC selector: @- conformedCaptionForCaption:error:@
conformedCaptionForCaption_error :: (IsAVCaptionFormatConformer avCaptionFormatConformer, IsAVCaption caption, IsNSError outError) => avCaptionFormatConformer -> caption -> outError -> IO (Id AVCaption)
conformedCaptionForCaption_error avCaptionFormatConformer caption outError =
  sendMessage avCaptionFormatConformer conformedCaptionForCaption_errorSelector (toAVCaption caption) (toNSError outError)

-- | conformsCaptionsToTimeRange
--
-- Specifies whether to conform the time range of a given canonical caption as well.
--
-- When set to YES, conforms time range.	When set to NO, the time range of the conformed caption will be same as a given canonical caption.	In the case of conforming to CAE608 format, AVCaption is encoded so that each CAE608 control code (2 bytes) fits into 1 frame duration (1001/30000).	When set to YES and if all the encoded data can not fit inside the canonical caption time range, the caption time range will be extended to fit all the data and will be returned in the conformed AVCaption.	The default value is NO.
--
-- ObjC selector: @- conformsCaptionsToTimeRange@
conformsCaptionsToTimeRange :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> IO Bool
conformsCaptionsToTimeRange avCaptionFormatConformer =
  sendMessage avCaptionFormatConformer conformsCaptionsToTimeRangeSelector

-- | conformsCaptionsToTimeRange
--
-- Specifies whether to conform the time range of a given canonical caption as well.
--
-- When set to YES, conforms time range.	When set to NO, the time range of the conformed caption will be same as a given canonical caption.	In the case of conforming to CAE608 format, AVCaption is encoded so that each CAE608 control code (2 bytes) fits into 1 frame duration (1001/30000).	When set to YES and if all the encoded data can not fit inside the canonical caption time range, the caption time range will be extended to fit all the data and will be returned in the conformed AVCaption.	The default value is NO.
--
-- ObjC selector: @- setConformsCaptionsToTimeRange:@
setConformsCaptionsToTimeRange :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> Bool -> IO ()
setConformsCaptionsToTimeRange avCaptionFormatConformer value =
  sendMessage avCaptionFormatConformer setConformsCaptionsToTimeRangeSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVCaptionFormatConformer)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVCaptionFormatConformer)
newSelector = mkSelector "new"

-- | @Selector@ for @captionFormatConformerWithConversionSettings:@
captionFormatConformerWithConversionSettingsSelector :: Selector '[Id NSDictionary] (Id AVCaptionFormatConformer)
captionFormatConformerWithConversionSettingsSelector = mkSelector "captionFormatConformerWithConversionSettings:"

-- | @Selector@ for @initWithConversionSettings:@
initWithConversionSettingsSelector :: Selector '[Id NSDictionary] (Id AVCaptionFormatConformer)
initWithConversionSettingsSelector = mkSelector "initWithConversionSettings:"

-- | @Selector@ for @conformedCaptionForCaption:error:@
conformedCaptionForCaption_errorSelector :: Selector '[Id AVCaption, Id NSError] (Id AVCaption)
conformedCaptionForCaption_errorSelector = mkSelector "conformedCaptionForCaption:error:"

-- | @Selector@ for @conformsCaptionsToTimeRange@
conformsCaptionsToTimeRangeSelector :: Selector '[] Bool
conformsCaptionsToTimeRangeSelector = mkSelector "conformsCaptionsToTimeRange"

-- | @Selector@ for @setConformsCaptionsToTimeRange:@
setConformsCaptionsToTimeRangeSelector :: Selector '[Bool] ()
setConformsCaptionsToTimeRangeSelector = mkSelector "setConformsCaptionsToTimeRange:"

