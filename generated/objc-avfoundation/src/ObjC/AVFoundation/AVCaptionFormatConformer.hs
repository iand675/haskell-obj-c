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
  , initSelector
  , newSelector
  , captionFormatConformerWithConversionSettingsSelector
  , initWithConversionSettingsSelector
  , conformedCaptionForCaption_errorSelector
  , conformsCaptionsToTimeRangeSelector
  , setConformsCaptionsToTimeRangeSelector


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
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> IO (Id AVCaptionFormatConformer)
init_ avCaptionFormatConformer  =
  sendMsg avCaptionFormatConformer (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVCaptionFormatConformer)
new  =
  do
    cls' <- getRequiredClass "AVCaptionFormatConformer"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

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
    withObjCPtr conversionSettings $ \raw_conversionSettings ->
      sendClassMsg cls' (mkSelector "captionFormatConformerWithConversionSettings:") (retPtr retVoid) [argPtr (castPtr raw_conversionSettings :: Ptr ())] >>= retainedObject . castPtr

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
initWithConversionSettings avCaptionFormatConformer  conversionSettings =
withObjCPtr conversionSettings $ \raw_conversionSettings ->
    sendMsg avCaptionFormatConformer (mkSelector "initWithConversionSettings:") (retPtr retVoid) [argPtr (castPtr raw_conversionSettings :: Ptr ())] >>= ownedObject . castPtr

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
conformedCaptionForCaption_error avCaptionFormatConformer  caption outError =
withObjCPtr caption $ \raw_caption ->
  withObjCPtr outError $ \raw_outError ->
      sendMsg avCaptionFormatConformer (mkSelector "conformedCaptionForCaption:error:") (retPtr retVoid) [argPtr (castPtr raw_caption :: Ptr ()), argPtr (castPtr raw_outError :: Ptr ())] >>= retainedObject . castPtr

-- | conformsCaptionsToTimeRange
--
-- Specifies whether to conform the time range of a given canonical caption as well.
--
-- When set to YES, conforms time range.	When set to NO, the time range of the conformed caption will be same as a given canonical caption.	In the case of conforming to CAE608 format, AVCaption is encoded so that each CAE608 control code (2 bytes) fits into 1 frame duration (1001/30000).	When set to YES and if all the encoded data can not fit inside the canonical caption time range, the caption time range will be extended to fit all the data and will be returned in the conformed AVCaption.	The default value is NO.
--
-- ObjC selector: @- conformsCaptionsToTimeRange@
conformsCaptionsToTimeRange :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> IO Bool
conformsCaptionsToTimeRange avCaptionFormatConformer  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg avCaptionFormatConformer (mkSelector "conformsCaptionsToTimeRange") retCULong []

-- | conformsCaptionsToTimeRange
--
-- Specifies whether to conform the time range of a given canonical caption as well.
--
-- When set to YES, conforms time range.	When set to NO, the time range of the conformed caption will be same as a given canonical caption.	In the case of conforming to CAE608 format, AVCaption is encoded so that each CAE608 control code (2 bytes) fits into 1 frame duration (1001/30000).	When set to YES and if all the encoded data can not fit inside the canonical caption time range, the caption time range will be extended to fit all the data and will be returned in the conformed AVCaption.	The default value is NO.
--
-- ObjC selector: @- setConformsCaptionsToTimeRange:@
setConformsCaptionsToTimeRange :: IsAVCaptionFormatConformer avCaptionFormatConformer => avCaptionFormatConformer -> Bool -> IO ()
setConformsCaptionsToTimeRange avCaptionFormatConformer  value =
  sendMsg avCaptionFormatConformer (mkSelector "setConformsCaptionsToTimeRange:") retVoid [argCULong (if value then 1 else 0)]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @captionFormatConformerWithConversionSettings:@
captionFormatConformerWithConversionSettingsSelector :: Selector
captionFormatConformerWithConversionSettingsSelector = mkSelector "captionFormatConformerWithConversionSettings:"

-- | @Selector@ for @initWithConversionSettings:@
initWithConversionSettingsSelector :: Selector
initWithConversionSettingsSelector = mkSelector "initWithConversionSettings:"

-- | @Selector@ for @conformedCaptionForCaption:error:@
conformedCaptionForCaption_errorSelector :: Selector
conformedCaptionForCaption_errorSelector = mkSelector "conformedCaptionForCaption:error:"

-- | @Selector@ for @conformsCaptionsToTimeRange@
conformsCaptionsToTimeRangeSelector :: Selector
conformsCaptionsToTimeRangeSelector = mkSelector "conformsCaptionsToTimeRange"

-- | @Selector@ for @setConformsCaptionsToTimeRange:@
setConformsCaptionsToTimeRangeSelector :: Selector
setConformsCaptionsToTimeRangeSelector = mkSelector "setConformsCaptionsToTimeRange:"

