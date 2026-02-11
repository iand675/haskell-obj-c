{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHContentEditingOutput@.
module ObjC.Photos.PHContentEditingOutput
  ( PHContentEditingOutput
  , IsPHContentEditingOutput(..)
  , initWithContentEditingInput
  , renderedContentURLForType_error
  , initWithPlaceholderForCreatedAsset
  , adjustmentData
  , setAdjustmentData
  , renderedContentURL
  , defaultRenderedContentType
  , supportedRenderedContentTypes
  , initWithContentEditingInputSelector
  , renderedContentURLForType_errorSelector
  , initWithPlaceholderForCreatedAssetSelector
  , adjustmentDataSelector
  , setAdjustmentDataSelector
  , renderedContentURLSelector
  , defaultRenderedContentTypeSelector
  , supportedRenderedContentTypesSelector


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

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithContentEditingInput:@
initWithContentEditingInput :: (IsPHContentEditingOutput phContentEditingOutput, IsPHContentEditingInput contentEditingInput) => phContentEditingOutput -> contentEditingInput -> IO (Id PHContentEditingOutput)
initWithContentEditingInput phContentEditingOutput  contentEditingInput =
withObjCPtr contentEditingInput $ \raw_contentEditingInput ->
    sendMsg phContentEditingOutput (mkSelector "initWithContentEditingInput:") (retPtr retVoid) [argPtr (castPtr raw_contentEditingInput :: Ptr ())] >>= ownedObject . castPtr

-- | Returns a file URL where the rendered output in the specified format, with adjustments baked-in, needs to be written to. Returns nil and provides an error identifying the reason if the format is unsupported or the requested content URL cannot be provided
--
-- ObjC selector: @- renderedContentURLForType:error:@
renderedContentURLForType_error :: (IsPHContentEditingOutput phContentEditingOutput, IsUTType type_, IsNSError error_) => phContentEditingOutput -> type_ -> error_ -> IO (Id NSURL)
renderedContentURLForType_error phContentEditingOutput  type_ error_ =
withObjCPtr type_ $ \raw_type_ ->
  withObjCPtr error_ $ \raw_error_ ->
      sendMsg phContentEditingOutput (mkSelector "renderedContentURLForType:error:") (retPtr retVoid) [argPtr (castPtr raw_type_ :: Ptr ()), argPtr (castPtr raw_error_ :: Ptr ())] >>= retainedObject . castPtr

-- | @- initWithPlaceholderForCreatedAsset:@
initWithPlaceholderForCreatedAsset :: (IsPHContentEditingOutput phContentEditingOutput, IsPHObjectPlaceholder placeholderForCreatedAsset) => phContentEditingOutput -> placeholderForCreatedAsset -> IO (Id PHContentEditingOutput)
initWithPlaceholderForCreatedAsset phContentEditingOutput  placeholderForCreatedAsset =
withObjCPtr placeholderForCreatedAsset $ \raw_placeholderForCreatedAsset ->
    sendMsg phContentEditingOutput (mkSelector "initWithPlaceholderForCreatedAsset:") (retPtr retVoid) [argPtr (castPtr raw_placeholderForCreatedAsset :: Ptr ())] >>= ownedObject . castPtr

-- | @- adjustmentData@
adjustmentData :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id PHAdjustmentData)
adjustmentData phContentEditingOutput  =
  sendMsg phContentEditingOutput (mkSelector "adjustmentData") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setAdjustmentData:@
setAdjustmentData :: (IsPHContentEditingOutput phContentEditingOutput, IsPHAdjustmentData value) => phContentEditingOutput -> value -> IO ()
setAdjustmentData phContentEditingOutput  value =
withObjCPtr value $ \raw_value ->
    sendMsg phContentEditingOutput (mkSelector "setAdjustmentData:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | File URL where the rendered output in the default format, with adjustments baked-in, needs to be written to.
--
-- ObjC selector: @- renderedContentURL@
renderedContentURL :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id NSURL)
renderedContentURL phContentEditingOutput  =
  sendMsg phContentEditingOutput (mkSelector "renderedContentURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the default type for the rendered content output
--
-- ObjC selector: @- defaultRenderedContentType@
defaultRenderedContentType :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id UTType)
defaultRenderedContentType phContentEditingOutput  =
  sendMsg phContentEditingOutput (mkSelector "defaultRenderedContentType") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns the supported types for the rendered content output
--
-- ObjC selector: @- supportedRenderedContentTypes@
supportedRenderedContentTypes :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id NSArray)
supportedRenderedContentTypes phContentEditingOutput  =
  sendMsg phContentEditingOutput (mkSelector "supportedRenderedContentTypes") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentEditingInput:@
initWithContentEditingInputSelector :: Selector
initWithContentEditingInputSelector = mkSelector "initWithContentEditingInput:"

-- | @Selector@ for @renderedContentURLForType:error:@
renderedContentURLForType_errorSelector :: Selector
renderedContentURLForType_errorSelector = mkSelector "renderedContentURLForType:error:"

-- | @Selector@ for @initWithPlaceholderForCreatedAsset:@
initWithPlaceholderForCreatedAssetSelector :: Selector
initWithPlaceholderForCreatedAssetSelector = mkSelector "initWithPlaceholderForCreatedAsset:"

-- | @Selector@ for @adjustmentData@
adjustmentDataSelector :: Selector
adjustmentDataSelector = mkSelector "adjustmentData"

-- | @Selector@ for @setAdjustmentData:@
setAdjustmentDataSelector :: Selector
setAdjustmentDataSelector = mkSelector "setAdjustmentData:"

-- | @Selector@ for @renderedContentURL@
renderedContentURLSelector :: Selector
renderedContentURLSelector = mkSelector "renderedContentURL"

-- | @Selector@ for @defaultRenderedContentType@
defaultRenderedContentTypeSelector :: Selector
defaultRenderedContentTypeSelector = mkSelector "defaultRenderedContentType"

-- | @Selector@ for @supportedRenderedContentTypes@
supportedRenderedContentTypesSelector :: Selector
supportedRenderedContentTypesSelector = mkSelector "supportedRenderedContentTypes"

