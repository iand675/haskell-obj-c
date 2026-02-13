{-# LANGUAGE DataKinds #-}
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
  , adjustmentDataSelector
  , defaultRenderedContentTypeSelector
  , initWithContentEditingInputSelector
  , initWithPlaceholderForCreatedAssetSelector
  , renderedContentURLForType_errorSelector
  , renderedContentURLSelector
  , setAdjustmentDataSelector
  , supportedRenderedContentTypesSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Photos.Internal.Classes
import ObjC.Foundation.Internal.Classes
import ObjC.UniformTypeIdentifiers.Internal.Classes

-- | @- initWithContentEditingInput:@
initWithContentEditingInput :: (IsPHContentEditingOutput phContentEditingOutput, IsPHContentEditingInput contentEditingInput) => phContentEditingOutput -> contentEditingInput -> IO (Id PHContentEditingOutput)
initWithContentEditingInput phContentEditingOutput contentEditingInput =
  sendOwnedMessage phContentEditingOutput initWithContentEditingInputSelector (toPHContentEditingInput contentEditingInput)

-- | Returns a file URL where the rendered output in the specified format, with adjustments baked-in, needs to be written to. Returns nil and provides an error identifying the reason if the format is unsupported or the requested content URL cannot be provided
--
-- ObjC selector: @- renderedContentURLForType:error:@
renderedContentURLForType_error :: (IsPHContentEditingOutput phContentEditingOutput, IsUTType type_, IsNSError error_) => phContentEditingOutput -> type_ -> error_ -> IO (Id NSURL)
renderedContentURLForType_error phContentEditingOutput type_ error_ =
  sendMessage phContentEditingOutput renderedContentURLForType_errorSelector (toUTType type_) (toNSError error_)

-- | @- initWithPlaceholderForCreatedAsset:@
initWithPlaceholderForCreatedAsset :: (IsPHContentEditingOutput phContentEditingOutput, IsPHObjectPlaceholder placeholderForCreatedAsset) => phContentEditingOutput -> placeholderForCreatedAsset -> IO (Id PHContentEditingOutput)
initWithPlaceholderForCreatedAsset phContentEditingOutput placeholderForCreatedAsset =
  sendOwnedMessage phContentEditingOutput initWithPlaceholderForCreatedAssetSelector (toPHObjectPlaceholder placeholderForCreatedAsset)

-- | @- adjustmentData@
adjustmentData :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id PHAdjustmentData)
adjustmentData phContentEditingOutput =
  sendMessage phContentEditingOutput adjustmentDataSelector

-- | @- setAdjustmentData:@
setAdjustmentData :: (IsPHContentEditingOutput phContentEditingOutput, IsPHAdjustmentData value) => phContentEditingOutput -> value -> IO ()
setAdjustmentData phContentEditingOutput value =
  sendMessage phContentEditingOutput setAdjustmentDataSelector (toPHAdjustmentData value)

-- | File URL where the rendered output in the default format, with adjustments baked-in, needs to be written to.
--
-- ObjC selector: @- renderedContentURL@
renderedContentURL :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id NSURL)
renderedContentURL phContentEditingOutput =
  sendMessage phContentEditingOutput renderedContentURLSelector

-- | Returns the default type for the rendered content output
--
-- ObjC selector: @- defaultRenderedContentType@
defaultRenderedContentType :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id UTType)
defaultRenderedContentType phContentEditingOutput =
  sendMessage phContentEditingOutput defaultRenderedContentTypeSelector

-- | Returns the supported types for the rendered content output
--
-- ObjC selector: @- supportedRenderedContentTypes@
supportedRenderedContentTypes :: IsPHContentEditingOutput phContentEditingOutput => phContentEditingOutput -> IO (Id NSArray)
supportedRenderedContentTypes phContentEditingOutput =
  sendMessage phContentEditingOutput supportedRenderedContentTypesSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithContentEditingInput:@
initWithContentEditingInputSelector :: Selector '[Id PHContentEditingInput] (Id PHContentEditingOutput)
initWithContentEditingInputSelector = mkSelector "initWithContentEditingInput:"

-- | @Selector@ for @renderedContentURLForType:error:@
renderedContentURLForType_errorSelector :: Selector '[Id UTType, Id NSError] (Id NSURL)
renderedContentURLForType_errorSelector = mkSelector "renderedContentURLForType:error:"

-- | @Selector@ for @initWithPlaceholderForCreatedAsset:@
initWithPlaceholderForCreatedAssetSelector :: Selector '[Id PHObjectPlaceholder] (Id PHContentEditingOutput)
initWithPlaceholderForCreatedAssetSelector = mkSelector "initWithPlaceholderForCreatedAsset:"

-- | @Selector@ for @adjustmentData@
adjustmentDataSelector :: Selector '[] (Id PHAdjustmentData)
adjustmentDataSelector = mkSelector "adjustmentData"

-- | @Selector@ for @setAdjustmentData:@
setAdjustmentDataSelector :: Selector '[Id PHAdjustmentData] ()
setAdjustmentDataSelector = mkSelector "setAdjustmentData:"

-- | @Selector@ for @renderedContentURL@
renderedContentURLSelector :: Selector '[] (Id NSURL)
renderedContentURLSelector = mkSelector "renderedContentURL"

-- | @Selector@ for @defaultRenderedContentType@
defaultRenderedContentTypeSelector :: Selector '[] (Id UTType)
defaultRenderedContentTypeSelector = mkSelector "defaultRenderedContentType"

-- | @Selector@ for @supportedRenderedContentTypes@
supportedRenderedContentTypesSelector :: Selector '[] (Id NSArray)
supportedRenderedContentTypesSelector = mkSelector "supportedRenderedContentTypes"

