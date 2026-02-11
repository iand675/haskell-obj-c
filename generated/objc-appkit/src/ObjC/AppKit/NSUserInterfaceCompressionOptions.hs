{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @NSUserInterfaceCompressionOptions@.
module ObjC.AppKit.NSUserInterfaceCompressionOptions
  ( NSUserInterfaceCompressionOptions
  , IsNSUserInterfaceCompressionOptions(..)
  , init_
  , initWithCoder
  , initWithIdentifier
  , initWithCompressionOptions
  , containsOptions
  , intersectsOptions
  , optionsByAddingOptions
  , optionsByRemovingOptions
  , empty
  , hideImagesOption
  , hideTextOption
  , reduceMetricsOption
  , breakEqualWidthsOption
  , standardOptions
  , initSelector
  , initWithCoderSelector
  , initWithIdentifierSelector
  , initWithCompressionOptionsSelector
  , containsOptionsSelector
  , intersectsOptionsSelector
  , optionsByAddingOptionsSelector
  , optionsByRemovingOptionsSelector
  , emptySelector
  , hideImagesOptionSelector
  , hideTextOptionSelector
  , reduceMetricsOptionSelector
  , breakEqualWidthsOptionSelector
  , standardOptionsSelector


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

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions => nsUserInterfaceCompressionOptions -> IO (Id NSUserInterfaceCompressionOptions)
init_ nsUserInterfaceCompressionOptions  =
  sendMsg nsUserInterfaceCompressionOptions (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSCoder coder) => nsUserInterfaceCompressionOptions -> coder -> IO (Id NSUserInterfaceCompressionOptions)
initWithCoder nsUserInterfaceCompressionOptions  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg nsUserInterfaceCompressionOptions (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSString identifier) => nsUserInterfaceCompressionOptions -> identifier -> IO (Id NSUserInterfaceCompressionOptions)
initWithIdentifier nsUserInterfaceCompressionOptions  identifier =
withObjCPtr identifier $ \raw_identifier ->
    sendMsg nsUserInterfaceCompressionOptions (mkSelector "initWithIdentifier:") (retPtr retVoid) [argPtr (castPtr raw_identifier :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCompressionOptions:@
initWithCompressionOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSSet options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
initWithCompressionOptions nsUserInterfaceCompressionOptions  options =
withObjCPtr options $ \raw_options ->
    sendMsg nsUserInterfaceCompressionOptions (mkSelector "initWithCompressionOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= ownedObject . castPtr

-- | @- containsOptions:@
containsOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO Bool
containsOptions nsUserInterfaceCompressionOptions  options =
withObjCPtr options $ \raw_options ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserInterfaceCompressionOptions (mkSelector "containsOptions:") retCULong [argPtr (castPtr raw_options :: Ptr ())]

-- | @- intersectsOptions:@
intersectsOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO Bool
intersectsOptions nsUserInterfaceCompressionOptions  options =
withObjCPtr options $ \raw_options ->
    fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserInterfaceCompressionOptions (mkSelector "intersectsOptions:") retCULong [argPtr (castPtr raw_options :: Ptr ())]

-- | @- optionsByAddingOptions:@
optionsByAddingOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
optionsByAddingOptions nsUserInterfaceCompressionOptions  options =
withObjCPtr options $ \raw_options ->
    sendMsg nsUserInterfaceCompressionOptions (mkSelector "optionsByAddingOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- optionsByRemovingOptions:@
optionsByRemovingOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
optionsByRemovingOptions nsUserInterfaceCompressionOptions  options =
withObjCPtr options $ \raw_options ->
    sendMsg nsUserInterfaceCompressionOptions (mkSelector "optionsByRemovingOptions:") (retPtr retVoid) [argPtr (castPtr raw_options :: Ptr ())] >>= retainedObject . castPtr

-- | @- empty@
empty :: IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions => nsUserInterfaceCompressionOptions -> IO Bool
empty nsUserInterfaceCompressionOptions  =
  fmap ((/= 0) :: CULong -> Bool) $ sendMsg nsUserInterfaceCompressionOptions (mkSelector "empty") retCULong []

-- | @+ hideImagesOption@
hideImagesOption :: IO (Id NSUserInterfaceCompressionOptions)
hideImagesOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMsg cls' (mkSelector "hideImagesOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ hideTextOption@
hideTextOption :: IO (Id NSUserInterfaceCompressionOptions)
hideTextOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMsg cls' (mkSelector "hideTextOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ reduceMetricsOption@
reduceMetricsOption :: IO (Id NSUserInterfaceCompressionOptions)
reduceMetricsOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMsg cls' (mkSelector "reduceMetricsOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ breakEqualWidthsOption@
breakEqualWidthsOption :: IO (Id NSUserInterfaceCompressionOptions)
breakEqualWidthsOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMsg cls' (mkSelector "breakEqualWidthsOption") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @+ standardOptions@
standardOptions :: IO (Id NSUserInterfaceCompressionOptions)
standardOptions  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMsg cls' (mkSelector "standardOptions") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCompressionOptions:@
initWithCompressionOptionsSelector :: Selector
initWithCompressionOptionsSelector = mkSelector "initWithCompressionOptions:"

-- | @Selector@ for @containsOptions:@
containsOptionsSelector :: Selector
containsOptionsSelector = mkSelector "containsOptions:"

-- | @Selector@ for @intersectsOptions:@
intersectsOptionsSelector :: Selector
intersectsOptionsSelector = mkSelector "intersectsOptions:"

-- | @Selector@ for @optionsByAddingOptions:@
optionsByAddingOptionsSelector :: Selector
optionsByAddingOptionsSelector = mkSelector "optionsByAddingOptions:"

-- | @Selector@ for @optionsByRemovingOptions:@
optionsByRemovingOptionsSelector :: Selector
optionsByRemovingOptionsSelector = mkSelector "optionsByRemovingOptions:"

-- | @Selector@ for @empty@
emptySelector :: Selector
emptySelector = mkSelector "empty"

-- | @Selector@ for @hideImagesOption@
hideImagesOptionSelector :: Selector
hideImagesOptionSelector = mkSelector "hideImagesOption"

-- | @Selector@ for @hideTextOption@
hideTextOptionSelector :: Selector
hideTextOptionSelector = mkSelector "hideTextOption"

-- | @Selector@ for @reduceMetricsOption@
reduceMetricsOptionSelector :: Selector
reduceMetricsOptionSelector = mkSelector "reduceMetricsOption"

-- | @Selector@ for @breakEqualWidthsOption@
breakEqualWidthsOptionSelector :: Selector
breakEqualWidthsOptionSelector = mkSelector "breakEqualWidthsOption"

-- | @Selector@ for @standardOptions@
standardOptionsSelector :: Selector
standardOptionsSelector = mkSelector "standardOptions"

