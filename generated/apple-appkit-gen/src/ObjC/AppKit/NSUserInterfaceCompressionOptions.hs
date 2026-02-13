{-# LANGUAGE DataKinds #-}
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
  , breakEqualWidthsOptionSelector
  , containsOptionsSelector
  , emptySelector
  , hideImagesOptionSelector
  , hideTextOptionSelector
  , initSelector
  , initWithCoderSelector
  , initWithCompressionOptionsSelector
  , initWithIdentifierSelector
  , intersectsOptionsSelector
  , optionsByAddingOptionsSelector
  , optionsByRemovingOptionsSelector
  , reduceMetricsOptionSelector
  , standardOptionsSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions => nsUserInterfaceCompressionOptions -> IO (Id NSUserInterfaceCompressionOptions)
init_ nsUserInterfaceCompressionOptions =
  sendOwnedMessage nsUserInterfaceCompressionOptions initSelector

-- | @- initWithCoder:@
initWithCoder :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSCoder coder) => nsUserInterfaceCompressionOptions -> coder -> IO (Id NSUserInterfaceCompressionOptions)
initWithCoder nsUserInterfaceCompressionOptions coder =
  sendOwnedMessage nsUserInterfaceCompressionOptions initWithCoderSelector (toNSCoder coder)

-- | @- initWithIdentifier:@
initWithIdentifier :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSString identifier) => nsUserInterfaceCompressionOptions -> identifier -> IO (Id NSUserInterfaceCompressionOptions)
initWithIdentifier nsUserInterfaceCompressionOptions identifier =
  sendOwnedMessage nsUserInterfaceCompressionOptions initWithIdentifierSelector (toNSString identifier)

-- | @- initWithCompressionOptions:@
initWithCompressionOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSSet options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
initWithCompressionOptions nsUserInterfaceCompressionOptions options =
  sendOwnedMessage nsUserInterfaceCompressionOptions initWithCompressionOptionsSelector (toNSSet options)

-- | @- containsOptions:@
containsOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO Bool
containsOptions nsUserInterfaceCompressionOptions options =
  sendMessage nsUserInterfaceCompressionOptions containsOptionsSelector (toNSUserInterfaceCompressionOptions options)

-- | @- intersectsOptions:@
intersectsOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO Bool
intersectsOptions nsUserInterfaceCompressionOptions options =
  sendMessage nsUserInterfaceCompressionOptions intersectsOptionsSelector (toNSUserInterfaceCompressionOptions options)

-- | @- optionsByAddingOptions:@
optionsByAddingOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
optionsByAddingOptions nsUserInterfaceCompressionOptions options =
  sendMessage nsUserInterfaceCompressionOptions optionsByAddingOptionsSelector (toNSUserInterfaceCompressionOptions options)

-- | @- optionsByRemovingOptions:@
optionsByRemovingOptions :: (IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions, IsNSUserInterfaceCompressionOptions options) => nsUserInterfaceCompressionOptions -> options -> IO (Id NSUserInterfaceCompressionOptions)
optionsByRemovingOptions nsUserInterfaceCompressionOptions options =
  sendMessage nsUserInterfaceCompressionOptions optionsByRemovingOptionsSelector (toNSUserInterfaceCompressionOptions options)

-- | @- empty@
empty :: IsNSUserInterfaceCompressionOptions nsUserInterfaceCompressionOptions => nsUserInterfaceCompressionOptions -> IO Bool
empty nsUserInterfaceCompressionOptions =
  sendMessage nsUserInterfaceCompressionOptions emptySelector

-- | @+ hideImagesOption@
hideImagesOption :: IO (Id NSUserInterfaceCompressionOptions)
hideImagesOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMessage cls' hideImagesOptionSelector

-- | @+ hideTextOption@
hideTextOption :: IO (Id NSUserInterfaceCompressionOptions)
hideTextOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMessage cls' hideTextOptionSelector

-- | @+ reduceMetricsOption@
reduceMetricsOption :: IO (Id NSUserInterfaceCompressionOptions)
reduceMetricsOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMessage cls' reduceMetricsOptionSelector

-- | @+ breakEqualWidthsOption@
breakEqualWidthsOption :: IO (Id NSUserInterfaceCompressionOptions)
breakEqualWidthsOption  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMessage cls' breakEqualWidthsOptionSelector

-- | @+ standardOptions@
standardOptions :: IO (Id NSUserInterfaceCompressionOptions)
standardOptions  =
  do
    cls' <- getRequiredClass "NSUserInterfaceCompressionOptions"
    sendClassMessage cls' standardOptionsSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id NSUserInterfaceCompressionOptions)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @initWithIdentifier:@
initWithIdentifierSelector :: Selector '[Id NSString] (Id NSUserInterfaceCompressionOptions)
initWithIdentifierSelector = mkSelector "initWithIdentifier:"

-- | @Selector@ for @initWithCompressionOptions:@
initWithCompressionOptionsSelector :: Selector '[Id NSSet] (Id NSUserInterfaceCompressionOptions)
initWithCompressionOptionsSelector = mkSelector "initWithCompressionOptions:"

-- | @Selector@ for @containsOptions:@
containsOptionsSelector :: Selector '[Id NSUserInterfaceCompressionOptions] Bool
containsOptionsSelector = mkSelector "containsOptions:"

-- | @Selector@ for @intersectsOptions:@
intersectsOptionsSelector :: Selector '[Id NSUserInterfaceCompressionOptions] Bool
intersectsOptionsSelector = mkSelector "intersectsOptions:"

-- | @Selector@ for @optionsByAddingOptions:@
optionsByAddingOptionsSelector :: Selector '[Id NSUserInterfaceCompressionOptions] (Id NSUserInterfaceCompressionOptions)
optionsByAddingOptionsSelector = mkSelector "optionsByAddingOptions:"

-- | @Selector@ for @optionsByRemovingOptions:@
optionsByRemovingOptionsSelector :: Selector '[Id NSUserInterfaceCompressionOptions] (Id NSUserInterfaceCompressionOptions)
optionsByRemovingOptionsSelector = mkSelector "optionsByRemovingOptions:"

-- | @Selector@ for @empty@
emptySelector :: Selector '[] Bool
emptySelector = mkSelector "empty"

-- | @Selector@ for @hideImagesOption@
hideImagesOptionSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
hideImagesOptionSelector = mkSelector "hideImagesOption"

-- | @Selector@ for @hideTextOption@
hideTextOptionSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
hideTextOptionSelector = mkSelector "hideTextOption"

-- | @Selector@ for @reduceMetricsOption@
reduceMetricsOptionSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
reduceMetricsOptionSelector = mkSelector "reduceMetricsOption"

-- | @Selector@ for @breakEqualWidthsOption@
breakEqualWidthsOptionSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
breakEqualWidthsOptionSelector = mkSelector "breakEqualWidthsOption"

-- | @Selector@ for @standardOptions@
standardOptionsSelector :: Selector '[] (Id NSUserInterfaceCompressionOptions)
standardOptionsSelector = mkSelector "standardOptions"

