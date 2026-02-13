{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @PHPickerViewController@.
module ObjC.PhotosUI.PHPickerViewController
  ( PHPickerViewController
  , IsPHPickerViewController(..)
  , initWithConfiguration
  , updatePickerUsingConfiguration
  , deselectAssetsWithIdentifiers
  , moveAssetWithIdentifier_afterAssetWithIdentifier
  , scrollToInitialPosition
  , zoomIn
  , zoomOut
  , new
  , init_
  , initWithNibName_bundle
  , initWithCoder
  , configuration
  , delegate
  , setDelegate
  , configurationSelector
  , delegateSelector
  , deselectAssetsWithIdentifiersSelector
  , initSelector
  , initWithCoderSelector
  , initWithConfigurationSelector
  , initWithNibName_bundleSelector
  , moveAssetWithIdentifier_afterAssetWithIdentifierSelector
  , newSelector
  , scrollToInitialPositionSelector
  , setDelegateSelector
  , updatePickerUsingConfigurationSelector
  , zoomInSelector
  , zoomOutSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PhotosUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new picker with the @configuration@ the picker should use.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsPHPickerViewController phPickerViewController, IsPHPickerConfiguration configuration) => phPickerViewController -> configuration -> IO (Id PHPickerViewController)
initWithConfiguration phPickerViewController configuration =
  sendOwnedMessage phPickerViewController initWithConfigurationSelector (toPHPickerConfiguration configuration)

-- | Updates the picker using the configuration.
--
-- ObjC selector: @- updatePickerUsingConfiguration:@
updatePickerUsingConfiguration :: (IsPHPickerViewController phPickerViewController, IsPHPickerUpdateConfiguration configuration) => phPickerViewController -> configuration -> IO ()
updatePickerUsingConfiguration phPickerViewController configuration =
  sendMessage phPickerViewController updatePickerUsingConfigurationSelector (toPHPickerUpdateConfiguration configuration)

-- | Deselects selected assets in the picker.
--
-- Does nothing if asset identifiers are invalid or not selected, or @photoLibrary@ is not specified in the configuration.
--
-- ObjC selector: @- deselectAssetsWithIdentifiers:@
deselectAssetsWithIdentifiers :: (IsPHPickerViewController phPickerViewController, IsNSArray identifiers) => phPickerViewController -> identifiers -> IO ()
deselectAssetsWithIdentifiers phPickerViewController identifiers =
  sendMessage phPickerViewController deselectAssetsWithIdentifiersSelector (toNSArray identifiers)

-- | Reorders selected assets in the picker. A @nil@ @afterIdentifier@ means moving to the front.
--
-- Does nothing if asset identifiers are invalid or not selected, or @photoLibrary@ is not specified in the configuration.
--
-- ObjC selector: @- moveAssetWithIdentifier:afterAssetWithIdentifier:@
moveAssetWithIdentifier_afterAssetWithIdentifier :: (IsPHPickerViewController phPickerViewController, IsNSString identifier, IsNSString afterIdentifier) => phPickerViewController -> identifier -> afterIdentifier -> IO ()
moveAssetWithIdentifier_afterAssetWithIdentifier phPickerViewController identifier afterIdentifier =
  sendMessage phPickerViewController moveAssetWithIdentifier_afterAssetWithIdentifierSelector (toNSString identifier) (toNSString afterIdentifier)

-- | Scrolls content to the initial position if possible.
--
-- ObjC selector: @- scrollToInitialPosition@
scrollToInitialPosition :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
scrollToInitialPosition phPickerViewController =
  sendMessage phPickerViewController scrollToInitialPositionSelector

-- | Zooms in content if possible.
--
-- ObjC selector: @- zoomIn@
zoomIn :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
zoomIn phPickerViewController =
  sendMessage phPickerViewController zoomInSelector

-- | Zooms out content if possible.
--
-- ObjC selector: @- zoomOut@
zoomOut :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
zoomOut phPickerViewController =
  sendMessage phPickerViewController zoomOutSelector

-- | @+ new@
new :: IO (Id PHPickerViewController)
new  =
  do
    cls' <- getRequiredClass "PHPickerViewController"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO (Id PHPickerViewController)
init_ phPickerViewController =
  sendOwnedMessage phPickerViewController initSelector

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsPHPickerViewController phPickerViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => phPickerViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id PHPickerViewController)
initWithNibName_bundle phPickerViewController nibNameOrNil nibBundleOrNil =
  sendOwnedMessage phPickerViewController initWithNibName_bundleSelector (toNSString nibNameOrNil) (toNSBundle nibBundleOrNil)

-- | @- initWithCoder:@
initWithCoder :: (IsPHPickerViewController phPickerViewController, IsNSCoder coder) => phPickerViewController -> coder -> IO (Id PHPickerViewController)
initWithCoder phPickerViewController coder =
  sendOwnedMessage phPickerViewController initWithCoderSelector (toNSCoder coder)

-- | The configuration passed in during initialization.
--
-- ObjC selector: @- configuration@
configuration :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO (Id PHPickerConfiguration)
configuration phPickerViewController =
  sendMessage phPickerViewController configurationSelector

-- | The delegate to be notified.
--
-- ObjC selector: @- delegate@
delegate :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO RawId
delegate phPickerViewController =
  sendMessage phPickerViewController delegateSelector

-- | The delegate to be notified.
--
-- ObjC selector: @- setDelegate:@
setDelegate :: IsPHPickerViewController phPickerViewController => phPickerViewController -> RawId -> IO ()
setDelegate phPickerViewController value =
  sendMessage phPickerViewController setDelegateSelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector '[Id PHPickerConfiguration] (Id PHPickerViewController)
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @updatePickerUsingConfiguration:@
updatePickerUsingConfigurationSelector :: Selector '[Id PHPickerUpdateConfiguration] ()
updatePickerUsingConfigurationSelector = mkSelector "updatePickerUsingConfiguration:"

-- | @Selector@ for @deselectAssetsWithIdentifiers:@
deselectAssetsWithIdentifiersSelector :: Selector '[Id NSArray] ()
deselectAssetsWithIdentifiersSelector = mkSelector "deselectAssetsWithIdentifiers:"

-- | @Selector@ for @moveAssetWithIdentifier:afterAssetWithIdentifier:@
moveAssetWithIdentifier_afterAssetWithIdentifierSelector :: Selector '[Id NSString, Id NSString] ()
moveAssetWithIdentifier_afterAssetWithIdentifierSelector = mkSelector "moveAssetWithIdentifier:afterAssetWithIdentifier:"

-- | @Selector@ for @scrollToInitialPosition@
scrollToInitialPositionSelector :: Selector '[] ()
scrollToInitialPositionSelector = mkSelector "scrollToInitialPosition"

-- | @Selector@ for @zoomIn@
zoomInSelector :: Selector '[] ()
zoomInSelector = mkSelector "zoomIn"

-- | @Selector@ for @zoomOut@
zoomOutSelector :: Selector '[] ()
zoomOutSelector = mkSelector "zoomOut"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHPickerViewController)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHPickerViewController)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector '[Id NSString, Id NSBundle] (Id PHPickerViewController)
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector '[Id NSCoder] (Id PHPickerViewController)
initWithCoderSelector = mkSelector "initWithCoder:"

-- | @Selector@ for @configuration@
configurationSelector :: Selector '[] (Id PHPickerConfiguration)
configurationSelector = mkSelector "configuration"

-- | @Selector@ for @delegate@
delegateSelector :: Selector '[] RawId
delegateSelector = mkSelector "delegate"

-- | @Selector@ for @setDelegate:@
setDelegateSelector :: Selector '[RawId] ()
setDelegateSelector = mkSelector "setDelegate:"

