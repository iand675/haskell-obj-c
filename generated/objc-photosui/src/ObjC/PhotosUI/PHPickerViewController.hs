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
  , initWithConfigurationSelector
  , updatePickerUsingConfigurationSelector
  , deselectAssetsWithIdentifiersSelector
  , moveAssetWithIdentifier_afterAssetWithIdentifierSelector
  , scrollToInitialPositionSelector
  , zoomInSelector
  , zoomOutSelector
  , newSelector
  , initSelector
  , initWithNibName_bundleSelector
  , initWithCoderSelector


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

import ObjC.PhotosUI.Internal.Classes
import ObjC.AppKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Initializes a new picker with the @configuration@ the picker should use.
--
-- ObjC selector: @- initWithConfiguration:@
initWithConfiguration :: (IsPHPickerViewController phPickerViewController, IsPHPickerConfiguration configuration) => phPickerViewController -> configuration -> IO (Id PHPickerViewController)
initWithConfiguration phPickerViewController  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg phPickerViewController (mkSelector "initWithConfiguration:") (retPtr retVoid) [argPtr (castPtr raw_configuration :: Ptr ())] >>= ownedObject . castPtr

-- | Updates the picker using the configuration.
--
-- ObjC selector: @- updatePickerUsingConfiguration:@
updatePickerUsingConfiguration :: (IsPHPickerViewController phPickerViewController, IsPHPickerUpdateConfiguration configuration) => phPickerViewController -> configuration -> IO ()
updatePickerUsingConfiguration phPickerViewController  configuration =
withObjCPtr configuration $ \raw_configuration ->
    sendMsg phPickerViewController (mkSelector "updatePickerUsingConfiguration:") retVoid [argPtr (castPtr raw_configuration :: Ptr ())]

-- | Deselects selected assets in the picker.
--
-- Does nothing if asset identifiers are invalid or not selected, or @photoLibrary@ is not specified in the configuration.
--
-- ObjC selector: @- deselectAssetsWithIdentifiers:@
deselectAssetsWithIdentifiers :: (IsPHPickerViewController phPickerViewController, IsNSArray identifiers) => phPickerViewController -> identifiers -> IO ()
deselectAssetsWithIdentifiers phPickerViewController  identifiers =
withObjCPtr identifiers $ \raw_identifiers ->
    sendMsg phPickerViewController (mkSelector "deselectAssetsWithIdentifiers:") retVoid [argPtr (castPtr raw_identifiers :: Ptr ())]

-- | Reorders selected assets in the picker. A @nil@ @afterIdentifier@ means moving to the front.
--
-- Does nothing if asset identifiers are invalid or not selected, or @photoLibrary@ is not specified in the configuration.
--
-- ObjC selector: @- moveAssetWithIdentifier:afterAssetWithIdentifier:@
moveAssetWithIdentifier_afterAssetWithIdentifier :: (IsPHPickerViewController phPickerViewController, IsNSString identifier, IsNSString afterIdentifier) => phPickerViewController -> identifier -> afterIdentifier -> IO ()
moveAssetWithIdentifier_afterAssetWithIdentifier phPickerViewController  identifier afterIdentifier =
withObjCPtr identifier $ \raw_identifier ->
  withObjCPtr afterIdentifier $ \raw_afterIdentifier ->
      sendMsg phPickerViewController (mkSelector "moveAssetWithIdentifier:afterAssetWithIdentifier:") retVoid [argPtr (castPtr raw_identifier :: Ptr ()), argPtr (castPtr raw_afterIdentifier :: Ptr ())]

-- | Scrolls content to the initial position if possible.
--
-- ObjC selector: @- scrollToInitialPosition@
scrollToInitialPosition :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
scrollToInitialPosition phPickerViewController  =
  sendMsg phPickerViewController (mkSelector "scrollToInitialPosition") retVoid []

-- | Zooms in content if possible.
--
-- ObjC selector: @- zoomIn@
zoomIn :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
zoomIn phPickerViewController  =
  sendMsg phPickerViewController (mkSelector "zoomIn") retVoid []

-- | Zooms out content if possible.
--
-- ObjC selector: @- zoomOut@
zoomOut :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO ()
zoomOut phPickerViewController  =
  sendMsg phPickerViewController (mkSelector "zoomOut") retVoid []

-- | @+ new@
new :: IO (Id PHPickerViewController)
new  =
  do
    cls' <- getRequiredClass "PHPickerViewController"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsPHPickerViewController phPickerViewController => phPickerViewController -> IO (Id PHPickerViewController)
init_ phPickerViewController  =
  sendMsg phPickerViewController (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithNibName:bundle:@
initWithNibName_bundle :: (IsPHPickerViewController phPickerViewController, IsNSString nibNameOrNil, IsNSBundle nibBundleOrNil) => phPickerViewController -> nibNameOrNil -> nibBundleOrNil -> IO (Id PHPickerViewController)
initWithNibName_bundle phPickerViewController  nibNameOrNil nibBundleOrNil =
withObjCPtr nibNameOrNil $ \raw_nibNameOrNil ->
  withObjCPtr nibBundleOrNil $ \raw_nibBundleOrNil ->
      sendMsg phPickerViewController (mkSelector "initWithNibName:bundle:") (retPtr retVoid) [argPtr (castPtr raw_nibNameOrNil :: Ptr ()), argPtr (castPtr raw_nibBundleOrNil :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithCoder:@
initWithCoder :: (IsPHPickerViewController phPickerViewController, IsNSCoder coder) => phPickerViewController -> coder -> IO (Id PHPickerViewController)
initWithCoder phPickerViewController  coder =
withObjCPtr coder $ \raw_coder ->
    sendMsg phPickerViewController (mkSelector "initWithCoder:") (retPtr retVoid) [argPtr (castPtr raw_coder :: Ptr ())] >>= ownedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithConfiguration:@
initWithConfigurationSelector :: Selector
initWithConfigurationSelector = mkSelector "initWithConfiguration:"

-- | @Selector@ for @updatePickerUsingConfiguration:@
updatePickerUsingConfigurationSelector :: Selector
updatePickerUsingConfigurationSelector = mkSelector "updatePickerUsingConfiguration:"

-- | @Selector@ for @deselectAssetsWithIdentifiers:@
deselectAssetsWithIdentifiersSelector :: Selector
deselectAssetsWithIdentifiersSelector = mkSelector "deselectAssetsWithIdentifiers:"

-- | @Selector@ for @moveAssetWithIdentifier:afterAssetWithIdentifier:@
moveAssetWithIdentifier_afterAssetWithIdentifierSelector :: Selector
moveAssetWithIdentifier_afterAssetWithIdentifierSelector = mkSelector "moveAssetWithIdentifier:afterAssetWithIdentifier:"

-- | @Selector@ for @scrollToInitialPosition@
scrollToInitialPositionSelector :: Selector
scrollToInitialPositionSelector = mkSelector "scrollToInitialPosition"

-- | @Selector@ for @zoomIn@
zoomInSelector :: Selector
zoomInSelector = mkSelector "zoomIn"

-- | @Selector@ for @zoomOut@
zoomOutSelector :: Selector
zoomOutSelector = mkSelector "zoomOut"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithNibName:bundle:@
initWithNibName_bundleSelector :: Selector
initWithNibName_bundleSelector = mkSelector "initWithNibName:bundle:"

-- | @Selector@ for @initWithCoder:@
initWithCoderSelector :: Selector
initWithCoderSelector = mkSelector "initWithCoder:"

