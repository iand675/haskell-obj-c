{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A class incorporating properties for a MediaExtension
--
-- AVMediaExtensionProperties objects are returned from property queries on AVAsset, AVPlayerItemTrack, AVSampleBufferDisplayLayer, or AVSampleBufferVideoRenderer. Subclasses of this type that are used from Swift must fulfill the requirements of a Sendable type.
--
-- Generated bindings for @AVMediaExtensionProperties@.
module ObjC.AVFoundation.AVMediaExtensionProperties
  ( AVMediaExtensionProperties
  , IsAVMediaExtensionProperties(..)
  , init_
  , new
  , extensionIdentifier
  , extensionName
  , containingBundleName
  , extensionURL
  , containingBundleURL
  , containingBundleNameSelector
  , containingBundleURLSelector
  , extensionIdentifierSelector
  , extensionNameSelector
  , extensionURLSelector
  , initSelector
  , newSelector


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
init_ :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id AVMediaExtensionProperties)
init_ avMediaExtensionProperties =
  sendOwnedMessage avMediaExtensionProperties initSelector

-- | @+ new@
new :: IO (Id AVMediaExtensionProperties)
new  =
  do
    cls' <- getRequiredClass "AVMediaExtensionProperties"
    sendOwnedClassMessage cls' newSelector

-- | The identifier of the Media Extension.
--
-- The extension identifier string, corresponding to the ClassImplementationID value from the EXAppExtensionAttributes dictionary in the Info.plist file.
--
-- ObjC selector: @- extensionIdentifier@
extensionIdentifier :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
extensionIdentifier avMediaExtensionProperties =
  sendMessage avMediaExtensionProperties extensionIdentifierSelector

-- | The name of the MediaExtension.
--
-- The localized name of the MediaExtension format reader or video decoder, corresponding to the CFBundleDisplayName.
--
-- ObjC selector: @- extensionName@
extensionName :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
extensionName avMediaExtensionProperties =
  sendMessage avMediaExtensionProperties extensionNameSelector

-- | The name of the containing application bundle.
--
-- The localized name of the application that hosts the MediaExtension.
--
-- ObjC selector: @- containingBundleName@
containingBundleName :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
containingBundleName avMediaExtensionProperties =
  sendMessage avMediaExtensionProperties containingBundleNameSelector

-- | The file URL of the MediaExtension bundle.
--
-- ObjC selector: @- extensionURL@
extensionURL :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSURL)
extensionURL avMediaExtensionProperties =
  sendMessage avMediaExtensionProperties extensionURLSelector

-- | The file URL of the host application for the MediaExtension.
--
-- ObjC selector: @- containingBundleURL@
containingBundleURL :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSURL)
containingBundleURL avMediaExtensionProperties =
  sendMessage avMediaExtensionProperties containingBundleURLSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id AVMediaExtensionProperties)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id AVMediaExtensionProperties)
newSelector = mkSelector "new"

-- | @Selector@ for @extensionIdentifier@
extensionIdentifierSelector :: Selector '[] (Id NSString)
extensionIdentifierSelector = mkSelector "extensionIdentifier"

-- | @Selector@ for @extensionName@
extensionNameSelector :: Selector '[] (Id NSString)
extensionNameSelector = mkSelector "extensionName"

-- | @Selector@ for @containingBundleName@
containingBundleNameSelector :: Selector '[] (Id NSString)
containingBundleNameSelector = mkSelector "containingBundleName"

-- | @Selector@ for @extensionURL@
extensionURLSelector :: Selector '[] (Id NSURL)
extensionURLSelector = mkSelector "extensionURL"

-- | @Selector@ for @containingBundleURL@
containingBundleURLSelector :: Selector '[] (Id NSURL)
containingBundleURLSelector = mkSelector "containingBundleURL"

