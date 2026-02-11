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
  , initSelector
  , newSelector
  , extensionIdentifierSelector
  , extensionNameSelector
  , containingBundleNameSelector
  , extensionURLSelector
  , containingBundleURLSelector


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
init_ :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id AVMediaExtensionProperties)
init_ avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id AVMediaExtensionProperties)
new  =
  do
    cls' <- getRequiredClass "AVMediaExtensionProperties"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | The identifier of the Media Extension.
--
-- The extension identifier string, corresponding to the ClassImplementationID value from the EXAppExtensionAttributes dictionary in the Info.plist file.
--
-- ObjC selector: @- extensionIdentifier@
extensionIdentifier :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
extensionIdentifier avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "extensionIdentifier") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the MediaExtension.
--
-- The localized name of the MediaExtension format reader or video decoder, corresponding to the CFBundleDisplayName.
--
-- ObjC selector: @- extensionName@
extensionName :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
extensionName avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "extensionName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The name of the containing application bundle.
--
-- The localized name of the application that hosts the MediaExtension.
--
-- ObjC selector: @- containingBundleName@
containingBundleName :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSString)
containingBundleName avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "containingBundleName") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The file URL of the MediaExtension bundle.
--
-- ObjC selector: @- extensionURL@
extensionURL :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSURL)
extensionURL avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "extensionURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | The file URL of the host application for the MediaExtension.
--
-- ObjC selector: @- containingBundleURL@
containingBundleURL :: IsAVMediaExtensionProperties avMediaExtensionProperties => avMediaExtensionProperties -> IO (Id NSURL)
containingBundleURL avMediaExtensionProperties  =
  sendMsg avMediaExtensionProperties (mkSelector "containingBundleURL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @extensionIdentifier@
extensionIdentifierSelector :: Selector
extensionIdentifierSelector = mkSelector "extensionIdentifier"

-- | @Selector@ for @extensionName@
extensionNameSelector :: Selector
extensionNameSelector = mkSelector "extensionName"

-- | @Selector@ for @containingBundleName@
containingBundleNameSelector :: Selector
containingBundleNameSelector = mkSelector "containingBundleName"

-- | @Selector@ for @extensionURL@
extensionURLSelector :: Selector
extensionURLSelector = mkSelector "extensionURL"

-- | @Selector@ for @containingBundleURL@
containingBundleURLSelector :: Selector
containingBundleURLSelector = mkSelector "containingBundleURL"

