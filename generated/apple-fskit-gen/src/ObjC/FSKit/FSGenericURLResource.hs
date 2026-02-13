{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A resource that represents an abstract URL.
--
-- An @FSGenericURLResource@ is a completely abstract resource. The only reference to its contents is a single URL, the contents of which are arbitrary. This URL might represent a PCI locator string like `/pci/usb\@5`, or some sort of network address for a remote file system. FSKit leaves interpretation of the URL and its contents entirely up to your implementation.
--
-- Use the @Info.plist@ key @FSSupportedSchemes@ to provide an array of case-insensitive URL schemes that your implementation supports. The following example shows how a hypothetical @FSGenericURLResource@ implementation declares support for the @rsh@ and @ssh@ URL schemes: ``` <key>FSSupportedSchemes</key> <array>     <string>rsh</string>     <string>ssh</string> </array> ```
--
-- Generated bindings for @FSGenericURLResource@.
module ObjC.FSKit.FSGenericURLResource
  ( FSGenericURLResource
  , IsFSGenericURLResource(..)
  , initWithURL
  , init_
  , url
  , initSelector
  , initWithURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a generic URL resource with the given URL. - Parameter url: A URL that provides the content of the file system. The format of this URL is completely arbitrary. It's up to your extension to access the contents represented by the URL and make them available as an ``FSVolume`` that FSKit can load.
--
-- ObjC selector: @- initWithURL:@
initWithURL :: (IsFSGenericURLResource fsGenericURLResource, IsNSURL url) => fsGenericURLResource -> url -> IO (Id FSGenericURLResource)
initWithURL fsGenericURLResource url =
  sendOwnedMessage fsGenericURLResource initWithURLSelector (toNSURL url)

-- | @- init@
init_ :: IsFSGenericURLResource fsGenericURLResource => fsGenericURLResource -> IO (Id FSGenericURLResource)
init_ fsGenericURLResource =
  sendOwnedMessage fsGenericURLResource initSelector

-- | The URL represented by the resource.
--
-- ObjC selector: @- url@
url :: IsFSGenericURLResource fsGenericURLResource => fsGenericURLResource -> IO (Id NSURL)
url fsGenericURLResource =
  sendMessage fsGenericURLResource urlSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:@
initWithURLSelector :: Selector '[Id NSURL] (Id FSGenericURLResource)
initWithURLSelector = mkSelector "initWithURL:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSGenericURLResource)
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

