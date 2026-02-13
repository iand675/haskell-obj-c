{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | A resource that represents a path in the system file space.
--
-- The URL passed to @FSPathURLResource@ may be a security-scoped URL. If the URL is a security-scoped URL, FSKit transports it intact from a client application to your extension.
--
-- Generated bindings for @FSPathURLResource@.
module ObjC.FSKit.FSPathURLResource
  ( FSPathURLResource
  , IsFSPathURLResource(..)
  , initWithURL_writable
  , init_
  , url
  , writable
  , initSelector
  , initWithURL_writableSelector
  , urlSelector
  , writableSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.FSKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Creates a path URL resource. - Parameters:   - URL: A URL in the system file space that represents the contents of a file system. This parameter uses the @file:@ scheme.   - writable: A Boolean value that indicates whether the file system supports writing to the contents of the URL.
--
-- ObjC selector: @- initWithURL:writable:@
initWithURL_writable :: (IsFSPathURLResource fsPathURLResource, IsNSURL url) => fsPathURLResource -> url -> Bool -> IO (Id FSPathURLResource)
initWithURL_writable fsPathURLResource url writable =
  sendOwnedMessage fsPathURLResource initWithURL_writableSelector (toNSURL url) writable

-- | @- init@
init_ :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO (Id FSPathURLResource)
init_ fsPathURLResource =
  sendOwnedMessage fsPathURLResource initSelector

-- | The URL represented by the resource.
--
-- ObjC selector: @- url@
url :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO (Id NSURL)
url fsPathURLResource =
  sendMessage fsPathURLResource urlSelector

-- | A Boolean value that indicates whether the file system supports writing to the contents of the path URL.
--
-- ObjC selector: @- writable@
writable :: IsFSPathURLResource fsPathURLResource => fsPathURLResource -> IO Bool
writable fsPathURLResource =
  sendMessage fsPathURLResource writableSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:writable:@
initWithURL_writableSelector :: Selector '[Id NSURL, Bool] (Id FSPathURLResource)
initWithURL_writableSelector = mkSelector "initWithURL:writable:"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id FSPathURLResource)
initSelector = mkSelector "init"

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @writable@
writableSelector :: Selector '[] Bool
writableSelector = mkSelector "writable"

