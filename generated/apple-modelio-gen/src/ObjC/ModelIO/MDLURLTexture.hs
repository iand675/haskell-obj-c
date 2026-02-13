{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLURLTexture  a texture provider initialized with a URL or file path.
--
-- if any of the properties of the texture, such as data, are referenced,             then the texture may be loaded, otherwise, the MDLURLTexture is merely             a lightweight reference to something that could be loaded
--
-- Generated bindings for @MDLURLTexture@.
module ObjC.ModelIO.MDLURLTexture
  ( MDLURLTexture
  , IsMDLURLTexture(..)
  , initWithURL_name
  , url
  , setURL
  , initWithURL_nameSelector
  , setURLSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithURL:name:@
initWithURL_name :: (IsMDLURLTexture mdlurlTexture, IsNSURL url, IsNSString name) => mdlurlTexture -> url -> name -> IO (Id MDLURLTexture)
initWithURL_name mdlurlTexture url name =
  sendOwnedMessage mdlurlTexture initWithURL_nameSelector (toNSURL url) (toNSString name)

-- | @- URL@
url :: IsMDLURLTexture mdlurlTexture => mdlurlTexture -> IO (Id NSURL)
url mdlurlTexture =
  sendMessage mdlurlTexture urlSelector

-- | @- setURL:@
setURL :: (IsMDLURLTexture mdlurlTexture, IsNSURL value) => mdlurlTexture -> value -> IO ()
setURL mdlurlTexture value =
  sendMessage mdlurlTexture setURLSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:name:@
initWithURL_nameSelector :: Selector '[Id NSURL, Id NSString] (Id MDLURLTexture)
initWithURL_nameSelector = mkSelector "initWithURL:name:"

-- | @Selector@ for @URL@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector '[Id NSURL] ()
setURLSelector = mkSelector "setURL:"

