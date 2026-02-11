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
  , urlSelector
  , setURLSelector


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

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithURL:name:@
initWithURL_name :: (IsMDLURLTexture mdlurlTexture, IsNSURL url, IsNSString name) => mdlurlTexture -> url -> name -> IO (Id MDLURLTexture)
initWithURL_name mdlurlTexture  url name =
withObjCPtr url $ \raw_url ->
  withObjCPtr name $ \raw_name ->
      sendMsg mdlurlTexture (mkSelector "initWithURL:name:") (retPtr retVoid) [argPtr (castPtr raw_url :: Ptr ()), argPtr (castPtr raw_name :: Ptr ())] >>= ownedObject . castPtr

-- | @- URL@
url :: IsMDLURLTexture mdlurlTexture => mdlurlTexture -> IO (Id NSURL)
url mdlurlTexture  =
  sendMsg mdlurlTexture (mkSelector "URL") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setURL:@
setURL :: (IsMDLURLTexture mdlurlTexture, IsNSURL value) => mdlurlTexture -> value -> IO ()
setURL mdlurlTexture  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlurlTexture (mkSelector "setURL:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithURL:name:@
initWithURL_nameSelector :: Selector
initWithURL_nameSelector = mkSelector "initWithURL:name:"

-- | @Selector@ for @URL@
urlSelector :: Selector
urlSelector = mkSelector "URL"

-- | @Selector@ for @setURL:@
setURLSelector :: Selector
setURLSelector = mkSelector "setURL:"

