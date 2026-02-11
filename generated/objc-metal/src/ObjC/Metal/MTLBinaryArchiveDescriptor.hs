{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MTLBinaryArchiveDescriptor
--
-- A class used to indicate how an archive should be created
--
-- Generated bindings for @MTLBinaryArchiveDescriptor@.
module ObjC.Metal.MTLBinaryArchiveDescriptor
  ( MTLBinaryArchiveDescriptor
  , IsMTLBinaryArchiveDescriptor(..)
  , url
  , setUrl
  , urlSelector
  , setUrlSelector


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

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | url
--
-- The file URL from which to open a MTLBinaryArchive, or nil to create an empty MTLBinaryArchive.
--
-- ObjC selector: @- url@
url :: IsMTLBinaryArchiveDescriptor mtlBinaryArchiveDescriptor => mtlBinaryArchiveDescriptor -> IO (Id NSURL)
url mtlBinaryArchiveDescriptor  =
  sendMsg mtlBinaryArchiveDescriptor (mkSelector "url") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | url
--
-- The file URL from which to open a MTLBinaryArchive, or nil to create an empty MTLBinaryArchive.
--
-- ObjC selector: @- setUrl:@
setUrl :: (IsMTLBinaryArchiveDescriptor mtlBinaryArchiveDescriptor, IsNSURL value) => mtlBinaryArchiveDescriptor -> value -> IO ()
setUrl mtlBinaryArchiveDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtlBinaryArchiveDescriptor (mkSelector "setUrl:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @url@
urlSelector :: Selector
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector
setUrlSelector = mkSelector "setUrl:"

