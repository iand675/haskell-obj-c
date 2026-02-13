{-# LANGUAGE DataKinds #-}
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
  , setUrlSelector
  , urlSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
url mtlBinaryArchiveDescriptor =
  sendMessage mtlBinaryArchiveDescriptor urlSelector

-- | url
--
-- The file URL from which to open a MTLBinaryArchive, or nil to create an empty MTLBinaryArchive.
--
-- ObjC selector: @- setUrl:@
setUrl :: (IsMTLBinaryArchiveDescriptor mtlBinaryArchiveDescriptor, IsNSURL value) => mtlBinaryArchiveDescriptor -> value -> IO ()
setUrl mtlBinaryArchiveDescriptor value =
  sendMessage mtlBinaryArchiveDescriptor setUrlSelector (toNSURL value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @url@
urlSelector :: Selector '[] (Id NSURL)
urlSelector = mkSelector "url"

-- | @Selector@ for @setUrl:@
setUrlSelector :: Selector '[Id NSURL] ()
setUrlSelector = mkSelector "setUrl:"

