{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @DOMFileList@.
module ObjC.WebKit.DOMFileList
  ( DOMFileList
  , IsDOMFileList(..)
  , item
  , length_
  , itemSelector
  , lengthSelector


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

import ObjC.WebKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- item:@
item :: IsDOMFileList domFileList => domFileList -> CUInt -> IO (Id DOMFile)
item domFileList  index =
  sendMsg domFileList (mkSelector "item:") (retPtr retVoid) [argCUInt (fromIntegral index)] >>= retainedObject . castPtr

-- | @- length@
length_ :: IsDOMFileList domFileList => domFileList -> IO CUInt
length_ domFileList  =
  sendMsg domFileList (mkSelector "length") retCUInt []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @item:@
itemSelector :: Selector
itemSelector = mkSelector "item:"

-- | @Selector@ for @length@
lengthSelector :: Selector
lengthSelector = mkSelector "length"

