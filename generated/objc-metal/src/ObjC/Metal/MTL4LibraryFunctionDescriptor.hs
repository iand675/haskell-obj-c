{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Describes a shader function from a Metal library.
--
-- Generated bindings for @MTL4LibraryFunctionDescriptor@.
module ObjC.Metal.MTL4LibraryFunctionDescriptor
  ( MTL4LibraryFunctionDescriptor
  , IsMTL4LibraryFunctionDescriptor(..)
  , name
  , setName
  , nameSelector
  , setNameSelector


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

-- | Assigns a name to the function.
--
-- ObjC selector: @- name@
name :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> IO (Id NSString)
name mtL4LibraryFunctionDescriptor  =
  sendMsg mtL4LibraryFunctionDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns a name to the function.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor, IsNSString value) => mtL4LibraryFunctionDescriptor -> value -> IO ()
setName mtL4LibraryFunctionDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4LibraryFunctionDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

