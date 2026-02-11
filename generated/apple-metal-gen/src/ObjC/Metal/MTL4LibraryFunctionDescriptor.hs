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
  , library
  , setLibrary
  , nameSelector
  , setNameSelector
  , librarySelector
  , setLibrarySelector


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

-- | Returns a reference to the library containing the function.
--
-- ObjC selector: @- library@
library :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> IO RawId
library mtL4LibraryFunctionDescriptor  =
    fmap (RawId . castPtr) $ sendMsg mtL4LibraryFunctionDescriptor (mkSelector "library") (retPtr retVoid) []

-- | Returns a reference to the library containing the function.
--
-- ObjC selector: @- setLibrary:@
setLibrary :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> RawId -> IO ()
setLibrary mtL4LibraryFunctionDescriptor  value =
    sendMsg mtL4LibraryFunctionDescriptor (mkSelector "setLibrary:") retVoid [argPtr (castPtr (unRawId value) :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @library@
librarySelector :: Selector
librarySelector = mkSelector "library"

-- | @Selector@ for @setLibrary:@
setLibrarySelector :: Selector
setLibrarySelector = mkSelector "setLibrary:"

