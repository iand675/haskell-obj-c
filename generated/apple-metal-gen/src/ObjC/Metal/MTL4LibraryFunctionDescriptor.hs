{-# LANGUAGE DataKinds #-}
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
  , librarySelector
  , nameSelector
  , setLibrarySelector
  , setNameSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Assigns a name to the function.
--
-- ObjC selector: @- name@
name :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> IO (Id NSString)
name mtL4LibraryFunctionDescriptor =
  sendMessage mtL4LibraryFunctionDescriptor nameSelector

-- | Assigns a name to the function.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor, IsNSString value) => mtL4LibraryFunctionDescriptor -> value -> IO ()
setName mtL4LibraryFunctionDescriptor value =
  sendMessage mtL4LibraryFunctionDescriptor setNameSelector (toNSString value)

-- | Returns a reference to the library containing the function.
--
-- ObjC selector: @- library@
library :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> IO RawId
library mtL4LibraryFunctionDescriptor =
  sendMessage mtL4LibraryFunctionDescriptor librarySelector

-- | Returns a reference to the library containing the function.
--
-- ObjC selector: @- setLibrary:@
setLibrary :: IsMTL4LibraryFunctionDescriptor mtL4LibraryFunctionDescriptor => mtL4LibraryFunctionDescriptor -> RawId -> IO ()
setLibrary mtL4LibraryFunctionDescriptor value =
  sendMessage mtL4LibraryFunctionDescriptor setLibrarySelector value

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

-- | @Selector@ for @library@
librarySelector :: Selector '[] RawId
librarySelector = mkSelector "library"

-- | @Selector@ for @setLibrary:@
setLibrarySelector :: Selector '[RawId] ()
setLibrarySelector = mkSelector "setLibrary:"

