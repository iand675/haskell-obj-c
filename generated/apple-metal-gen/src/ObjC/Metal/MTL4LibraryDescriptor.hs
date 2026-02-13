{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Serves as the base descriptor for creating a Metal library.
--
-- Generated bindings for @MTL4LibraryDescriptor@.
module ObjC.Metal.MTL4LibraryDescriptor
  ( MTL4LibraryDescriptor
  , IsMTL4LibraryDescriptor(..)
  , source
  , setSource
  , options
  , setOptions
  , name
  , setName
  , nameSelector
  , optionsSelector
  , setNameSelector
  , setOptionsSelector
  , setSourceSelector
  , sourceSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Metal.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | Assigns an optional string containing the source code of the shader language program to compile into a Metal library.
--
-- ObjC selector: @- source@
source :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id NSString)
source mtL4LibraryDescriptor =
  sendMessage mtL4LibraryDescriptor sourceSelector

-- | Assigns an optional string containing the source code of the shader language program to compile into a Metal library.
--
-- ObjC selector: @- setSource:@
setSource :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsNSString value) => mtL4LibraryDescriptor -> value -> IO ()
setSource mtL4LibraryDescriptor value =
  sendMessage mtL4LibraryDescriptor setSourceSelector (toNSString value)

-- | Provides compile-time options for the Metal library.
--
-- ObjC selector: @- options@
options :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id MTLCompileOptions)
options mtL4LibraryDescriptor =
  sendMessage mtL4LibraryDescriptor optionsSelector

-- | Provides compile-time options for the Metal library.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsMTLCompileOptions value) => mtL4LibraryDescriptor -> value -> IO ()
setOptions mtL4LibraryDescriptor value =
  sendMessage mtL4LibraryDescriptor setOptionsSelector (toMTLCompileOptions value)

-- | Assigns an optional name to the Metal library.
--
-- ObjC selector: @- name@
name :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id NSString)
name mtL4LibraryDescriptor =
  sendMessage mtL4LibraryDescriptor nameSelector

-- | Assigns an optional name to the Metal library.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsNSString value) => mtL4LibraryDescriptor -> value -> IO ()
setName mtL4LibraryDescriptor value =
  sendMessage mtL4LibraryDescriptor setNameSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector '[] (Id NSString)
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector '[Id NSString] ()
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @options@
optionsSelector :: Selector '[] (Id MTLCompileOptions)
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector '[Id MTLCompileOptions] ()
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @name@
nameSelector :: Selector '[] (Id NSString)
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector '[Id NSString] ()
setNameSelector = mkSelector "setName:"

