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
  , sourceSelector
  , setSourceSelector
  , optionsSelector
  , setOptionsSelector
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

-- | Assigns an optional string containing the source code of the shader language program to compile into a Metal library.
--
-- ObjC selector: @- source@
source :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id NSString)
source mtL4LibraryDescriptor  =
  sendMsg mtL4LibraryDescriptor (mkSelector "source") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional string containing the source code of the shader language program to compile into a Metal library.
--
-- ObjC selector: @- setSource:@
setSource :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsNSString value) => mtL4LibraryDescriptor -> value -> IO ()
setSource mtL4LibraryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4LibraryDescriptor (mkSelector "setSource:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Provides compile-time options for the Metal library.
--
-- ObjC selector: @- options@
options :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id MTLCompileOptions)
options mtL4LibraryDescriptor  =
  sendMsg mtL4LibraryDescriptor (mkSelector "options") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Provides compile-time options for the Metal library.
--
-- ObjC selector: @- setOptions:@
setOptions :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsMTLCompileOptions value) => mtL4LibraryDescriptor -> value -> IO ()
setOptions mtL4LibraryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4LibraryDescriptor (mkSelector "setOptions:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- | Assigns an optional name to the Metal library.
--
-- ObjC selector: @- name@
name :: IsMTL4LibraryDescriptor mtL4LibraryDescriptor => mtL4LibraryDescriptor -> IO (Id NSString)
name mtL4LibraryDescriptor  =
  sendMsg mtL4LibraryDescriptor (mkSelector "name") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Assigns an optional name to the Metal library.
--
-- ObjC selector: @- setName:@
setName :: (IsMTL4LibraryDescriptor mtL4LibraryDescriptor, IsNSString value) => mtL4LibraryDescriptor -> value -> IO ()
setName mtL4LibraryDescriptor  value =
withObjCPtr value $ \raw_value ->
    sendMsg mtL4LibraryDescriptor (mkSelector "setName:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @source@
sourceSelector :: Selector
sourceSelector = mkSelector "source"

-- | @Selector@ for @setSource:@
setSourceSelector :: Selector
setSourceSelector = mkSelector "setSource:"

-- | @Selector@ for @options@
optionsSelector :: Selector
optionsSelector = mkSelector "options"

-- | @Selector@ for @setOptions:@
setOptionsSelector :: Selector
setOptionsSelector = mkSelector "setOptions:"

-- | @Selector@ for @name@
nameSelector :: Selector
nameSelector = mkSelector "name"

-- | @Selector@ for @setName:@
setNameSelector :: Selector
setNameSelector = mkSelector "setName:"

