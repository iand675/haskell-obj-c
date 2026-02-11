{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | The configuration options that control the behavior of a compilation task for a Metal 4 compiler instance.
--
-- You can configure task-specific settings that affect a compilation task by creating an instance of this class, setting its properties, and passing it to one of the applicable methods of an ``MTL4Compiler`` instance.
--
-- Generated bindings for @MTL4CompilerTaskOptions@.
module ObjC.Metal.MTL4CompilerTaskOptions
  ( MTL4CompilerTaskOptions
  , IsMTL4CompilerTaskOptions(..)
  , lookupArchives
  , setLookupArchives
  , lookupArchivesSelector
  , setLookupArchivesSelector


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

-- | An array of archive instances that can potentially accelerate a compilation task.
--
-- The compiler can reduce the runtime of a compilation task if it finds an entry that matches a function description within any of the archives in this array. The compiler searches the archives in the order of the array's element.
--
-- Consider adding archives to the array in scenarios that can benefit from the runtime savings, such as repeat builds or when your app can share compilation results across multiple contexts.
--
-- - Important: Only add ``MTL4Archive`` instances to the array that are compatible with the Metal device.
--
-- ObjC selector: @- lookupArchives@
lookupArchives :: IsMTL4CompilerTaskOptions mtL4CompilerTaskOptions => mtL4CompilerTaskOptions -> IO (Id NSArray)
lookupArchives mtL4CompilerTaskOptions  =
    sendMsg mtL4CompilerTaskOptions (mkSelector "lookupArchives") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | An array of archive instances that can potentially accelerate a compilation task.
--
-- The compiler can reduce the runtime of a compilation task if it finds an entry that matches a function description within any of the archives in this array. The compiler searches the archives in the order of the array's element.
--
-- Consider adding archives to the array in scenarios that can benefit from the runtime savings, such as repeat builds or when your app can share compilation results across multiple contexts.
--
-- - Important: Only add ``MTL4Archive`` instances to the array that are compatible with the Metal device.
--
-- ObjC selector: @- setLookupArchives:@
setLookupArchives :: (IsMTL4CompilerTaskOptions mtL4CompilerTaskOptions, IsNSArray value) => mtL4CompilerTaskOptions -> value -> IO ()
setLookupArchives mtL4CompilerTaskOptions  value =
  withObjCPtr value $ \raw_value ->
      sendMsg mtL4CompilerTaskOptions (mkSelector "setLookupArchives:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @lookupArchives@
lookupArchivesSelector :: Selector
lookupArchivesSelector = mkSelector "lookupArchives"

-- | @Selector@ for @setLookupArchives:@
setLookupArchivesSelector :: Selector
setLookupArchivesSelector = mkSelector "setLookupArchives:"

