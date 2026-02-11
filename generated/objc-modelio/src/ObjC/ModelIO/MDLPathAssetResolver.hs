{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MDLPathAssetResolver
--
-- The path asset resolver searches for referenced files by prepending path.
--
-- Path should resolve to a well formed URI. A file system path might take the form "file:///path/to/all/assets/
--
-- A trailing slash is automatically appended to path if not provided.
--
-- Generated bindings for @MDLPathAssetResolver@.
module ObjC.ModelIO.MDLPathAssetResolver
  ( MDLPathAssetResolver
  , IsMDLPathAssetResolver(..)
  , initWithPath
  , path
  , setPath
  , initWithPathSelector
  , pathSelector
  , setPathSelector


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

-- | @- initWithPath:@
initWithPath :: (IsMDLPathAssetResolver mdlPathAssetResolver, IsNSString path) => mdlPathAssetResolver -> path -> IO (Id MDLPathAssetResolver)
initWithPath mdlPathAssetResolver  path =
withObjCPtr path $ \raw_path ->
    sendMsg mdlPathAssetResolver (mkSelector "initWithPath:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- path@
path :: IsMDLPathAssetResolver mdlPathAssetResolver => mdlPathAssetResolver -> IO (Id NSString)
path mdlPathAssetResolver  =
  sendMsg mdlPathAssetResolver (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPath:@
setPath :: (IsMDLPathAssetResolver mdlPathAssetResolver, IsNSString value) => mdlPathAssetResolver -> value -> IO ()
setPath mdlPathAssetResolver  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlPathAssetResolver (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

