{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.ModelIO.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- initWithPath:@
initWithPath :: (IsMDLPathAssetResolver mdlPathAssetResolver, IsNSString path) => mdlPathAssetResolver -> path -> IO (Id MDLPathAssetResolver)
initWithPath mdlPathAssetResolver path =
  sendOwnedMessage mdlPathAssetResolver initWithPathSelector (toNSString path)

-- | @- path@
path :: IsMDLPathAssetResolver mdlPathAssetResolver => mdlPathAssetResolver -> IO (Id NSString)
path mdlPathAssetResolver =
  sendMessage mdlPathAssetResolver pathSelector

-- | @- setPath:@
setPath :: (IsMDLPathAssetResolver mdlPathAssetResolver, IsNSString value) => mdlPathAssetResolver -> value -> IO ()
setPath mdlPathAssetResolver value =
  sendMessage mdlPathAssetResolver setPathSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithPath:@
initWithPathSelector :: Selector '[Id NSString] (Id MDLPathAssetResolver)
initWithPathSelector = mkSelector "initWithPath:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSString] ()
setPathSelector = mkSelector "setPath:"

