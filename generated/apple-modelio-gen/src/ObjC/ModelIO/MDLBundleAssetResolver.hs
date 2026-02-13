{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @MDLBundleAssetResolver@.
module ObjC.ModelIO.MDLBundleAssetResolver
  ( MDLBundleAssetResolver
  , IsMDLBundleAssetResolver(..)
  , initWithBundle
  , path
  , setPath
  , initWithBundleSelector
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

-- | @- initWithBundle:@
initWithBundle :: (IsMDLBundleAssetResolver mdlBundleAssetResolver, IsNSString path) => mdlBundleAssetResolver -> path -> IO (Id MDLBundleAssetResolver)
initWithBundle mdlBundleAssetResolver path =
  sendOwnedMessage mdlBundleAssetResolver initWithBundleSelector (toNSString path)

-- | @- path@
path :: IsMDLBundleAssetResolver mdlBundleAssetResolver => mdlBundleAssetResolver -> IO (Id NSString)
path mdlBundleAssetResolver =
  sendMessage mdlBundleAssetResolver pathSelector

-- | @- setPath:@
setPath :: (IsMDLBundleAssetResolver mdlBundleAssetResolver, IsNSString value) => mdlBundleAssetResolver -> value -> IO ()
setPath mdlBundleAssetResolver value =
  sendMessage mdlBundleAssetResolver setPathSelector (toNSString value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundle:@
initWithBundleSelector :: Selector '[Id NSString] (Id MDLBundleAssetResolver)
initWithBundleSelector = mkSelector "initWithBundle:"

-- | @Selector@ for @path@
pathSelector :: Selector '[] (Id NSString)
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector '[Id NSString] ()
setPathSelector = mkSelector "setPath:"

