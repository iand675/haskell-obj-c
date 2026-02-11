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

-- | @- initWithBundle:@
initWithBundle :: (IsMDLBundleAssetResolver mdlBundleAssetResolver, IsNSString path) => mdlBundleAssetResolver -> path -> IO (Id MDLBundleAssetResolver)
initWithBundle mdlBundleAssetResolver  path =
withObjCPtr path $ \raw_path ->
    sendMsg mdlBundleAssetResolver (mkSelector "initWithBundle:") (retPtr retVoid) [argPtr (castPtr raw_path :: Ptr ())] >>= ownedObject . castPtr

-- | @- path@
path :: IsMDLBundleAssetResolver mdlBundleAssetResolver => mdlBundleAssetResolver -> IO (Id NSString)
path mdlBundleAssetResolver  =
  sendMsg mdlBundleAssetResolver (mkSelector "path") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | @- setPath:@
setPath :: (IsMDLBundleAssetResolver mdlBundleAssetResolver, IsNSString value) => mdlBundleAssetResolver -> value -> IO ()
setPath mdlBundleAssetResolver  value =
withObjCPtr value $ \raw_value ->
    sendMsg mdlBundleAssetResolver (mkSelector "setPath:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @initWithBundle:@
initWithBundleSelector :: Selector
initWithBundleSelector = mkSelector "initWithBundle:"

-- | @Selector@ for @path@
pathSelector :: Selector
pathSelector = mkSelector "path"

-- | @Selector@ for @setPath:@
setPathSelector :: Selector
setPathSelector = mkSelector "setPath:"

