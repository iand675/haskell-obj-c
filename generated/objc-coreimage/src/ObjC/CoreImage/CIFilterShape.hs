{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @CIFilterShape@.
module ObjC.CoreImage.CIFilterShape
  ( CIFilterShape
  , IsCIFilterShape(..)
  , insetByX_Y
  , unionWith
  , intersectWith
  , insetByX_YSelector
  , unionWithSelector
  , intersectWithSelector


  ) where

import Foreign.Ptr (Ptr, nullPtr, castPtr)
import Foreign.LibFFI
import Foreign.C.Types
import Data.Int (Int8, Int16)
import Data.Word (Word16)
import Data.Coerce (coerce)

import ObjC.Runtime.Types
import ObjC.Runtime.MsgSend (sendMsg, sendClassMsg, sendMsgStret, sendClassMsgStret)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.CoreImage.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- insetByX:Y:@
insetByX_Y :: IsCIFilterShape ciFilterShape => ciFilterShape -> CInt -> CInt -> IO (Id CIFilterShape)
insetByX_Y ciFilterShape  dx dy =
  sendMsg ciFilterShape (mkSelector "insetByX:Y:") (retPtr retVoid) [argCInt (fromIntegral dx), argCInt (fromIntegral dy)] >>= retainedObject . castPtr

-- | @- unionWith:@
unionWith :: (IsCIFilterShape ciFilterShape, IsCIFilterShape s2) => ciFilterShape -> s2 -> IO (Id CIFilterShape)
unionWith ciFilterShape  s2 =
withObjCPtr s2 $ \raw_s2 ->
    sendMsg ciFilterShape (mkSelector "unionWith:") (retPtr retVoid) [argPtr (castPtr raw_s2 :: Ptr ())] >>= retainedObject . castPtr

-- | @- intersectWith:@
intersectWith :: (IsCIFilterShape ciFilterShape, IsCIFilterShape s2) => ciFilterShape -> s2 -> IO (Id CIFilterShape)
intersectWith ciFilterShape  s2 =
withObjCPtr s2 $ \raw_s2 ->
    sendMsg ciFilterShape (mkSelector "intersectWith:") (retPtr retVoid) [argPtr (castPtr raw_s2 :: Ptr ())] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @insetByX:Y:@
insetByX_YSelector :: Selector
insetByX_YSelector = mkSelector "insetByX:Y:"

-- | @Selector@ for @unionWith:@
unionWithSelector :: Selector
unionWithSelector = mkSelector "unionWith:"

-- | @Selector@ for @intersectWith:@
intersectWithSelector :: Selector
intersectWithSelector = mkSelector "intersectWith:"

