{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Concrete implementation of <MDLTransformComponent>. For more complex transform components create a class that conforms to  <MDLTransformComponent>.
--
-- Setting any of scale, translation, or rotation individually will  set the matrix property, and clear any timing information.
--
-- Generated bindings for @MDLTransform@.
module ObjC.ModelIO.MDLTransform
  ( MDLTransform
  , IsMDLTransform(..)
  , init_
  , initWithIdentity
  , initWithTransformComponent
  , initWithTransformComponent_resetsTransform
  , setIdentity
  , initSelector
  , initWithIdentitySelector
  , initWithTransformComponentSelector
  , initWithTransformComponent_resetsTransformSelector
  , setIdentitySelector


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

-- | init
--
-- Initialize an MDLTransform's matrices with identity
--
-- ObjC selector: @- init@
init_ :: IsMDLTransform mdlTransform => mdlTransform -> IO (Id MDLTransform)
init_ mdlTransform  =
  sendMsg mdlTransform (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithIdentity@
initWithIdentity :: IsMDLTransform mdlTransform => mdlTransform -> IO (Id MDLTransform)
initWithIdentity mdlTransform  =
  sendMsg mdlTransform (mkSelector "initWithIdentity") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- initWithTransformComponent:@
initWithTransformComponent :: IsMDLTransform mdlTransform => mdlTransform -> RawId -> IO (Id MDLTransform)
initWithTransformComponent mdlTransform  component =
  sendMsg mdlTransform (mkSelector "initWithTransformComponent:") (retPtr retVoid) [argPtr (castPtr (unRawId component) :: Ptr ())] >>= ownedObject . castPtr

-- | @- initWithTransformComponent:resetsTransform:@
initWithTransformComponent_resetsTransform :: IsMDLTransform mdlTransform => mdlTransform -> RawId -> Bool -> IO (Id MDLTransform)
initWithTransformComponent_resetsTransform mdlTransform  component resetsTransform =
  sendMsg mdlTransform (mkSelector "initWithTransformComponent:resetsTransform:") (retPtr retVoid) [argPtr (castPtr (unRawId component) :: Ptr ()), argCULong (if resetsTransform then 1 else 0)] >>= ownedObject . castPtr

-- | Set all transform components to identity
--
-- ObjC selector: @- setIdentity@
setIdentity :: IsMDLTransform mdlTransform => mdlTransform -> IO ()
setIdentity mdlTransform  =
  sendMsg mdlTransform (mkSelector "setIdentity") retVoid []

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentity@
initWithIdentitySelector :: Selector
initWithIdentitySelector = mkSelector "initWithIdentity"

-- | @Selector@ for @initWithTransformComponent:@
initWithTransformComponentSelector :: Selector
initWithTransformComponentSelector = mkSelector "initWithTransformComponent:"

-- | @Selector@ for @initWithTransformComponent:resetsTransform:@
initWithTransformComponent_resetsTransformSelector :: Selector
initWithTransformComponent_resetsTransformSelector = mkSelector "initWithTransformComponent:resetsTransform:"

-- | @Selector@ for @setIdentity@
setIdentitySelector :: Selector
setIdentitySelector = mkSelector "setIdentity"

