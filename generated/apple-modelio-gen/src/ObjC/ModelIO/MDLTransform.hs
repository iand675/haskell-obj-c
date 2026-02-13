{-# LANGUAGE DataKinds #-}
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
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
init_ mdlTransform =
  sendOwnedMessage mdlTransform initSelector

-- | @- initWithIdentity@
initWithIdentity :: IsMDLTransform mdlTransform => mdlTransform -> IO (Id MDLTransform)
initWithIdentity mdlTransform =
  sendOwnedMessage mdlTransform initWithIdentitySelector

-- | @- initWithTransformComponent:@
initWithTransformComponent :: IsMDLTransform mdlTransform => mdlTransform -> RawId -> IO (Id MDLTransform)
initWithTransformComponent mdlTransform component =
  sendOwnedMessage mdlTransform initWithTransformComponentSelector component

-- | @- initWithTransformComponent:resetsTransform:@
initWithTransformComponent_resetsTransform :: IsMDLTransform mdlTransform => mdlTransform -> RawId -> Bool -> IO (Id MDLTransform)
initWithTransformComponent_resetsTransform mdlTransform component resetsTransform =
  sendOwnedMessage mdlTransform initWithTransformComponent_resetsTransformSelector component resetsTransform

-- | Set all transform components to identity
--
-- ObjC selector: @- setIdentity@
setIdentity :: IsMDLTransform mdlTransform => mdlTransform -> IO ()
setIdentity mdlTransform =
  sendMessage mdlTransform setIdentitySelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id MDLTransform)
initSelector = mkSelector "init"

-- | @Selector@ for @initWithIdentity@
initWithIdentitySelector :: Selector '[] (Id MDLTransform)
initWithIdentitySelector = mkSelector "initWithIdentity"

-- | @Selector@ for @initWithTransformComponent:@
initWithTransformComponentSelector :: Selector '[RawId] (Id MDLTransform)
initWithTransformComponentSelector = mkSelector "initWithTransformComponent:"

-- | @Selector@ for @initWithTransformComponent:resetsTransform:@
initWithTransformComponent_resetsTransformSelector :: Selector '[RawId, Bool] (Id MDLTransform)
initWithTransformComponent_resetsTransformSelector = mkSelector "initWithTransformComponent:resetsTransform:"

-- | @Selector@ for @setIdentity@
setIdentitySelector :: Selector '[] ()
setIdentitySelector = mkSelector "setIdentity"

