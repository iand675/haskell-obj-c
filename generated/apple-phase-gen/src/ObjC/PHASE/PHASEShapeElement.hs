{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | *************************************************************************************************
--
-- PHASEShapeElement
--
-- A single element within a shape. The attached material defines its acoustical properties.
--
-- Generated bindings for @PHASEShapeElement@.
module ObjC.PHASE.PHASEShapeElement
  ( PHASEShapeElement
  , IsPHASEShapeElement(..)
  , init_
  , new
  , material
  , setMaterial
  , initSelector
  , materialSelector
  , newSelector
  , setMaterialSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEShapeElement phaseShapeElement => phaseShapeElement -> IO (Id PHASEShapeElement)
init_ phaseShapeElement =
  sendOwnedMessage phaseShapeElement initSelector

-- | @+ new@
new :: IO (Id PHASEShapeElement)
new  =
  do
    cls' <- getRequiredClass "PHASEShapeElement"
    sendOwnedClassMessage cls' newSelector

-- | material
--
-- The shape's material defines the acoustical properties of this element.
--
-- ObjC selector: @- material@
material :: IsPHASEShapeElement phaseShapeElement => phaseShapeElement -> IO (Id PHASEMaterial)
material phaseShapeElement =
  sendMessage phaseShapeElement materialSelector

-- | material
--
-- The shape's material defines the acoustical properties of this element.
--
-- ObjC selector: @- setMaterial:@
setMaterial :: (IsPHASEShapeElement phaseShapeElement, IsPHASEMaterial value) => phaseShapeElement -> value -> IO ()
setMaterial phaseShapeElement value =
  sendMessage phaseShapeElement setMaterialSelector (toPHASEMaterial value)

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id PHASEShapeElement)
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id PHASEShapeElement)
newSelector = mkSelector "new"

-- | @Selector@ for @material@
materialSelector :: Selector '[] (Id PHASEMaterial)
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector '[Id PHASEMaterial] ()
setMaterialSelector = mkSelector "setMaterial:"

