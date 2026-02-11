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
  , newSelector
  , materialSelector
  , setMaterialSelector


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

import ObjC.PHASE.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @- init@
init_ :: IsPHASEShapeElement phaseShapeElement => phaseShapeElement -> IO (Id PHASEShapeElement)
init_ phaseShapeElement  =
  sendMsg phaseShapeElement (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @+ new@
new :: IO (Id PHASEShapeElement)
new  =
  do
    cls' <- getRequiredClass "PHASEShapeElement"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | material
--
-- The shape's material defines the acoustical properties of this element.
--
-- ObjC selector: @- material@
material :: IsPHASEShapeElement phaseShapeElement => phaseShapeElement -> IO (Id PHASEMaterial)
material phaseShapeElement  =
  sendMsg phaseShapeElement (mkSelector "material") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | material
--
-- The shape's material defines the acoustical properties of this element.
--
-- ObjC selector: @- setMaterial:@
setMaterial :: (IsPHASEShapeElement phaseShapeElement, IsPHASEMaterial value) => phaseShapeElement -> value -> IO ()
setMaterial phaseShapeElement  value =
withObjCPtr value $ \raw_value ->
    sendMsg phaseShapeElement (mkSelector "setMaterial:") retVoid [argPtr (castPtr raw_value :: Ptr ())]

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @material@
materialSelector :: Selector
materialSelector = mkSelector "material"

-- | @Selector@ for @setMaterial:@
setMaterialSelector :: Selector
setMaterialSelector = mkSelector "setMaterial:"

