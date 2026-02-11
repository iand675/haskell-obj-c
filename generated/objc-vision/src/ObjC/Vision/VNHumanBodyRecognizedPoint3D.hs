{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Generated bindings for @VNHumanBodyRecognizedPoint3D@.
module ObjC.Vision.VNHumanBodyRecognizedPoint3D
  ( VNHumanBodyRecognizedPoint3D
  , IsVNHumanBodyRecognizedPoint3D(..)
  , new
  , init_
  , parentJoint
  , newSelector
  , initSelector
  , parentJointSelector


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

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNHumanBodyRecognizedPoint3D)
new  =
  do
    cls' <- getRequiredClass "VNHumanBodyRecognizedPoint3D"
    sendClassMsg cls' (mkSelector "new") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- init@
init_ :: IsVNHumanBodyRecognizedPoint3D vnHumanBodyRecognizedPoint3D => vnHumanBodyRecognizedPoint3D -> IO (Id VNHumanBodyRecognizedPoint3D)
init_ vnHumanBodyRecognizedPoint3D  =
  sendMsg vnHumanBodyRecognizedPoint3D (mkSelector "init") (retPtr retVoid) [] >>= ownedObject . castPtr

-- | @- parentJoint@
parentJoint :: IsVNHumanBodyRecognizedPoint3D vnHumanBodyRecognizedPoint3D => vnHumanBodyRecognizedPoint3D -> IO (Id NSString)
parentJoint vnHumanBodyRecognizedPoint3D  =
  sendMsg vnHumanBodyRecognizedPoint3D (mkSelector "parentJoint") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector
initSelector = mkSelector "init"

-- | @Selector@ for @parentJoint@
parentJointSelector :: Selector
parentJointSelector = mkSelector "parentJoint"

