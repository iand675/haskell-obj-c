{-# LANGUAGE DataKinds #-}
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
  , initSelector
  , newSelector
  , parentJointSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.Vision.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | @+ new@
new :: IO (Id VNHumanBodyRecognizedPoint3D)
new  =
  do
    cls' <- getRequiredClass "VNHumanBodyRecognizedPoint3D"
    sendOwnedClassMessage cls' newSelector

-- | @- init@
init_ :: IsVNHumanBodyRecognizedPoint3D vnHumanBodyRecognizedPoint3D => vnHumanBodyRecognizedPoint3D -> IO (Id VNHumanBodyRecognizedPoint3D)
init_ vnHumanBodyRecognizedPoint3D =
  sendOwnedMessage vnHumanBodyRecognizedPoint3D initSelector

-- | @- parentJoint@
parentJoint :: IsVNHumanBodyRecognizedPoint3D vnHumanBodyRecognizedPoint3D => vnHumanBodyRecognizedPoint3D -> IO (Id NSString)
parentJoint vnHumanBodyRecognizedPoint3D =
  sendMessage vnHumanBodyRecognizedPoint3D parentJointSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @new@
newSelector :: Selector '[] (Id VNHumanBodyRecognizedPoint3D)
newSelector = mkSelector "new"

-- | @Selector@ for @init@
initSelector :: Selector '[] (Id VNHumanBodyRecognizedPoint3D)
initSelector = mkSelector "init"

-- | @Selector@ for @parentJoint@
parentJointSelector :: Selector '[] (Id NSString)
parentJointSelector = mkSelector "parentJoint"

