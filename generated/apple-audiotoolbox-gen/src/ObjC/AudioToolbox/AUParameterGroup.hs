{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | AUParameterGroup
--
-- A group of related parameters.
--
-- A parameter group is KVC-compliant for its children; e.g. valueForKey:"volume" will		return a child parameter whose identifier is "volume".
--
-- Generated bindings for @AUParameterGroup@.
module ObjC.AudioToolbox.AUParameterGroup
  ( AUParameterGroup
  , IsAUParameterGroup(..)
  , children
  , allParameters
  , allParametersSelector
  , childrenSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The group's child nodes (AUParameterGroupNode).
--
-- ObjC selector: @- children@
children :: IsAUParameterGroup auParameterGroup => auParameterGroup -> IO (Id NSArray)
children auParameterGroup =
  sendMessage auParameterGroup childrenSelector

-- | Returns a flat array of all parameters in the group, including those in child groups.
--
-- ObjC selector: @- allParameters@
allParameters :: IsAUParameterGroup auParameterGroup => auParameterGroup -> IO (Id NSArray)
allParameters auParameterGroup =
  sendMessage auParameterGroup allParametersSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @children@
childrenSelector :: Selector '[] (Id NSArray)
childrenSelector = mkSelector "children"

-- | @Selector@ for @allParameters@
allParametersSelector :: Selector '[] (Id NSArray)
allParametersSelector = mkSelector "allParameters"

