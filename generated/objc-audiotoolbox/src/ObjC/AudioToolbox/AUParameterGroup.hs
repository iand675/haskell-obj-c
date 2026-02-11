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
  , childrenSelector
  , allParametersSelector


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

import ObjC.AudioToolbox.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | The group's child nodes (AUParameterGroupNode).
--
-- ObjC selector: @- children@
children :: IsAUParameterGroup auParameterGroup => auParameterGroup -> IO (Id NSArray)
children auParameterGroup  =
  sendMsg auParameterGroup (mkSelector "children") (retPtr retVoid) [] >>= retainedObject . castPtr

-- | Returns a flat array of all parameters in the group, including those in child groups.
--
-- ObjC selector: @- allParameters@
allParameters :: IsAUParameterGroup auParameterGroup => auParameterGroup -> IO (Id NSArray)
allParameters auParameterGroup  =
  sendMsg auParameterGroup (mkSelector "allParameters") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @children@
childrenSelector :: Selector
childrenSelector = mkSelector "children"

-- | @Selector@ for @allParameters@
allParametersSelector :: Selector
allParametersSelector = mkSelector "allParameters"

