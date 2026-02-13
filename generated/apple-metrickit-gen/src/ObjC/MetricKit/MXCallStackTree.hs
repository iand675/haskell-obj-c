{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

-- | MXCallStackTree
--
-- A data class that encapsulates call stack trees vended by MetricKit.
--
-- You should use the JSONRepresentation API to generate human readable call stack trees for symbolication off device.
--
-- Generated bindings for @MXCallStackTree@.
module ObjC.MetricKit.MXCallStackTree
  ( MXCallStackTree
  , IsMXCallStackTree(..)
  , jsonRepresentation
  , jsonRepresentationSelector


  ) where

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.Types

import ObjC.Runtime.Types
import ObjC.Runtime.Message (sendMessage, sendOwnedMessage, sendClassMessage, sendOwnedClassMessage)
import ObjC.Runtime.Selector (mkSelector)
import ObjC.Runtime.Class (getRequiredClass)

import ObjC.MetricKit.Internal.Classes
import ObjC.Foundation.Internal.Classes

-- | JSONRepresentation
--
-- Convenience method to return a JSON representation of this callstack tree.
--
-- The JSON structure of MXCallStackTree is organized into individual groups of call stacks. Individual call stacks contain stack frames, which consist of information needed to symbolicate the frame off device. This includes binary image name, binary UUID, offset in binary text segment, address, and sample count (for stack trees that contain temporally sampled data.)
--
-- MXCallStackTrees can be organized into a single callstack for the entire application, or broken up into callstacks associated with individual threads.
--
-- Returns: An NSData object containing the JSON representation
--
-- ObjC selector: @- JSONRepresentation@
jsonRepresentation :: IsMXCallStackTree mxCallStackTree => mxCallStackTree -> IO (Id NSData)
jsonRepresentation mxCallStackTree =
  sendMessage mxCallStackTree jsonRepresentationSelector

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector '[] (Id NSData)
jsonRepresentationSelector = mkSelector "JSONRepresentation"

