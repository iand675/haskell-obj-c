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
jsonRepresentation mxCallStackTree  =
  sendMsg mxCallStackTree (mkSelector "JSONRepresentation") (retPtr retVoid) [] >>= retainedObject . castPtr

-- ---------------------------------------------------------------------------
-- Selectors
-- ---------------------------------------------------------------------------

-- | @Selector@ for @JSONRepresentation@
jsonRepresentationSelector :: Selector
jsonRepresentationSelector = mkSelector "JSONRepresentation"

